#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode};
use frame_support::{decl_module, decl_storage, decl_event, decl_error, dispatch, traits::Get};
use frame_system::ensure_signed;
use sp_std::collections::BTreeMap;
use sp_runtime::{RuntimeDebug};
use orml_traits::{MultiReservableCurrency, MultiCurrency};
use orml_utilities::with_transaction_result;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature ="std")]
pub use serde::{Deserialize, Serialize};

type BalanceOf<T> = <<T as Trait>::Currency as MultiCurrency<<T as frame_system::Trait>::AccountId>>::Balance;
type CurrencyIdOf<T> = <<T as Trait>::Currency as MultiCurrency<<T as frame_system::Trait>::AccountId>>::CurrencyId;
type DexAccount<T> = <T as frame_system::Trait>::AccountId;

const MINIMUM_LIQUIDITY:u32 = 10_000;
const INITIAL_SHARES: u32 = 1_000;
const FEERATE: u32 = 3; 
const FEE_RATE_DENOMINATOR: u32 = 1_000;
pub const DEX_ACCOUNT_ID: DexAccount = 1;

pub trait Trait: frame_system::Trait {
	type Event: From<Event<Self>> + Into<<Self as frame_system::Trait>::Event>;
}

#[cfg_attr(feature="std", derive(Serialize, Deserialize))]
#[derive(Encode, Decode, Clone, PartialEq, Eq, Debug, RuntimeDebug)]
pub struct TokenPair<T: Trait> {
	fee_rate: BalanceOf<T>,
	token_a_pool: BalanceOf<T>,
	token_b_pool: BalanceOf<T>,
	invariant: BalanceOf<T>,
	total_shares:BalanceOf<T>,
	shares:BTreeMap<T::AccountId, BalanceOf<T>>,	
}

decl_storage! {
	trait Store for Module<T: Trait> as TemplateModule {
		// TO-DO: CurrencyIdOf<T>, CurrencyIdOf<T> use linkedlist
		pub PairStructs get(fn pair_structs) map hash(blake2_128_concat) (CurrencyIdOf<T>, CurrencyIdOf<T>) => TokenPair<T>;
	}
}

decl_event!(
	pub enum Event<T> 
	where 
		AccountId = <T as frame_system::Trait>::AccountId,
		CurrencyId = CurrencyIdOf<T>,
		Shares = BalanceOf<T>,
		Balance = BalanceOf<T>,
	{
		Initialized(AccountId, CurrencyId, CurrencyId, Shares),
		Swapped(AccountId, Balance, CurrencyId, Balance),
		Invested(AccountId, CurrencyId, Shares);
		Devested(AccountId, CurrencyId, Shares);
	}
);

decl_error! {
	pub enum Error for Module<T: Trait> {
		LowTokenAmount,
		TokenAExists,
		TotalShareIsNotNull,
		PairNotExists,
		SamePair,
		LowAmountReceived,
		InsufficientPool,
		InvalidShares,
		InsufficientShares,
		DoesNotOwnShare,
		LowAmountOut,
	}
}

decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		type Error = Error<T>;
		fn deposit_event() = default;

		// to-do: #[weight = 10_000 + T::DbWeight::get().writes(1)]
		pub fn initialize_exchange(origin, tokenA: CurrencyIdOf<T>, tokenA_amount: T::BalanceOf<T>, tokenB: CurrencyIdOf<T>, tokenB_amount: T::BalanceOf<T>) -> dispatch::DispatchResult{
			let sender = ensure_signed(origin)?;
			ensure!(tokenA_amount > 0, Error::<T>::LowTokenAmount);
			ensure!(tokenB_amount > 0, Error::<T>::LowTokenAmount);

			let token_pair = Self::pair_structs(tokenA, tokenB);
			ensure!(token_pair.invariant = 0, Error::<T>::TokenAExists);
			ensure!(token_pair.total_shares = 0, Error::<T>::TotalShareIsNotNull);

			let shares_map = BTreeMap::new();
			shares_map::insert(sender, INITIAL_SHARES)
			let pair = TokenPair {
				fee_rate: FEERATE,
				token_a_pool: tokenA_amount,
				token_b_pool: tokenB_amount,
				invariant: tokenA_amount * tokenB_amount,
				total_shares: INITIAL_SHARES,
				shares: shares_map,
			}
			PairStructs::<T>::insert((tokenA,tokenB) pair);
			// TO-DO use with_transaction_result(||{Currency::transfer} to replace
			// <generic_asset::Module<T>>::make_transfer_with_event(&tokenA, &sender, &DEX_ACCOUNT_ID, tokenA_amount)?;
			// <generic_asset::Module<T>>::make_transfer_with_event(&tokenB, &sender, &DEX_ACCOUNT_ID, tokenB_amount)?;
			Self::deposit_event(RawEvent::Initialized(sender, tokenA, tokenB, INITIAL_SHARES));
			Ok(())
		}
		#[weight = 10_000]
		pub fn token_to_token_swap(origin, token_from: CurrencyIdOf<T>, token_from_amount: T::BalanceOf<T>, token_to: CurrencyIdOf<T>,  min_token_received: BalanceOf<T>, receiver: T::AccountId)-> dispatch::DispatchResult{
			let sender = ensure_signed(origin)?;
			ensure!(token_from != token_to, Error::<T>::SamePair);		
			ensure!(PairStructs::<T>::contains_key(token_from, token_to), Error::<T>::PairNotExists);

			let pair = Self::pair_structs(token_from, token_to);
			let fee = token_from_amount * pair.fee_rate / FEE_RATE_DENOMINATOR.into();
			let new_token_from_pool = pair.token_a_pool + token_from_amount;
			let temp_token_from_pool = new_token_from_pool - fee;
			let new_token_to_pool = pair.invariant / temp_token_from_pool;
			let tokens_out = pair.token_b_pool - new_token_to_pool;
			ensure!(tokens_out >= min_token_received, Error::<T>::LowAmountReceived);
			ensure!(tokens_out <= pair.token_b_pool, Error::<T>::InsufficientPool);

			<PairStructs<T>>::mutate(token, |pair| {
				pair.token_a_pool = new_token_from_pool;
				pair.token_b_pool = new_token_to_pool;
				pair.invariant = token_a_pool * token_b_pool;
			})
			// TO-DO use with_transaction_result(||{Currency::transfer} to replace
			// <generic_asset::Module<T>>::make_transfer_with_event(&token_from, &sender, &DEX_ACCOUNT_ID, token_from_amount)?;
			// <generic_asset::Module<T>>::make_transfer_with_event(&token_to, &DEX_ACCOUNT_ID, &receiver, min_token_received)?;
			Self::deposit_event(RawEvent::Swapped(token_from, token_from_amount, token_to, min_token_received));
			Ok(())
		}
		pub fn invest_liquidity(origin, token_a: CurrencyIdOf<T>, token_b: CurrencyIdOf<T>, shares: BalanceOf<T>)-> dispatch::DispatchResult{
			let sender = ensure_signed(origin)?;
			ensure!(token_a != token_b, Error::<T>::SamePair);
			ensure!(PairStructs::<T>::contains_key(token_a, token_b), Error::<T>::PairNotExist);
			let pair = Self::pair_structs(token_a, token_b);
			// let token_b_per_share = pair.token_b_pool/pair.total_shares;
			// let token_b_cost = token_b_per_share * shares;
			let token_a_per_share = pair.token_a_pool/pair.total_shares;
			let token_a_cost = token_a_per_share * shares;

			<PairStructs<T>>::mutate(token_a, |pair|{
				let updated_shares = if let Some(prev_shares) = pair.shares.get(&sender){
					*prev_shares + shares
				} else {
					share
				};
				pair.shares.insert(sender.clone(), updated_shares);
				pair.total_shares += shares;
				//pair.token_b_pool += token_b_cost;
				pair.token_a_pool += token_a_cost;
				pair.invariant = pair.token_b_pool * pair.token_a_pool;
			})

			// <generic_asset::Module<T>>::make_transfer_with_event(&token_a, &sender, &DEX_ACCOUNT_ID, token_b_cost)?;
			// <generic_asset::Module<T>>::make_transfer_with_event(&token_b, &sender, &DEX_ACCOUNT_ID, token_a_cost)?;
			Self::deposit_event(RawEvent::Invested(sender, token_a, shares));
			Ok(())

		}
		pub fn devest_liquidity(origin, token_a: CurrencyIdOf<T>, token_b: CurrencyIdOf<T>, shares_burned: BalanceOf<T>, min_token_a_received: BalanceOf<T>)-> dispatch::DispatchResult{
			let sender = ensure_signed(origin)?;
			ensure!(token_a != token_b, Error::<T>::SamePair);
			ensure!(PairStructs::<T>::contains_key(token_a, token_b), Error::<T>::PairNotExist);

			ensure!(shares_burned > 0, Error::<T>::InvalidShares);
			if let Some(shares) = pair.shares.get(&sender){
				ensure!(*shares >= shares_burned, Error::<T>::InsufficientShares);
			} else {
				Err(Error::<T>::DoesNotOwnShare.into())
			}
			let pair = Self::pair_structs(token_a, token_b);
			// let token_b_per_share = pair.token_b_pool/pair.total_shares;
			// let token_b_cost = token_b_per_share * shares_burned;
			let token_a_per_share = pair.token_a_pool/pair.total_shares;
			let token_a_cost = token_a_per_share * shares_burned;

			//ensure!(token_b_cost >= min_token_b_received, Error::<T>::LowAmountOut);
			ensure!(token_a_cost >= min_token_a_received, Error::<T>::LowAmountOut);

			<PairStructs<T>>::mutate(token_a, token_b, |pair| {			
				if let Some(share) = pair.shares_burned.get_mut(&sender){
					*share -= shares_burned;
				}
				pair.total_shares -= shares_burned;
				//pair.token_b_pool -= token_b_cost;
				pair.token_a_pool -= token_a_cost;
				if pair.total_shares == 0){
					pair.invariant = 0;
				} else {
					pair.invariant = pair.token_a_pool * pair. token_b_pool;
				}
			});

			// <generic_asset::Module<T>>::make_transfer_with_event(&token_a, &DEX_ACCOUNT_ID, &sender, token_b_cost)?;
			// <generic_asset::Module<T>>::make_transfer_with_event(&token_b, &DEX_ACCOUNT_ID, &sender, token_a_cost)?;
			Self::deposit_event(RawEvent::Devested(sender, token_a, shares_burned));
			Ok(())
		}
			
	}
}

