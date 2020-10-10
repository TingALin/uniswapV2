#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode};
use frame_support::{decl_module, decl_storage, decl_event, decl_error, dispatch, traits::Get};
use frame_system::ensure_signed;
use sp_std::collections::BTreeMap;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature ="std")]
pub use serde::{Deserialize, Serialize};

type BalanceOf<T> = <T as generic_asset::Trait>::Balance;
type DexAccount<T> = <T as frame_system::Trait>::AccountId;

const MINIMUM_LIQUIDITY:u32 = 10_000;
const INITIAL_SHARES: u32 = 1000;
const FEERATE: u32 = 3; 
const FEE_RATE_DENOMINATOR: u32 = 1000;
const DEX_ACCOUNT_ID: DexAccount = AccountId::default(); // to-do

pub trait Trait: frame_system::Trait {
	type Event: From<Event<Self>> 
		+ Into<<Self as frame_system::Trait>::Event>
		+ Into<<Self as generic_asset::Trait>::Event>;
	type AssetId: Into<<Self as generic_asset::Trait>::AssetId; //TO- DO
}

#[cfg_attr(feature="std", derive(Serialize, Deserialize))]
#[derive(Encode, Decode, Clone, PartialEq, Eq, Debug)]
pub struct TokenPair<T:Trait> {
	fee_rate: BalanceOf<T>,
	token_a_pool: BalanceOf<T>,
	token_b_pool: BalanceOf<T>,
	invariant: BalanceOf<T>,
	total_shares:BalanceOf<T>,
	shares:BTreeMap<T::AccountId, BalanceOf<T>>,	
}

decl_storage! {
	trait Store for Module<T: Trait> as TemplateModule {
		// T::AssetId, T::AssetId use linkedlist???
		pub PairStructs get(fn pair_structs) map hash(blake2_128_concat) (T::AssetId, T::AssetId) => TokenPair<T>;
	}
}

decl_event!(
	pub enum Event<T> 
	where 
		AccountId = <T as frame_system::Trait>::AccountId,
		<T as Trait>::AssetId,
		Shares = BalanceOf<T>,
		Balance = BalanceOf<T>,
	{
		Initialized(AccountId, AssetId, AssetId, Shares),
		Swapped(AccountId, Balance, AssetId, Balance),
		Invested(AccountId, AssetId, AssetId, Shares);
		Devested(AccountId, AssetId, AssetId, Shares);
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
		pub fn initialize_exchange(origin, tokenA: T::AssetId, tokenA_amount: T::BalanceOf<T>, tokenB: T::AssetId, tokenB_amount: T::BalanceOf<T>) -> dispatch::DispatchResult{
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
			<generic_asset::Module<T>>::make_transfer_with_event(&tokenA, &sender, &DEX_ACCOUNT_ID, tokenA_amount)?;
			<generic_asset::Module<T>>::make_transfer_with_event(&tokenB, &sender, &DEX_ACCOUNT_ID, tokenB_amount)?;
			Self::deposit_event(RawEvent::Initialized(sender, tokenA, tokenB, INITIAL_SHARES));
			Ok(())
		}
		#[weight = 10_000]
		pub fn token_to_token_swap(origin, token_from: T::AssetId, token_from_amount: T::BalanceOf<T>, token_to: T::AssetId,  min_token_received: BalanceOf<T>, receiver: T::AccountId)-> dispatch::DispatchResult{
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
			<generic_asset::Module<T>>::make_transfer_with_event(&token_from, &sender, &DEX_ACCOUNT_ID, token_from_amount)?;
			<generic_asset::Module<T>>::make_transfer_with_event(&token_to, &DEX_ACCOUNT_ID, &receiver, min_token_received)?;
			Self::deposit_event(RawEvent::Swapped(token_from, token_from_amount, token_to, min_token_received));
			Ok(())
		}
		pub fn invest_liquidity(origin, token_a: T::AssetId, token_b: T::AssetId, shares: BalanceOf<T>)-> dispatch::DispatchResult{
			let sender = ensure_signed(origin)?;
			ensure!(token_a != token_b, Error::<T>::SamePair);
			ensure!(PairStructs::<T>::contains_key(token_a, token_b), Error::<T>::PairNotExist);
			let pair = Self::pair_structs(token_a, token_b);
			let token_b_per_share = pair.token_b_pool/pair.total_shares;
			let token_b_cost = token_b_per_share * shares;
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
				pair.token_b_pool += token_b_cost;
				pair.token_a_pool += token_a_cost;
				pair.invariant = pair.ksm_pool * pair.token_a_pool;
			})

			<generic_asset::Module<T>>::make_transfer_with_event(&token_a, &sender, &DEX_ACCOUNT_ID, token_b_cost)?;
			<generic_asset::Module<T>>::make_transfer_with_event(&token_b, &sender, &DEX_ACCOUNT_ID, token_a_cost)?;
			Self::deposit_event(RawEvent::Invested(sender, token_a, token_b, shares));
			Ok(())

		}
		pub fn devest_liquidity(origin, token_a: T::AssetId, token_b: T::AssetId, shares_burned: BalanceOf<T>, min_token_a_received: BalanceOf<T>, min_token_b_received: BalanceOf<T> )-> dispatch::DispatchResult{
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
			let token_b_per_share = pair.token_b_pool/pair.total_shares;
			let token_b_cost = token_b_per_share * shares_burned;
			let token_a_per_share = pair.token_a_pool/pair.total_shares;
			let token_a_cost = token_a_per_share * shares_burned;

			ensure!(token_b_cost >= min_token_b_received, Error::<T>::LowAmountOut);
			ensure!(token_a_cost >= min_token_a_received, Error::<T>::LowAmountOut);

			<PairStructs<T>>::mutate(token_a, token_b, |pair| {			
				if let Some(share) = pair.shares_burned.get_mut(&sender){
					*share -= shares_burned;
				}
				pair.total_shares -= shares_burned;
				pair.token_b_pool -= token_b_cost;
				pair.token_a_pool -= token_a_cost;
				if pair.total_shares == 0){
					pair.invariant = 0;
				} else {
					pair.invariant = pair.token_a_pool * pair. token_b_pool;
				}
			});

			<generic_asset::Module<T>>::make_transfer_with_event(&token_a, &DEX_ACCOUNT_ID, &sender, token_b_cost)?;
			<generic_asset::Module<T>>::make_transfer_with_event(&token_b, &DEX_ACCOUNT_ID, &sender, token_a_cost)?;
			Self::deposit_event(RawEvent::Devested(sender, token_a, token_b, shares_burned));
			Ok(())
		}
			
	}
}

// impl <T: Trait> Module <T> {
// 	fn skim( ) {}
// 	fn sync( ) {}
// 	fn flash_swapping(){}
// 	fn WTHE(){}
// 	//time-weighted average prices
// 	fn update(blance_a:BalanceOf<T>, balance_b:BalanceOf<T>, tokenA_amount: BalanceOf<T>, tokenB_amount: BalanceOf<T>){
// 		let blockTimestampLast:u128 =; //
// 		let blockTimestamp = timestamp; //  
// 		let timeElapsed:u32 = blockTimestamp - blockTimestampLast;
// 		let mut price_cumulative_a:u128 = 0;
// 		let mut price_cumulative_b:u128 = 0;
// 		if timeElapsed > 0 {
// 			price_cumulative_a += tokenB_amount/tokenA_amount * timeElapsed
// 			price_cumulative_b += tokenA_amount/tokenB_amount * timeElapsed
// 		}
// 	}
	
// }
// // computes the direction and magnitude of the profit-maximizing trade
// pub fn swap_to_price(token_a_price: BalanceOf<T>, token_b_price: BalanceOf<T>, token_a:AssetId, token_b:AssetId, token_a_max_spend: BalanceOf<T>, token_b_max_spend:BalanceOf<T>, receiver:AccountId){
// 	//  to-do : tokena/b exits
// 	// token_a != token_b
// 	//token_a_price/b > 0
// 	let pair = Self::pair_structs(token_a, token_b)
// 	let aToB = pair.token_a_pool * token_b_price / pair.token_b_pool;
// 	let left: f32; // type: check later
// 	let right: f32;
// 	if aToB < token_a_price{ 
// 		left = (pair.invariant * token_a_price *1000 / token_b_price * 997).into().sqrt();
// 		right = (pair.token_a_pool * 1000/997).into();	
// 		max_spend = token_a_max_spend;
// 	} else {
// 		left = (pair.invariant * token_b_price *1000 / token_a_price * 997).into().sqrt();
// 		right = (pair.token_b_pool * 1000/997).into();
// 		max_spend = token_b_max_spend;
// 	}
// 	// compute the amount that must be sent to move the price to the profit-maximizing price
// 	let amount_in = left - right; // tokena or b????
// 	if amount_in > max_spend{
// 		let amount_in = max_spend;
// 	}
// 	// return mount_in
// }
