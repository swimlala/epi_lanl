CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:43Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190543  20181005190543  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��j;�E1   @��j��@1z�G��c�j~��#1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  Aa��A���A���A�  A�  A�33A�  A�33A�  B   B  B��B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C��3C�  C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� DfD�fDfD� D  D� D��D� D  D� DfD�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� DfD�fD��D� D   D �fD!  D!y�D"  D"�fD#  D#� D$fD$� D%  D%y�D&  D&� D'  D'y�D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D3��D4� D4��D5� D6fD6� D6��D7y�D7��D8� D9  D9� D9��D:y�D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DCy�DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJy�DJ��DKy�DK��DL�fDM  DM� DN  DN� DN��DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DWfDW�fDX  DX� DX��DY� DZ  DZ� D[  D[� D[��D\y�D]  D]� D^fD^�fD_  D_y�D_��D`y�Da  Da�fDbfDb�fDc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDhfDh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Ds��Dt� Dt��Duy�Dv  Dv� Dw  Dw� Dw�fDy�
D�,�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A  A$  AD  Ae��A���A���A�  A�  A�33A�  A�33A�  B  B	  B��B  B!  B)  B1  B9  BA  BIffBQ  BY  Ba  Bh��Bq  By  B�� B�� B�� B�� B�� B�L�B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ B�L�BԀ B؀ B܀ B�� B� B� B� B�L�B� B�� B�� C @ C@ C@ C@ CY�C
Y�C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>Y�C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT&fCV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�3C�3C�3C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�,�C�  C�  C�3C�  C�,�C�,�C�,�C�  C�,�C�,�C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�3C�3C�  C�3C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�,�C�  C�  C�  C�  C�3C�  C�,�C�  C�  C�  C�3C�3C�3C�  C�,�C�  C�  C�  C�  C�3C�3C�3C�3C�3C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�3C�3C�3D 	�D � D D� D D� D D� D D� D D� DfD� D D� D D� D	 D	� D
 D
� D D� DfD�fDfD� D D� D	�D� D D� DfD�fD D� D D��D D� D D� D D� D D� D D�fDfD� D D� D D� D D� D D� DfD�fD	�D� D  D �fD! D!��D" D"�fD# D#� D$fD$� D% D%��D& D&� D' D'��D( D(� D) D)�fD* D*� D+ D+� D, D,� D- D-� D.fD.� D/ D/� D0fD0� D1 D1� D2 D2� D3 D3� D4	�D4� D5	�D5� D6fD6� D7	�D7��D8	�D8� D9 D9� D:	�D:��D; D;� D< D<� D= D=� D>fD>� D? D?� D@ D@� DA DA� DB DB�fDC DC��DD	�DD� DE DE� DF DF� DG DG� DH DH� DIfDI�fDJ DJ��DK	�DK��DL	�DL�fDM DM� DN DN� DO	�DO� DP DP��DQ DQ� DR DR� DS DS� DT DT� DU DU� DV	�DV� DWfDW�fDX DX� DY	�DY� DZ DZ� D[ D[� D\	�D\��D] D]� D^fD^�fD_ D_��D`	�D`��Da Da�fDbfDb�fDc Dc� Dd Dd� De De� Df Df� DgfDg�fDhfDh� Di Di� Dj Dj��Dk Dk� Dl Dl��Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq��Dr Dr� Ds Ds� Dt	�Dt� Du	�Du��Dv Dv� Dw Dw� Dw�fDy�
D�4�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�%A�bA�JA�VA�VA��A��A� �A�(�A�33A�33A�7LA�&�A��A�bA�A��A���A���A��A���A�AϼjAϼjA��A�oA��A��A�oA��A���A�O�A�ƨA�t�A�VA�x�A΅AΑhAΗ�A΅A�n�A�S�A�C�A�$�A��A�n�A�7LAˁA� �AɅA� �A�Q�AƋDA�33A���A®A��A���A�?}A�p�A���A���A�?}A�z�A���A�1A�G�A���A�JA��9A�~�A�(�A�E�A�9XA�K�A�$�A��`A�M�A���A��\A��A���A���A��/A�z�A���A��`A�ZA��A�x�A�ƨA���A�l�A�$�A�\)A�O�A���A���A�S�A���A�I�A|�uAz-Aw"�Ar�uAq&�AoƨAm?}Ak�PAj��Ah�yAf��Ae/Ac+A`ȴA\�9AX�AT�yAQ+APE�AO;dANA�AJ��AG��AD��AC��AB  AA
=A?�
A>bNA=�#A=��A<ȴA:��A9O�A8��A7S�A5�A3p�A1ƨA0A.-A,�yA*v�A)\)A)/A(�A(�DA(�A't�A&�jA$�A#�A"�DA"v�A"�A!��AS�A(�A��A��AbNA�A��A�FA"�A�!A�
A|�A�A�A��A5?A�uAƨA
=A�RAbNA��AI�AC�A�9A�A�hA&�A
�A
r�A	+A�A�TA�Ap�A33A1AS�A%A��AA��A"�A ��A �A �A ^5A 5?@�@��^@�  @��@�/@�+@�@�K�@��@�{@��@��@�p�@�\)@�^@�G�@�u@�|�@��#@�A�@�(�@��@�~�@���@�(�@�dZ@�"�@��H@�=q@� �@�33@�O�@؃@�ƨ@�+@�^5@�%@�r�@ӕ�@��@�r�@�bN@϶F@�K�@��y@Ο�@��@Ͳ-@���@��;@�K�@�n�@ɡ�@�V@ț�@� �@�t�@���@�Q�@�(�@�b@���@��
@Ý�@�|�@���@Ý�@�K�@�o@�+@�@��@�M�@��@�@���@�?}@�V@�V@��@�%@��j@���@�S�@��H@�=q@�O�@�r�@��F@�K�@��!@�E�@�J@��@�p�@�(�@�l�@���@��y@���@�ff@��T@���@��D@�(�@�ƨ@��R@�$�@��-@�V@�Ĝ@���@�Ĝ@�I�@�1@��m@���@��@�33@�~�@��@��@���@��@�@���@��7@�X@��@��@���@�z�@��;@��P@�dZ@��@���@���@�J@�7L@�7L@���@��/@�Ĝ@���@�1'@���@��@�33@�ȴ@�^5@��@�/@���@�Z@�1@��P@��\@�ff@�{@��T@��7@��/@���@��@��/@��u@�9X@��@�b@��w@���@�C�@��@�o@���@�~�@�=q@�@�p�@��@�%@�&�@�?}@�x�@�`B@��@��`@���@�Z@��u@��@��P@�C�@�S�@���@�ff@���@���@���@�/@��
@��@���@��y@��\@��@���@��^@��@�X@�7L@���@�j@�  @��@�S�@�+@���@�n�@�^5@�M�@�E�@�-@��#@�`B@��@���@�r�@�1'@�b@��;@���@�|�@�K�@��H@�v�@�^5@�-@�{@�@���@�?}@��@��j@�bN@�9X@��@��@�|�@�l�@�
=@���@�-@��@�J@��@��7@�X@�7L@��@�Z@���@���@��F@��P@�S�@�+@��H@��\@�~�@�V@�5?@�{@��@��T@��#@��-@��7@�x�@�G�@��`@���@�r�@�bN@�Q�@�A�@�1'@�I�@���@v�@e�'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�
=A�%A�bA�JA�VA�VA��A��A� �A�(�A�33A�33A�7LA�&�A��A�bA�A��A���A���A��A���A�AϼjAϼjA��A�oA��A��A�oA��A���A�O�A�ƨA�t�A�VA�x�A΅AΑhAΗ�A΅A�n�A�S�A�C�A�$�A��A�n�A�7LAˁA� �AɅA� �A�Q�AƋDA�33A���A®A��A���A�?}A�p�A���A���A�?}A�z�A���A�1A�G�A���A�JA��9A�~�A�(�A�E�A�9XA�K�A�$�A��`A�M�A���A��\A��A���A���A��/A�z�A���A��`A�ZA��A�x�A�ƨA���A�l�A�$�A�\)A�O�A���A���A�S�A���A�I�A|�uAz-Aw"�Ar�uAq&�AoƨAm?}Ak�PAj��Ah�yAf��Ae/Ac+A`ȴA\�9AX�AT�yAQ+APE�AO;dANA�AJ��AG��AD��AC��AB  AA
=A?�
A>bNA=�#A=��A<ȴA:��A9O�A8��A7S�A5�A3p�A1ƨA0A.-A,�yA*v�A)\)A)/A(�A(�DA(�A't�A&�jA$�A#�A"�DA"v�A"�A!��AS�A(�A��A��AbNA�A��A�FA"�A�!A�
A|�A�A�A��A5?A�uAƨA
=A�RAbNA��AI�AC�A�9A�A�hA&�A
�A
r�A	+A�A�TA�Ap�A33A1AS�A%A��AA��A"�A ��A �A �A ^5A 5?@�@��^@�  @��@�/@�+@�@�K�@��@�{@��@��@�p�@�\)@�^@�G�@�u@�|�@��#@�A�@�(�@��@�~�@���@�(�@�dZ@�"�@��H@�=q@� �@�33@�O�@؃@�ƨ@�+@�^5@�%@�r�@ӕ�@��@�r�@�bN@϶F@�K�@��y@Ο�@��@Ͳ-@���@��;@�K�@�n�@ɡ�@�V@ț�@� �@�t�@���@�Q�@�(�@�b@���@��
@Ý�@�|�@���@Ý�@�K�@�o@�+@�@��@�M�@��@�@���@�?}@�V@�V@��@�%@��j@���@�S�@��H@�=q@�O�@�r�@��F@�K�@��!@�E�@�J@��@�p�@�(�@�l�@���@��y@���@�ff@��T@���@��D@�(�@�ƨ@��R@�$�@��-@�V@�Ĝ@���@�Ĝ@�I�@�1@��m@���@��@�33@�~�@��@��@���@��@�@���@��7@�X@��@��@���@�z�@��;@��P@�dZ@��@���@���@�J@�7L@�7L@���@��/@�Ĝ@���@�1'@���@��@�33@�ȴ@�^5@��@�/@���@�Z@�1@��P@��\@�ff@�{@��T@��7@��/@���@��@��/@��u@�9X@��@�b@��w@���@�C�@��@�o@���@�~�@�=q@�@�p�@��@�%@�&�@�?}@�x�@�`B@��@��`@���@�Z@��u@��@��P@�C�@�S�@���@�ff@���@���@���@�/@��
@��@���@��y@��\@��@���@��^@��@�X@�7L@���@�j@�  @��@�S�@�+@���@�n�@�^5@�M�@�E�@�-@��#@�`B@��@���@�r�@�1'@�b@��;@���@�|�@�K�@��H@�v�@�^5@�-@�{@�@���@�?}@��@��j@�bN@�9X@��@��@�|�@�l�@�
=@���@�-@��@�J@��@��7@�X@�7L@��@�Z@���@���@��F@��P@�S�@�+@��H@��\@�~�@�V@�5?@�{@��@��T@��#@��-@��7@�x�@�G�@��`@���@�r�@�bN@�Q�@�A�@�1'@�I�@���@v�@e�'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B%B%B+B+B%B	7BhBbB�B#�B-B_;B�^B	�+B	�wB	ĜB	ƨB	��B	��B	��B	�B	�B	�/B	�HB	�yB
�B
YB
�bB
��B
�B
�BB&�BXBT�BR�B`BBdZBgmBk�Bn�Bn�Bm�Bm�Bm�Bl�Bn�Bt�Bv�Bq�Bs�B��B�^BÖB�B�NB�mB��B  B�B�B(�B8RBM�B[#BaHBdZBe`BgmBhsBhsBffBffB_;BVBD�B5?B6FB�B�B�B�B�B+B��B�B��B��B�RB�-B��B�+B2-BB
�B
��B
��B
�+B
s�B
L�B
�B	�TB	�wB	�B	��B	�B	y�B	s�B	k�B	p�B	n�B	gmB	]/B	T�B	I�B	:^B	%�B	uB��B�B�`B�5B��BŢB�}BŢB��B��BɺBȴBƨBĜBÖB��B�jB�LB�FB�-B�-B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�FB�XB�RB�^B�^B�dB�dB�dB��BBƨB��B��B��B��B��B��BɺB��B��B��B��B�B�ZB�ZB�ZB�HB�HB�;B�HB�BB�/B�#B�;B�ZB�fB�sB�yB�yB�fB�ZB�`B�ZB�ZB�HB�`B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	1B	PB	VB	VB	uB	�B	�B	�B	�B	"�B	#�B	$�B	'�B	/B	2-B	49B	9XB	:^B	:^B	:^B	;dB	A�B	C�B	E�B	G�B	H�B	J�B	K�B	L�B	N�B	P�B	R�B	T�B	VB	[#B	\)B	]/B	_;B	aHB	bNB	e`B	hsB	jB	k�B	k�B	l�B	n�B	n�B	o�B	t�B	w�B	y�B	|�B	~�B	� B	�B	�B	�B	�+B	�7B	�7B	�1B	�=B	�DB	�JB	�PB	�\B	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�LB	�XB	�qB	�wB	��B	B	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�5B	�HB	�ZB	�`B	�`B	�`B	�fB	�fB	�B	�B	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
($B
49222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B%B%B+B+B%B	7BhBbB�B#�B-B_;B�^B	�+B	�wB	ĜB	ƨB	��B	��B	��B	�B	�B	�/B	�HB	�yB
�B
YB
�bB
��B
�B
�BB&�BXBT�BR�B`BBdZBgmBk�Bn�Bn�Bm�Bm�Bm�Bl�Bn�Bt�Bv�Bq�Bs�B��B�^BÖB�B�NB�mB��B  B�B�B(�B8RBM�B[#BaHBdZBe`BgmBhsBhsBffBffB_;BVBD�B5?B6FB�B�B�B�B�B+B��B�B��B��B�RB�-B��B�+B2-BB
�B
��B
��B
�+B
s�B
L�B
�B	�TB	�wB	�B	��B	�B	y�B	s�B	k�B	p�B	n�B	gmB	]/B	T�B	I�B	:^B	%�B	uB��B�B�`B�5B��BŢB�}BŢB��B��BɺBȴBƨBĜBÖB��B�jB�LB�FB�-B�-B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�FB�XB�RB�^B�^B�dB�dB�dB��BBƨB��B��B��B��B��B��BɺB��B��B��B��B�B�ZB�ZB�ZB�HB�HB�;B�HB�BB�/B�#B�;B�ZB�fB�sB�yB�yB�fB�ZB�`B�ZB�ZB�HB�`B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	1B	PB	VB	VB	uB	�B	�B	�B	�B	"�B	#�B	$�B	'�B	/B	2-B	49B	9XB	:^B	:^B	:^B	;dB	A�B	C�B	E�B	G�B	H�B	J�B	K�B	L�B	N�B	P�B	R�B	T�B	VB	[#B	\)B	]/B	_;B	aHB	bNB	e`B	hsB	jB	k�B	k�B	l�B	n�B	n�B	o�B	t�B	w�B	y�B	|�B	~�B	� B	�B	�B	�B	�+B	�7B	�7B	�1B	�=B	�DB	�JB	�PB	�\B	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�LB	�XB	�qB	�wB	��B	B	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�5B	�HB	�ZB	�`B	�`B	�`B	�fB	�fB	�B	�B	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
($B
49222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.25 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190543                              AO  ARCAADJP                                                                    20181005190543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190543  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190543  QCF$                G�O�G�O�G�O�8000            