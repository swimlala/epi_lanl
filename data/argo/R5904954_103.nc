CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:12Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191712  20181005191712  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               gA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$��԰1   @��%K�%\@4���S���dJ5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      gA   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B ffB��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���C  C  C  C�C
  C�fC  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C7�fC9�fC<  C>  C@  CB�CD�CF�CH�CJ  CK�fCN  CP  CR  CT  CU�fCX  CZ  C[�fC]�fC`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��C��C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C��3C�  C�  C��C��3C�  C�  C�  C�  C��C�  C��C��C��C�  C��C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C��3C�  C��fC��3C�  C�  C�  C�  C��C��C�  D   D �fD  D� D  D� DfD� D��D�fDfD�fD  D� D��D� D��Dy�D	  D	� D
fD
�fDfD� D  D��DfD� D  D�fD  D� D�3D� D  D� D  D� DfD�fD  D� D  D� D  Dy�D��D� D��Ds3D��Dy�D��Dy�D  D� D  D�fD  D� D  Ds3D  D�fD   D � D!fD!� D!��D"� D#  D#� D$fD$y�D%  D%� D&  D&�fD&��D'y�D(  D(� D)  D)� D*fD*�fD+  D+y�D,  D,� D-  D-� D.  D.� D/fD/� D0fD0� D1  D1y�D2  D2� D3fD3� D3��D4y�D4��D5y�D6  D6� D6��D7y�D8  D8� D9fD9�fD:fD:� D;fD;�fD<  D<�fD<��D=�fD>fD>� D>�3D?� D@fD@� D@��DAy�DB  DB�fDCfDC� DD  DD� DE  DEy�DE��DFy�DF��DGy�DH  DH� DI  DIy�DJ  DJ�fDK  DK� DL  DL� DM  DM� DNfDN�fDO  DO� DPfDP�fDQfDQ�fDRfDR� DR��DS�fDT  DT� DU  DU�fDVfDV�fDW  DW�fDX�DX�fDX��DY� DZfDZ�fD[fD[�fD[��D\y�D]  D]s3D]��D^� D_  D_y�D_��D`� Da  Da�fDb  Db� Dc  Dc� Dd  Ddy�De  De�fDf  Df� Dg  Dgy�Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dm� DnfDn� Do  Doy�Do��Dp� DqfDq�fDr  Dr� DsfDs� Dt  Dt�fDu  Duy�Du��Dvy�Dv��Dw� Dw� Dy�RD�:=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @N�R@�\)@ʏ\AG�A%G�AEG�AeG�A���A���A���A���A£�Aң�A�p�A�p�B�RB�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B�u�B���B���B���C :�CT{CT{CT{CnC
T{C:�CT{CT{CT{CT{CnCT{CT{CT{CT{C T{C"T{C$T{C&T{C(T{C*T{C,T{C.T{C0nC2T{C4T{C6T{C8:�C::�C<T{C>T{C@T{CBnCDnCFnCHnCJT{CL:�CNT{CPT{CRT{CTT{CV:�CXT{CZT{C\:�C^:�C`T{CbT{Cd:�CfT{ChT{CjT{ClT{CnT{CpT{CrT{CtT{CvT{CxT{CzT{C|T{C~T{C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�7
C�*=C�*=C�*=C�pC�*=C�7
C�*=C�*=C�7
C�7
C�*=C�7
C�*=C�*=C�7
C�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�pC�pC�*=C�*=C�pC�*=C�7
C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�*=C�*=C�*=C�*=C�pC�pC�*=C�*=C�7
C�*=C�*=C�pC�*=C�*=C�7
C�pC�*=C�*=C�*=C�*=C�7
C�*=C�7
C�7
C�C�C�*=C�7
C�*=C�*=C�pC�*=C�*=C�7
C�*=C�pC�*=C�*=C�*=C�*=C�pC�*=C�7
C�7
C�7
C�7
C�*=C�pC�*=C��C�pC�*=C�*=C�*=C�*=C�7
C�7
C�*=D D ��DD�DD�D�D�D�D��D�D��DD�D�D�D�D��D	D	�D
�D
��D�D�DD��D�D�DD��DD�DRD�DD�DD�D�D��DD�DD�DD��D�D�D�D�RD�D��D�D��DD�DD��DD�DD�RDD��D D �D!�D!�D"�D"�D#D#�D$�D$��D%D%�D&D&��D'�D'��D(D(�D)D)�D*�D*��D+D+��D,D,�D-D-�D.D.�D/�D/�D0�D0�D1D1��D2D2�D3�D3�D4�D4��D5�D5��D6D6�D7�D7��D8D8�D9�D9��D:�D:�D;�D;��D<D<��D=�D=��D>�D>�D?RD?�D@�D@�DA�DA��DBDB��DC�DC�DDDD�DEDE��DF�DF��DG�DG��DHDH�DIDI��DJDJ��DKDK�DLDL�DMDM�DN�DN��DODO�DP�DP��DQ�DQ��DR�DR�DS�DS��DTDT�DUDU��DV�DV��DWDW��DX!�DX��DY�DY�DZ�DZ��D[�D[��D\�D\��D]D]�RD^�D^�D_D_��D`�D`�DaDa��DbDb�DcDc�DdDd��DeDe��DfDf�DgDg��Dh�Dh��DiDi�DjDj�DkDk�DlDl��Dm�Dm�Dn�Dn�DoDo��Dp�Dp�Dq�Dq��DrDr�Ds�Ds�DtDt��DuDu��Dv�Dv��Dw�Dw�Dw�Dy�qD�D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�r�A�r�A�t�A�v�A�r�A�v�A�t�A�v�A�x�A�K�A���A��A��mA��/A�ȴA��A�M�A�x�A�(�A�7LA�A�t�A��A�ȴA�JA�z�A�bA�M�A��`A�C�AŅAĲ-A��HA��/A��A��hA���A�VA�$�A��wA�/A���A�l�A�JA���A�
=A��A��mA�^5A�l�A��A��yA��mA���A�M�A�JA���A��A�ffA���A��A��A���A�$�A��DA���A��A�33A�K�A�^5A��+A��A���A��A�~�A��A�r�A�n�A���A�S�A���A�%A�VA��#A��A���A���A���A�A���A��!A���A�?}AC�A~M�A{�^AzQ�AvffAtZAs&�ArjAq�hAn(�Ak�Aj�+AfjAe/AdVAc\)Aa|�A_7LA]��AZ��AW�TAUC�ATE�AS��ASG�AR-AP��AM��AJĜAI��AHz�AD��AC��AC"�AB��AAA<��A;ƨA:��A:-A9��A8v�A6v�A5�7A4��A4I�A3��A3A/��A-��A,�+A,  A+`BA+G�A+�A*��A*1'A(�!A%�mA%S�A$�`A$Q�A#�;A"z�A!33A 9XA�;A%A�;AXA�A��A�7A�AQ�A��AK�A~�AA/A$�A��AAA�^A"�A�wAVA1'AĜA��A&�A�RA�A�AjAp�A	dZA	&�A	�A�HA��A-A  A��A��AA�;A��A��A��A$�AdZA�yAffAA��A��A��A�A��A��A33A Z@��@�J@�j@���@�J@�ȴ@�  @�o@�&�@ꗍ@�`B@�h@�x�@�7L@�w@�-@�p�@�j@��@��/@�t�@�b@��@ו�@�"�@�ȴ@�~�@�M�@� �@�M�@�/@�33@ͺ^@��@˶F@��@�M�@�x�@Ȭ@�S�@���@���@�`B@Ĭ@�Q�@� �@�  @���@å�@�@��y@\@�V@�$�@��#@�p�@���@�bN@�b@���@�
=@�n�@��7@��
@�o@��!@��#@�%@�1@��P@�\)@�S�@��y@�V@�x�@�V@���@�  @�ƨ@�|�@���@�v�@��@��#@��h@�Z@�(�@���@��@��@��R@�/@��@��m@��@�n�@�x�@�j@���@��;@�I�@��@�X@���@�p�@��T@�9X@�Z@� �@�;d@���@���@��u@���@�p�@�M�@�V@��@��@��T@��^@�x�@�?}@��@��P@�o@���@�@��H@��+@�~�@��T@�Q�@�o@�$�@���@��@�7L@�V@�X@��-@��T@�X@�Ĝ@�Ĝ@���@��w@�b@�1@�1@��F@�\)@�"�@�"�@�C�@�o@�o@�o@��y@���@�-@�@��@��T@��^@�x�@�X@��`@��@���@�j@�I�@� �@���@�ƨ@�|�@�C�@�+@��@��R@��+@�^5@���@��7@�hs@�G�@�7L@�&�@��@��`@� �@��
@��@�l�@�;d@�"�@��@�
=@�@��H@�ȴ@�n�@�J@���@�?}@���@���@��u@�bN@�Z@�bN@�Z@�A�@��@���@�+@�ff@��T@�X@��@��9@��@��@��
@���@��@�|�@�K�@�@�ȴ@��R@��R@��R@���@�n�@�{@��@�$�@�5?@��@��-@�&�@�r�@��@�;d@�ȴ@�~�@�M�@��@���@��#@���@��@�/@���@��`@��9@��u@�Q�@� �@���@�ƨ@��P@�33@�
=@���@���@�=q@���@�@��@�?}@���@��@�z�@�bN@�I�@�  @~�1@lbN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�r�A�r�A�t�A�v�A�r�A�v�A�t�A�v�A�x�A�K�A���A��A��mA��/A�ȴA��A�M�A�x�A�(�A�7LA�A�t�A��A�ȴA�JA�z�A�bA�M�A��`A�C�AŅAĲ-A��HA��/A��A��hA���A�VA�$�A��wA�/A���A�l�A�JA���A�
=A��A��mA�^5A�l�A��A��yA��mA���A�M�A�JA���A��A�ffA���A��A��A���A�$�A��DA���A��A�33A�K�A�^5A��+A��A���A��A�~�A��A�r�A�n�A���A�S�A���A�%A�VA��#A��A���A���A���A�A���A��!A���A�?}AC�A~M�A{�^AzQ�AvffAtZAs&�ArjAq�hAn(�Ak�Aj�+AfjAe/AdVAc\)Aa|�A_7LA]��AZ��AW�TAUC�ATE�AS��ASG�AR-AP��AM��AJĜAI��AHz�AD��AC��AC"�AB��AAA<��A;ƨA:��A:-A9��A8v�A6v�A5�7A4��A4I�A3��A3A/��A-��A,�+A,  A+`BA+G�A+�A*��A*1'A(�!A%�mA%S�A$�`A$Q�A#�;A"z�A!33A 9XA�;A%A�;AXA�A��A�7A�AQ�A��AK�A~�AA/A$�A��AAA�^A"�A�wAVA1'AĜA��A&�A�RA�A�AjAp�A	dZA	&�A	�A�HA��A-A  A��A��AA�;A��A��A��A$�AdZA�yAffAA��A��A��A�A��A��A33A Z@��@�J@�j@���@�J@�ȴ@�  @�o@�&�@ꗍ@�`B@�h@�x�@�7L@�w@�-@�p�@�j@��@��/@�t�@�b@��@ו�@�"�@�ȴ@�~�@�M�@� �@�M�@�/@�33@ͺ^@��@˶F@��@�M�@�x�@Ȭ@�S�@���@���@�`B@Ĭ@�Q�@� �@�  @���@å�@�@��y@\@�V@�$�@��#@�p�@���@�bN@�b@���@�
=@�n�@��7@��
@�o@��!@��#@�%@�1@��P@�\)@�S�@��y@�V@�x�@�V@���@�  @�ƨ@�|�@���@�v�@��@��#@��h@�Z@�(�@���@��@��@��R@�/@��@��m@��@�n�@�x�@�j@���@��;@�I�@��@�X@���@�p�@��T@�9X@�Z@� �@�;d@���@���@��u@���@�p�@�M�@�V@��@��@��T@��^@�x�@�?}@��@��P@�o@���@�@��H@��+@�~�@��T@�Q�@�o@�$�@���@��@�7L@�V@�X@��-@��T@�X@�Ĝ@�Ĝ@���@��w@�b@�1@�1@��F@�\)@�"�@�"�@�C�@�o@�o@�o@��y@���@�-@�@��@��T@��^@�x�@�X@��`@��@���@�j@�I�@� �@���@�ƨ@�|�@�C�@�+@��@��R@��+@�^5@���@��7@�hs@�G�@�7L@�&�@��@��`@� �@��
@��@�l�@�;d@�"�@��@�
=@�@��H@�ȴ@�n�@�J@���@�?}@���@���@��u@�bN@�Z@�bN@�Z@�A�@��@���@�+@�ff@��T@�X@��@��9@��@��@��
@���@��@�|�@�K�@�@�ȴ@��R@��R@��R@���@�n�@�{@��@�$�@�5?@��@��-@�&�@�r�@��@�;d@�ȴ@�~�@�M�@��@���@��#@���@��@�/@���@��`@��9@��u@�Q�@� �@���@�ƨ@��P@�33@�
=@���@���@�=q@���@�@��@�?}@���@��@�z�@�bN@�I�@�  @~�1@lbN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BE�BE�BD�BD�BD�BD�BD�BD�BD�BD�BA�B=qBB�BE�BG�BG�B=qBJB��B��B��BB%B	7B{B�B#�B'�B-B<jB@�BI�BQ�BZBcTBl�Bu�B�B�%B�=B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�By�Bu�Bs�Bk�B_;BVBO�BJ�B>wB$�BVB��B�#B��B��B�^B��B�JB�Bx�BVB#�B�BoB
��B
�`B
ǮB
�jB
�?B
��B
�{B
�7B
�B
t�B
dZB
O�B
;dB
2-B
#�B
�B
B	��B	�B	�`B	�5B	ȴB	�XB	�B	��B	�PB	�7B	�B	z�B	n�B	aHB	R�B	@�B	1'B	,B	&�B	$�B	�B	�B		7B��B��B�B�HB�)B�B��B��BĜB��B�qB�dB�XB�FB�3B�!B�B�B��B��B��B��B��B�{B�uB�oB�hB�bB�VB�DB�7B�+B�%B�B�B�B� B~�B|�B{�By�Bx�Bz�B|�B|�B}�B}�B~�B� B�B�+B�DB�VB�JB�DB�DB�=B�%B� B{�Bv�Bx�Bw�Bu�Bs�Bs�Br�Bq�Bo�Bk�BjBjBk�Bk�BjBjBk�Bk�Bl�Bk�Bp�Bs�Bu�B|�B�B�%B�%B�JB�oB�{B��B��B��B��B��B�uB�bB�+B�B�%B�+B�B�B�1B�DB�+B�DB�uB��B��B��B��B��B��B��B��B��B�B�!B�-B�-B�-B�-B�'B�FB�^B�jB��BĜBŢBƨBǮBȴB��B��B��B��B��B��B�B�
B�B�B�B�B�5B�5B�HB�TB�ZB�`B�mB�yB�B�B�B��B��B	  B	%B	1B	1B	JB	hB	�B	�B	�B	�B	�B	!�B	&�B	)�B	,B	0!B	1'B	2-B	7LB	8RB	9XB	9XB	<jB	=qB	=qB	>wB	A�B	@�B	?}B	?}B	=qB	<jB	;dB	8RB	;dB	9XB	1'B	/B	49B	6FB	<jB	@�B	A�B	F�B	[#B	^5B	^5B	[#B	YB	]/B	aHB	dZB	jB	t�B	z�B	|�B	� B	�B	�B	�B	�B	�%B	�B	�B	�B	�%B	�1B	�DB	�PB	�DB	�%B	�B	�%B	�+B	�7B	�=B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�RB	�^B	�^B	�^B	�jB	�wB	�}B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�;B	�HB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�<B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BE�BE�BD�BD�BD�BD�BD�BD�BD�BD�BA�B=qBB�BE�BG�BG�B=qBJB��B��B��BB%B	7B{B�B#�B'�B-B<jB@�BI�BQ�BZBcTBl�Bu�B�B�%B�=B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�By�Bu�Bs�Bk�B_;BVBO�BJ�B>wB$�BVB��B�#B��B��B�^B��B�JB�Bx�BVB#�B�BoB
��B
�`B
ǮB
�jB
�?B
��B
�{B
�7B
�B
t�B
dZB
O�B
;dB
2-B
#�B
�B
B	��B	�B	�`B	�5B	ȴB	�XB	�B	��B	�PB	�7B	�B	z�B	n�B	aHB	R�B	@�B	1'B	,B	&�B	$�B	�B	�B		7B��B��B�B�HB�)B�B��B��BĜB��B�qB�dB�XB�FB�3B�!B�B�B��B��B��B��B��B�{B�uB�oB�hB�bB�VB�DB�7B�+B�%B�B�B�B� B~�B|�B{�By�Bx�Bz�B|�B|�B}�B}�B~�B� B�B�+B�DB�VB�JB�DB�DB�=B�%B� B{�Bv�Bx�Bw�Bu�Bs�Bs�Br�Bq�Bo�Bk�BjBjBk�Bk�BjBjBk�Bk�Bl�Bk�Bp�Bs�Bu�B|�B�B�%B�%B�JB�oB�{B��B��B��B��B��B�uB�bB�+B�B�%B�+B�B�B�1B�DB�+B�DB�uB��B��B��B��B��B��B��B��B��B�B�!B�-B�-B�-B�-B�'B�FB�^B�jB��BĜBŢBƨBǮBȴB��B��B��B��B��B��B�B�
B�B�B�B�B�5B�5B�HB�TB�ZB�`B�mB�yB�B�B�B��B��B	  B	%B	1B	1B	JB	hB	�B	�B	�B	�B	�B	!�B	&�B	)�B	,B	0!B	1'B	2-B	7LB	8RB	9XB	9XB	<jB	=qB	=qB	>wB	A�B	@�B	?}B	?}B	=qB	<jB	;dB	8RB	;dB	9XB	1'B	/B	49B	6FB	<jB	@�B	A�B	F�B	[#B	^5B	^5B	[#B	YB	]/B	aHB	dZB	jB	t�B	z�B	|�B	� B	�B	�B	�B	�B	�%B	�B	�B	�B	�%B	�1B	�DB	�PB	�DB	�%B	�B	�%B	�+B	�7B	�=B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�RB	�^B	�^B	�^B	�jB	�wB	�}B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�;B	�HB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�<B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191712                              AO  ARCAADJP                                                                    20181005191712    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191712  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191712  QCF$                G�O�G�O�G�O�8000            