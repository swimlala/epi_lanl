CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-21T15:43:19Z creation;2023-02-21T15:43:20Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230221154319  20230221155659  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���l��1   @����to@;X���F�c���v�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A���A�  A�  A�33A�  A�  A�33B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C�fC�fC   C"�C$�C&  C(  C)�fC,  C.  C0  C1�fC4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D�fDfD� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD�fD  D� D��D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&fD&�fD'  D'y�D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dxy�Dy  Dy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fDfD� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�|�D���D�  D�@ D��3D��3D�3D�C3D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D���D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D D�� D���D�@ DÀ D��3D�  D�<�DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D���D�@ Dɀ D�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�  D�@ D�|�D�� D�3D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�<�Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D��3D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�@ D׀ D��3D�  D�@ D؀ D�� D���D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D���D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���@���AffA>ffA^ffA~ffA�  A�33A�33A�ffA�33A�33A�ffA�33B��B��B��B34B'��B/��B7��B?��BG��BO��BW��B`  Bg��Bo34Bw34B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bϙ�B���B���Bۙ�B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC
  C�fC�fC�fC�fC�fC�fC�fC�fC��C��C�fC"  C$  C%�fC'�fC)��C+�fC-�fC/�fC1��C3�fC6  C7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi��Ck�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C�  C��3C��3C�  C��3C��3C��3C��3C��3C�  C�  C��3C��3C��fC��3C�  C��3C��fC��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C��3C��fC��3C��3C��fC��3C�  C�  C��3C�  C��3C��3C�  C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C�  C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��D� D��Dy�D��Dy�D��D� D  Dy�D�4Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D  D� D��Dy�D�4Dy�D��Dy�D��Dy�D��Ds4D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"�4D#y�D#��D$y�D$��D%y�D&  D&� D&��D's4D'�4D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-� D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2s4D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBs4DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D`  D`y�D`��Da� Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgs4Dg��Dhy�Dh��Di� Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds�4Dty�Dt��Duy�Du��Dvy�Dv��Dw� Dw��Dxs4Dx��Dy� Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~� D  Dy�D��D�<�D�|�D���D���D�9�D�y�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�9�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�y�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�� D���D���D�@ D�|�D���D���D�<�D�� D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�9�D�y�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�y�D���D���D�<�D�� D�� D�  D�@ D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�y�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�D�� D���D�9�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�y�D̼�D�  D�<�D�|�D͹�D���D�<�D�|�Dμ�D���D�9�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�D�� D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�@ D�|�Dּ�D���D�<�D�|�D�� D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�y�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�9�D�y�D湚D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�y�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D� D��D���D�<�D�|�D���D���D�<�D�|�DD���D�<�D�|�D��D���D�9�D�|�D��D���D�<�D�|�D��D���D�9�D�y�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^�A�aA�ffA�g8A�h�A�jA�k�A�l�A�m)A�l�A�n/A�o�A�poA�qAA�r|A�sA�rA�sMA�v+A�xA�v�A�v�A�oiA�e,A�]�A�Z�A�V�A�L�A�:^A�$�A��A�q�A���A�bA�� A���A��A��sA�_�A�ΥA���A��4A���A�͟A��CA�dZA�MA�I�A�e�A�zDA�m)A�jA�`�A�UgA�:�A���A�5?A��]A��yA�B'A��2A���A�k�A�f�A�K�A��A��8A���A��HA�)�A�_pA��wA��LA��)A���A���A��[A��TA��;A�0�A� 4A���A���A��RA��DA���A��?A�=qA�4nA���A��A��bA�*eA��A���A�A�A~xlA}+kAz($Aw��AvQAu=�AtX�As`BAq��Apy�Ao�Ao�IAoZAo4An�An��An��An��Am�XAh�"Ae�Ae��Ab��A_�]A_'�A^MjA]�A\��A[��AZ�DAZ��AY��AYw�AXqvAV��AS�IAQ�AOJ#AN�nAN�AL�?AK��AKHAJںAJJ�AI��AI&AHl�AFD�AE/�ADϫAD�AC��ACK�AA�)AA~(AAv�AAA@�A@&�A@�A?l"A>X�A= �A<u�A;e,A9C�A8_A7B[A5��A52�A4!A3A1�'A0��A/v`A.E�A-  A+��A*��A*�A)�4A)�A({A'ĜA'�[A'c A'?A'7LA'�A'�A'A';A&�>A&� A%�A!-�A @OA�KA8�AA�bA'RAM�A� A��Al�ACA��AXAy�A�[A��A~A�|As�A��AE�A-�Au�A�oA4�A��AK�A($A
�"A
�A
�gA
�XA
l�A	�dA�CA��A&As�A�#Ah�AK�A
�AOA��A�KA ��A ?@�1�@��]@���@�s�@���@�J#@���@��@�	�@���@�e@�$@��@�ϫ@���@�V@�6@�J#@��@��@�.@�7@�S@�?}@���@���@�N�@�.@��]@�7@�e@�p;@�!�@��@�\�@ۊ�@ڑ�@ٷ�@��@�W�@�2�@׷@���@��@�q�@��@���@��@͞�@�V�@���@ˊ	@ʱ�@�'R@�^�@�S�@�Dg@�Z@Ĺ�@�ߤ@���@�d�@��@���@��m@�+k@���@�:�@��y@�=q@�j@�V@��F@��@�!�@���@�D�@��@�zx@�-�@�d�@�Xy@��K@��{@��@���@�w�@��;@�?}@���@���@��~@��@��c@��B@�($@�x�@�n�@��Q@��R@�-@���@��p@�xl@�1@���@�RT@��]@��@���@�W?@��[@�k�@���@��Y@�c @�H@��@��@�IR@�(�@��@��@��'@�GE@�7@��m@��@���@���@���@�5�@��P@��@���@���@�l"@�M�@�"h@���@���@��h@�p�@�=@��@���@��9@���@���@���@��@��@��@�t�@���@�!@��Q@��7@��@�x@�@� �@��@���@��g@��X@��@�[W@�=�@�q@�ȴ@���@��;@��@�@�@�{@��j@��@�rG@�Q�@��@�>B@��@��@�خ@��@���@�}�@�s@�hs@�]�@�P�@�=@�%F@��"@���@���@��.@���@���@�|�@�d�@�M�@�L0@�?@�!�@��@�zx@��@���@��2@�~�@���@���@��:@�S&@��@�!�@�C@�+@��@��@��@���@�N�@�/�@�~@��@�|�@�O�@�@��@�ߤ@�I�@�O@��@~�"@~��@~^5@}�3@}�@|��@|�E@|Ĝ@|��@|r�@{�6@{dZ@{W?@{�@z�+@zC�@y�z@yx�@x��@x�@w��@w$t@vTa@u�n@uT�@uDg@u5�@uV@t�@s�6@s��@s��@sƨ@s�a@sƨ@s�@s�@s@O@r��@r�\@r^5@r5?@r�@qϫ@qk�@q-w@p�@pI�@o�[@n^5@m��@m�~@mQ�@m \@l�o@k��@k��@kg�@k;d@j�}@j �@i}�@iY�@h��@h��@h�@hM@g��@g�:@gx@go�@gqv@go�@g_p@g=@f�b@e�@eu�@ea�@eO�@e�@d��@d2�@c�P@bȴ@bW�@b3�@a�.@a��@a��@a��@a�X@aA @a%@`֡@`@^Q@]L�@]V@\�P@\�@\�@\�/@\��@\�/@\�v@\�@\�`@\�@\�@\�@\�@\�@\��@\�@\�E@\�_@\1'@[a@[�@Z�6@Z�@Z�A@Z}V@Zd�@ZQ@Z#:@Y��@Yw2@YQ�@Yq@Y%@X�?@Xl"@X6@X  @W��@W�K@WZ�@V0U@U%@T��@T�@T�@T�o@Toi@TS�@TN�@T6@T	�@S� @S�F@S��@SC�@R�@R	@Q�@Q��@Q��@Q��@Q;@P�4@PtT@Pg8@PPH@P9X@PG@O�@O@N�@N�h@N�F@Nv�@N_�@NL0@N1�@M�.@Mf�@M@L�@L�@L��@L_@L�@K��@K��@K�*@K�V@K��@K��@J��@J�@I��@I��@I��@I��@I��@I�h@I��@I`B@I7L@I2a@I#�@H�`@H�$@G��@G+@F��@Fff@F	@E��@D��@DXy@D%�@C��@C��@Ce�@CE9@C,�@B��@B�<@BC�@A@A��@Ac�@AF@A�@@��@@�v@@�@@�_@@�_@@�_@@z�@@x@?�m@?�0@?>�@?�@>�B@>n�@>1�@=�T@=u�@=+@<r�@<M@;�r@;�r@;˒@;��@;{J@;s@;o�@;a@;X�@;E9@;1�@:��@:
�@8�@8bN@8N�@84n@7�;@7"�@6�@6�@6�@6�'@6�!@6:*@5��@5rG@54@4��@4�4@3��@3��@38@2�c@2�R@2��@2��@2��@2��@2Ta@1�@1�d@1��@1��@1s�@15�@0�@0��@0�D@0@/�V@.ں@.E�@,Ĝ@,�@,�I@,��@,�@,g8@,`�@,e�@,~(@,c�@,N�@,7@+�W@+l�@+C�@+E9@+H�@+&@+&@+"�@+�@*�@*��@*&�@)�@(�/@'��@'Y@&�@&�@&z@&kQ@&^5@&M�@%��@%@%�M@$��@$z�@#�m@#�	@#v`@#J#@#>�@#8@#+@#,�@#1�@#$t@"��@"��@!*0@ $@l�@8@ߤ@��@�+@kQ@R�@H�@H�@V@Z�@B[@0U@($@_@�@�d@�@��@rG@�	@��@�_@�@M@ƨ@��@{J@'�@��@��@n�@d�@i�@Ov@8�@J@��@�)@�.@]d@�+@�*@�$@y�@o@�6@W�@�@`B@T�@<6@V@�e@��@h�@�m@��@~�@qv@g�@g�@S�@;d@"�@��@u%@�@ԕ@�3@��@|@L�@�f@��@�@�u@y>@bN@>B@*�@7@�@��@U�@�@�y@ȴ@�@xl@c @L0@=q@�@�@��@��@u�@:�@�@�@�@z�@G@�W@�&@�g@��@�V@�@W?@
� @	�N@	�@	�X@	�"@	p�@	\�@	Q�@	L�@	=�@��@��@��@q@H@2�@-�@�@g�@.I@&@�@�@�8@͟@��@Q@ϫ@S&@Q�@T�@[W@^�@a�@f�@j@j@[W@�@�@��@h�@�@��@�6@�@@��@qv@e�@X�@S�@O@K�@E9@@O@>�@=@�@�"@��@͟@�@��@�A111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^�A�aA�ffA�g8A�h�A�jA�k�A�l�A�m)A�l�A�n/A�o�A�poA�qAA�r|A�sA�rA�sMA�v+A�xA�v�A�v�A�oiA�e,A�]�A�Z�A�V�A�L�A�:^A�$�A��A�q�A���A�bA�� A���A��A��sA�_�A�ΥA���A��4A���A�͟A��CA�dZA�MA�I�A�e�A�zDA�m)A�jA�`�A�UgA�:�A���A�5?A��]A��yA�B'A��2A���A�k�A�f�A�K�A��A��8A���A��HA�)�A�_pA��wA��LA��)A���A���A��[A��TA��;A�0�A� 4A���A���A��RA��DA���A��?A�=qA�4nA���A��A��bA�*eA��A���A�A�A~xlA}+kAz($Aw��AvQAu=�AtX�As`BAq��Apy�Ao�Ao�IAoZAo4An�An��An��An��Am�XAh�"Ae�Ae��Ab��A_�]A_'�A^MjA]�A\��A[��AZ�DAZ��AY��AYw�AXqvAV��AS�IAQ�AOJ#AN�nAN�AL�?AK��AKHAJںAJJ�AI��AI&AHl�AFD�AE/�ADϫAD�AC��ACK�AA�)AA~(AAv�AAA@�A@&�A@�A?l"A>X�A= �A<u�A;e,A9C�A8_A7B[A5��A52�A4!A3A1�'A0��A/v`A.E�A-  A+��A*��A*�A)�4A)�A({A'ĜA'�[A'c A'?A'7LA'�A'�A'A';A&�>A&� A%�A!-�A @OA�KA8�AA�bA'RAM�A� A��Al�ACA��AXAy�A�[A��A~A�|As�A��AE�A-�Au�A�oA4�A��AK�A($A
�"A
�A
�gA
�XA
l�A	�dA�CA��A&As�A�#Ah�AK�A
�AOA��A�KA ��A ?@�1�@��]@���@�s�@���@�J#@���@��@�	�@���@�e@�$@��@�ϫ@���@�V@�6@�J#@��@��@�.@�7@�S@�?}@���@���@�N�@�.@��]@�7@�e@�p;@�!�@��@�\�@ۊ�@ڑ�@ٷ�@��@�W�@�2�@׷@���@��@�q�@��@���@��@͞�@�V�@���@ˊ	@ʱ�@�'R@�^�@�S�@�Dg@�Z@Ĺ�@�ߤ@���@�d�@��@���@��m@�+k@���@�:�@��y@�=q@�j@�V@��F@��@�!�@���@�D�@��@�zx@�-�@�d�@�Xy@��K@��{@��@���@�w�@��;@�?}@���@���@��~@��@��c@��B@�($@�x�@�n�@��Q@��R@�-@���@��p@�xl@�1@���@�RT@��]@��@���@�W?@��[@�k�@���@��Y@�c @�H@��@��@�IR@�(�@��@��@��'@�GE@�7@��m@��@���@���@���@�5�@��P@��@���@���@�l"@�M�@�"h@���@���@��h@�p�@�=@��@���@��9@���@���@���@��@��@��@�t�@���@�!@��Q@��7@��@�x@�@� �@��@���@��g@��X@��@�[W@�=�@�q@�ȴ@���@��;@��@�@�@�{@��j@��@�rG@�Q�@��@�>B@��@��@�خ@��@���@�}�@�s@�hs@�]�@�P�@�=@�%F@��"@���@���@��.@���@���@�|�@�d�@�M�@�L0@�?@�!�@��@�zx@��@���@��2@�~�@���@���@��:@�S&@��@�!�@�C@�+@��@��@��@���@�N�@�/�@�~@��@�|�@�O�@�@��@�ߤ@�I�@�O@��@~�"@~��@~^5@}�3@}�@|��@|�E@|Ĝ@|��@|r�@{�6@{dZ@{W?@{�@z�+@zC�@y�z@yx�@x��@x�@w��@w$t@vTa@u�n@uT�@uDg@u5�@uV@t�@s�6@s��@s��@sƨ@s�a@sƨ@s�@s�@s@O@r��@r�\@r^5@r5?@r�@qϫ@qk�@q-w@p�@pI�@o�[@n^5@m��@m�~@mQ�@m \@l�o@k��@k��@kg�@k;d@j�}@j �@i}�@iY�@h��@h��@h�@hM@g��@g�:@gx@go�@gqv@go�@g_p@g=@f�b@e�@eu�@ea�@eO�@e�@d��@d2�@c�P@bȴ@bW�@b3�@a�.@a��@a��@a��@a�X@aA @a%@`֡@`@^Q@]L�@]V@\�P@\�@\�@\�/@\��@\�/@\�v@\�@\�`@\�@\�@\�@\�@\�@\��@\�@\�E@\�_@\1'@[a@[�@Z�6@Z�@Z�A@Z}V@Zd�@ZQ@Z#:@Y��@Yw2@YQ�@Yq@Y%@X�?@Xl"@X6@X  @W��@W�K@WZ�@V0U@U%@T��@T�@T�@T�o@Toi@TS�@TN�@T6@T	�@S� @S�F@S��@SC�@R�@R	@Q�@Q��@Q��@Q��@Q;@P�4@PtT@Pg8@PPH@P9X@PG@O�@O@N�@N�h@N�F@Nv�@N_�@NL0@N1�@M�.@Mf�@M@L�@L�@L��@L_@L�@K��@K��@K�*@K�V@K��@K��@J��@J�@I��@I��@I��@I��@I��@I�h@I��@I`B@I7L@I2a@I#�@H�`@H�$@G��@G+@F��@Fff@F	@E��@D��@DXy@D%�@C��@C��@Ce�@CE9@C,�@B��@B�<@BC�@A@A��@Ac�@AF@A�@@��@@�v@@�@@�_@@�_@@�_@@z�@@x@?�m@?�0@?>�@?�@>�B@>n�@>1�@=�T@=u�@=+@<r�@<M@;�r@;�r@;˒@;��@;{J@;s@;o�@;a@;X�@;E9@;1�@:��@:
�@8�@8bN@8N�@84n@7�;@7"�@6�@6�@6�@6�'@6�!@6:*@5��@5rG@54@4��@4�4@3��@3��@38@2�c@2�R@2��@2��@2��@2��@2Ta@1�@1�d@1��@1��@1s�@15�@0�@0��@0�D@0@/�V@.ں@.E�@,Ĝ@,�@,�I@,��@,�@,g8@,`�@,e�@,~(@,c�@,N�@,7@+�W@+l�@+C�@+E9@+H�@+&@+&@+"�@+�@*�@*��@*&�@)�@(�/@'��@'Y@&�@&�@&z@&kQ@&^5@&M�@%��@%@%�M@$��@$z�@#�m@#�	@#v`@#J#@#>�@#8@#+@#,�@#1�@#$t@"��@"��@!*0@ $@l�@8@ߤ@��@�+@kQ@R�@H�@H�@V@Z�@B[@0U@($@_@�@�d@�@��@rG@�	@��@�_@�@M@ƨ@��@{J@'�@��@��@n�@d�@i�@Ov@8�@J@��@�)@�.@]d@�+@�*@�$@y�@o@�6@W�@�@`B@T�@<6@V@�e@��@h�@�m@��@~�@qv@g�@g�@S�@;d@"�@��@u%@�@ԕ@�3@��@|@L�@�f@��@�@�u@y>@bN@>B@*�@7@�@��@U�@�@�y@ȴ@�@xl@c @L0@=q@�@�@��@��@u�@:�@�@�@�@z�@G@�W@�&@�g@��@�V@�@W?@
� @	�N@	�@	�X@	�"@	p�@	\�@	Q�@	L�@	=�@��@��@��@q@H@2�@-�@�@g�@.I@&@�@�@�8@͟@��@Q@ϫ@S&@Q�@T�@[W@^�@a�@f�@j@j@[W@�@�@��@h�@�@��@�6@�@@��@qv@e�@X�@S�@O@K�@E9@@O@>�@=@�@�"@��@͟@�@��@�A111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B'�B'�B'�B'�B'�B'�B'mB'�B'�B'�B'�B'�B'�B'�B'�B(
B(
B(
B(
B(
B(>B(XB(�B(�B(�B(�B)*B(�B(sB(sB($B'8B"4BBmB
=B�B��B�B�KB�B�
B�B��B��B��BŢBǔB��B�!B��BޞBݲBܒB�7B�@BʌB��B��B��B��ByXBu�Bt�Bq�Bk�Bf�Bd�B^�BP.B7fB&�BB�B��B�xB��Bp�BK)B7fB0�BB�B�bB�xB�HB��B��BshBS&BC�B<�B*B;B�B�B�B
�XB
�hB
��B
�uB
�xB
��B
��B
�B
��B
�GB
�}B
��B
�B
�6B
�0B
�yB
��B
��B
�'B
��B
z�B
w�B
oiB
^�B
V�B
Q�B
KxB
IRB
DgB
@4B
>BB
:�B
6FB
/OB
!�B
�B
�B	�$B	��B	�B	��B	�
B	�B	�B	�B	��B	��B	��B	�hB	�B	�B	�vB	�B	āB	�qB	�DB	��B	��B	��B	�fB	��B	�FB	��B	�qB	�B	�$B	�VB	�=B	�aB	�B	�	B	��B	}�B	w�B	sMB	l�B	d�B	`�B	^OB	Z�B	X_B	VSB	T�B	RB	O�B	P.B	PHB	O�B	O�B	O�B	PHB	P.B	P}B	P�B	N�B	M�B	AoB	9�B	8RB	5?B	49B	2B	1[B	-B	*0B	'B	]B	kB	�B	�B	�B	uB	HB	"B	�B	�B		B	�B	 �B��B�$B�RB��B�ZB�tB��B�B�B�B�B�B�B�B�_B�
B�mB��B�,B�tB�B�-B�B��B�pB��B�B�	BچB�eB��B�+BּB��B��BԯB�B�uB�BѝB�BѝBѝB��BбB��B��BбB�HB�B�(B̈́B��B��B��B�VB͹B�B�B�B��B̈́B�6B̈́B�B̘B�dB�pB�<BοB�BBϫB�.B��B�&B��B��B�7B�B�)B�B�IBݲB�IB�B��B�B��B�5B�VB�-B��B��B�B�B�hB�B��B�>B�B��B�_B�DB�DB�B�B��B	�B	B	�B	�B	VB	.B	�B	�B	jB	#�B	&LB	&�B	'RB	)�B	+kB	+�B	/B	4�B	6�B	:DB	>B	>�B	@�B	A�B	C-B	E9B	IB	J�B	KDB	SuB	S�B	V�B	W�B	X_B	X�B	ZQB	\)B	_pB	`\B	`�B	a�B	b�B	fB	f�B	hXB	jB	jB	j�B	kB	l�B	m�B	ncB	oiB	poB	p�B	qAB	r-B	t9B	t�B	u�B	vFB	w2B	xB	yXB	�B	�3B	��B	��B	��B	�mB	�
B	��B	�OB	�bB	�:B	��B	�B	��B	��B	� B	��B	��B	�;B	�'B	�B	�B	��B	�nB	��B	��B	��B	�EB	�=B	�xB	�6B	ϫB	��B	ЗB	ҽB	�KB	�7B	��B	یB	�xB	��B	�IB	�~B	��B	�B	�jB	�!B	��B	�HB	��B	�`B	�B	��B	��B	�B	�sB	�B	�B	�KB	�6B	�CB	�iB	�hB	��B
�B
tB
	�B

�B
�B
VB
�B
�B
�B
�B
�B
�B
[B
�B
B

B
�B
�B
�B
�B
OB
�B
�B
#nB
$&B
&�B
)_B
*KB
+�B
-wB
/iB
2�B
3MB
3�B
4TB
5B
7fB
8�B
8�B
9�B
:�B
;�B
<�B
="B
@�B
B�B
D�B
E�B
H�B
J�B
L~B
MB
MPB
M�B
Q�B
R�B
RTB
RoB
R�B
R�B
R�B
R�B
SB
TFB
VB
V�B
WYB
W�B
XEB
X�B
ZB
Z�B
[qB
]B
^�B
b�B
c�B
d�B
e�B
e�B
gRB
h�B
i*B
i�B
j0B
kkB
l�B
m�B
m�B
n�B
o�B
p;B
r|B
s3B
s3B
s�B
tB
tB
t9B
t�B
u%B
v�B
yXB
z�B
z�B
{B
|jB
~BB
cB
�;B
�B
�SB
�B
�zB
��B
�B
�B
�B
��B
��B
��B
�<B
��B
��B
�?B
��B
��B
��B
�B
�B
�+B
�+B
�+B
�+B
�EB
�+B
�B
�+B
�B
�B
�EB
�_B
�1B
�kB
��B
�~B
��B
��B
�B
�B
�VB
�pB
��B
�HB
��B
�B
��B
��B
�nB
�@B
��B
�B
�B
�`B
��B
��B
�0B
��B
�kB
�kB
�QB
��B
��B
��B
�B
�qB
��B
�CB
��B
�IB
��B
��B
�'B
�AB
�AB
�'B
�-B
�-B
��B
�|B
��B
��B
�MB
��B
�B
��B
�	B
�rB
��B
��B
�*B
�^B
��B
��B
�qB
��B
�(B
��B
�HB
�B
��B
��B
�B
� B
� B
� B
�B
�gB
ĜB
�mB
żB
��B
��B
�B
�%B
ƨB
�+B
�+B
�+B
ǔB
��B
�	B
�DB
˒B
�PB
͹B
�B
��B
�.B
�HB
бB
�4B
��B
�:B
�TB
ҽB
�B
�FB
�2B
՛B
��B
�B
ּB
�
B
�
B
�sB
��B
��B
רB
��B
ٴB
�QB
�QB
�=B
�WB
��B
��B
�IB
��B
޸B
�!B
��B
�bB
�|B
�|B
��B
�hB
�B
�B
�B
��B
�B
��B
��B
� B
�,B
�RB
��B
��B
�
B
��B
�eB
�B
�B
�eB
��B
�B
��B
��B
��B
�CB
��B
�B
�OB
�B
��B
�B
�vB
�[B
�B
�B
�B
�B
�B
��B
��B
�3B
�MB
�B
�9B
�nB
�B
�?B
��B
��B
��B
�DB
��B
��B
��B
��B
�DB
�DB
�^B
�B
�DB
�xB
��B
�B
��B
�B
�B
�B
��B
�PB
�PB
�jB
��B
�B
��B
�BB
�cB �B�B�B'B�B�B�B�B�B�BBmB�BtB�B�B�B�B�B�B�B�B�B�BzB
	B^BdB~BBPB�B�B�B�B�B�B�B�B"B"BVB�B�B�B�BB�B.BHBHB�BhB�B�B:B�B�B@B@B&B[BuB�B,BgB�B�BmB�B�B�B�BB_BB�B�B�B7B�B�B�B�BB]BCBxBxB�B�B�B�B�B5B�B�B�B!B!B�B�B 'B BB vB vB �B �B �B!-B!�B!�B"NB"�B"�B"�B# B#:B#TB#�B#�B#�B$@B$ZB$�B$�B%B%B%FB%�B&�B&�B&�B&�B'B'B'B'RB(�B)�B)�B)�B)�B)�B*0B*B*0B*0B*�B+B+6B+QB+�B+�B+�B,B,�B-)B-B-)B-B-CB-�B-�B./B.�B/�B/iB/iB/iB/iB/iB/iB/iB/iB/iB0B0;B0�B0�B1[B1�B1�B1�B2B2GB2GB2GB2aB2aB2aB2|B2|B2|B2|B2�B2�B2�B3B3hB3hB3�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B'�B'�B'�B'�B'�B'�B'mB'�B'�B'�B'�B'�B'�B'�B'�B(
B(
B(
B(
B(
B(>B(XB(�B(�B(�B(�B)*B(�B(sB(sB($B'8B"4BBmB
=B�B��B�B�KB�B�
B�B��B��B��BŢBǔB��B�!B��BޞBݲBܒB�7B�@BʌB��B��B��B��ByXBu�Bt�Bq�Bk�Bf�Bd�B^�BP.B7fB&�BB�B��B�xB��Bp�BK)B7fB0�BB�B�bB�xB�HB��B��BshBS&BC�B<�B*B;B�B�B�B
�XB
�hB
��B
�uB
�xB
��B
��B
�B
��B
�GB
�}B
��B
�B
�6B
�0B
�yB
��B
��B
�'B
��B
z�B
w�B
oiB
^�B
V�B
Q�B
KxB
IRB
DgB
@4B
>BB
:�B
6FB
/OB
!�B
�B
�B	�$B	��B	�B	��B	�
B	�B	�B	�B	��B	��B	��B	�hB	�B	�B	�vB	�B	āB	�qB	�DB	��B	��B	��B	�fB	��B	�FB	��B	�qB	�B	�$B	�VB	�=B	�aB	�B	�	B	��B	}�B	w�B	sMB	l�B	d�B	`�B	^OB	Z�B	X_B	VSB	T�B	RB	O�B	P.B	PHB	O�B	O�B	O�B	PHB	P.B	P}B	P�B	N�B	M�B	AoB	9�B	8RB	5?B	49B	2B	1[B	-B	*0B	'B	]B	kB	�B	�B	�B	uB	HB	"B	�B	�B		B	�B	 �B��B�$B�RB��B�ZB�tB��B�B�B�B�B�B�B�B�_B�
B�mB��B�,B�tB�B�-B�B��B�pB��B�B�	BچB�eB��B�+BּB��B��BԯB�B�uB�BѝB�BѝBѝB��BбB��B��BбB�HB�B�(B̈́B��B��B��B�VB͹B�B�B�B��B̈́B�6B̈́B�B̘B�dB�pB�<BοB�BBϫB�.B��B�&B��B��B�7B�B�)B�B�IBݲB�IB�B��B�B��B�5B�VB�-B��B��B�B�B�hB�B��B�>B�B��B�_B�DB�DB�B�B��B	�B	B	�B	�B	VB	.B	�B	�B	jB	#�B	&LB	&�B	'RB	)�B	+kB	+�B	/B	4�B	6�B	:DB	>B	>�B	@�B	A�B	C-B	E9B	IB	J�B	KDB	SuB	S�B	V�B	W�B	X_B	X�B	ZQB	\)B	_pB	`\B	`�B	a�B	b�B	fB	f�B	hXB	jB	jB	j�B	kB	l�B	m�B	ncB	oiB	poB	p�B	qAB	r-B	t9B	t�B	u�B	vFB	w2B	xB	yXB	�B	�3B	��B	��B	��B	�mB	�
B	��B	�OB	�bB	�:B	��B	�B	��B	��B	� B	��B	��B	�;B	�'B	�B	�B	��B	�nB	��B	��B	��B	�EB	�=B	�xB	�6B	ϫB	��B	ЗB	ҽB	�KB	�7B	��B	یB	�xB	��B	�IB	�~B	��B	�B	�jB	�!B	��B	�HB	��B	�`B	�B	��B	��B	�B	�sB	�B	�B	�KB	�6B	�CB	�iB	�hB	��B
�B
tB
	�B

�B
�B
VB
�B
�B
�B
�B
�B
�B
[B
�B
B

B
�B
�B
�B
�B
OB
�B
�B
#nB
$&B
&�B
)_B
*KB
+�B
-wB
/iB
2�B
3MB
3�B
4TB
5B
7fB
8�B
8�B
9�B
:�B
;�B
<�B
="B
@�B
B�B
D�B
E�B
H�B
J�B
L~B
MB
MPB
M�B
Q�B
R�B
RTB
RoB
R�B
R�B
R�B
R�B
SB
TFB
VB
V�B
WYB
W�B
XEB
X�B
ZB
Z�B
[qB
]B
^�B
b�B
c�B
d�B
e�B
e�B
gRB
h�B
i*B
i�B
j0B
kkB
l�B
m�B
m�B
n�B
o�B
p;B
r|B
s3B
s3B
s�B
tB
tB
t9B
t�B
u%B
v�B
yXB
z�B
z�B
{B
|jB
~BB
cB
�;B
�B
�SB
�B
�zB
��B
�B
�B
�B
��B
��B
��B
�<B
��B
��B
�?B
��B
��B
��B
�B
�B
�+B
�+B
�+B
�+B
�EB
�+B
�B
�+B
�B
�B
�EB
�_B
�1B
�kB
��B
�~B
��B
��B
�B
�B
�VB
�pB
��B
�HB
��B
�B
��B
��B
�nB
�@B
��B
�B
�B
�`B
��B
��B
�0B
��B
�kB
�kB
�QB
��B
��B
��B
�B
�qB
��B
�CB
��B
�IB
��B
��B
�'B
�AB
�AB
�'B
�-B
�-B
��B
�|B
��B
��B
�MB
��B
�B
��B
�	B
�rB
��B
��B
�*B
�^B
��B
��B
�qB
��B
�(B
��B
�HB
�B
��B
��B
�B
� B
� B
� B
�B
�gB
ĜB
�mB
żB
��B
��B
�B
�%B
ƨB
�+B
�+B
�+B
ǔB
��B
�	B
�DB
˒B
�PB
͹B
�B
��B
�.B
�HB
бB
�4B
��B
�:B
�TB
ҽB
�B
�FB
�2B
՛B
��B
�B
ּB
�
B
�
B
�sB
��B
��B
רB
��B
ٴB
�QB
�QB
�=B
�WB
��B
��B
�IB
��B
޸B
�!B
��B
�bB
�|B
�|B
��B
�hB
�B
�B
�B
��B
�B
��B
��B
� B
�,B
�RB
��B
��B
�
B
��B
�eB
�B
�B
�eB
��B
�B
��B
��B
��B
�CB
��B
�B
�OB
�B
��B
�B
�vB
�[B
�B
�B
�B
�B
�B
��B
��B
�3B
�MB
�B
�9B
�nB
�B
�?B
��B
��B
��B
�DB
��B
��B
��B
��B
�DB
�DB
�^B
�B
�DB
�xB
��B
�B
��B
�B
�B
�B
��B
�PB
�PB
�jB
��B
�B
��B
�BB
�cB �B�B�B'B�B�B�B�B�B�BBmB�BtB�B�B�B�B�B�B�B�B�B�BzB
	B^BdB~BBPB�B�B�B�B�B�B�B�B"B"BVB�B�B�B�BB�B.BHBHB�BhB�B�B:B�B�B@B@B&B[BuB�B,BgB�B�BmB�B�B�B�BB_BB�B�B�B7B�B�B�B�BB]BCBxBxB�B�B�B�B�B5B�B�B�B!B!B�B�B 'B BB vB vB �B �B �B!-B!�B!�B"NB"�B"�B"�B# B#:B#TB#�B#�B#�B$@B$ZB$�B$�B%B%B%FB%�B&�B&�B&�B&�B'B'B'B'RB(�B)�B)�B)�B)�B)�B*0B*B*0B*0B*�B+B+6B+QB+�B+�B+�B,B,�B-)B-B-)B-B-CB-�B-�B./B.�B/�B/iB/iB/iB/iB/iB/iB/iB/iB/iB0B0;B0�B0�B1[B1�B1�B1�B2B2GB2GB2GB2aB2aB2aB2|B2|B2|B2|B2�B2�B2�B3B3hB3hB3�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230221154256  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230221154319  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230221154319  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230221154320                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230221154321  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230221154321  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230221155659                      G�O�G�O�G�O�                