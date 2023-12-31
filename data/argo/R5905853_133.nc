CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-30T09:47:20Z creation;2022-09-30T09:47:20Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220930094720  20220930100111  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��Lk��1   @�򊰼�@.>vȴ9X�cd9XbN1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�33B���B���B���B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&�C(�C*�C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @34@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B���B���B�  B���B�fgB���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B�  B���C�fC�fC�fC�fC	�fC�fC��C�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC&  C(  C*  C+��C-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC`  Ca�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&� D'  D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D|  D|� D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AڙeAڧAښAڙ1A�{�AڌJA�^�A�=�A�'A�+A�!�A���A��BA��/A�ٴA���A��2A���A��vA��^A���A��tA���AٽqAٶFAٱ[A٬qA٩�A٠\AْA�b�A�[�A�%zA�V�A�҉A�FA�v+A�E�A��\A�T,A��A��LA��SA�+A��A�;�A�0�A�;0A��A��jA��A�� A��~A��A���A���A���A���A�_A�\�A��0A��A��mA�_�A��2A�j�A���A�o�A�Q�A���A��`A��DA��
A��A�PA���A}��Ax�-Aw\�Av��Au$As�Ar,=Ap�0Aoc�AjAa��A\��AW͟AVzxARA�AP9XAM�ZAK��AJ\�AGm]AF�AE��AD�A?��A<��A9}�A6��A3�AA.g�A,��A+��A)��A&.IA$�7A#c A"�LA!�A˒A��AMjA�$A  A��A	�A�AݘAI�A�4A!<�A!A ��A!��A#�3A%C-A$|�A#DgA#\�A"�A!rGA!-A��A�
AAU2AE9A�EAXyAA�AOvA�XA(�AD�A��A?A�)A\�A��Ae,A�AL�ASA�|A+kA�yA�AA��A� Aw2A�.A��A.�AѷA,=A�Ac�A
��A
�A	7LA�_AV�A�Ai�A��A>BA�A�"Au�A��AMAh
AW?A��A6�AbA �DA �ZA Z�A S�A �A �A ($@�j�@�,�@��?@�{@�=�@��@�]d@��W@��@��c@��D@��[@��2@���@�H@��@���@�>�@�@���@��@�y�@��@�@�*�@�@�w�@��@�/�@���@��@@�7�@�(�@���@���@�V@��Z@�	@��@莊@��@���@�q�@�q@��|@��M@�e@�=@�J�@��@�8�@�\�@ޜx@�O@ݮ@��8@���@ۗ$@�@�C-@�2a@آ4@��]@�6z@։�@���@�j�@��@��@�6z@ҽ<@�($@�u�@�ߤ@В�@�˒@�0�@�|�@��@�e�@̶�@��@���@ː�@�Y�@��@ʁo@��@�G�@��@��@ȔF@�W�@�@�@�<�@Ǯ@�҉@Ʈ}@ƌ@�tT@�Ta@�e@�G@���@�}�@��s@ę1@�PH@Ñh@°!@�!�@��@��@��@�|@� i@�}V@���@�1�@�/@��6@�?�@�  @���@�(�@�33@��j@�E�@�Vm@���@�U2@�Ta@���@���@�@�@��}@�x@�H�@�0�@��@��6@�&�@��
@�iD@��y@��@��1@�q�@�˒@��@�RT@���@�?�@�qv@���@�x�@�S�@�%F@�	l@��@��L@�kQ@���@��j@��)@��K@�˒@��[@�o�@�ی@���@��)@��]@�/�@�W?@��~@�a@��@�� @�[�@��@�S&@� \@��v@�Z�@�G@���@�rG@�L�@���@��@�N�@��@���@���@�e�@�-w@��@��1@�;�@�b@���@�Vm@��@��@�l�@�5?@��@���@�e�@��@��A@�.�@�(�@�$�@���@�>�@�@��@��@�+k@�}�@�+@��@��}@�n�@���@�J#@��@��@��@��@�n�@�I�@�=q@�#:@��W@���@�?}@�	l@��@��@�6@�-@��t@�e,@��@���@�m�@�I�@�*�@��n@�[W@�/�@��z@�Q@��@��)@���@�N<@��@��P@�~�@��@��z@���@��P@�Mj@�֡@��+@�~@���@�f�@�A @��@��h@���@���@�]d@���@���@�}�@�/@�%@���@���@��o@�J@��d@��k@�n/@�(�@���@��@��h@���@�_@��@���@�8�@���@���@�}V@�a|@�C�@��&@��C@���@��	@�a@��R@�m�@�<�@�	@��@��@�<6@��B@��j@��_@�V�@��D@��a@��k@���@���@��@�]�@��@�҉@��U@���@�i�@�1@���@��9@��*@���@�|�@�c�@�?}@��@��p@��4@���@�Ov@�@=@~��@~@}��@}�@}L�@|�K@|��@|/�@{��@{t�@{W?@{�@z�@z�@z&�@y�X@yp�@y \@xV�@w�	@v�m@v_�@u�@u�h@uS&@t�v@t/�@s�@s]�@r��@rȴ@r��@r�@r1�@q��@q=�@p��@p��@p�@o~�@o�@nZ�@m�t@mj@m@lH@k��@kiD@kF�@j��@jR�@i�)@i��@i}�@i�@h�@g�@f�@f)�@e�t@d�)@d�@c�@c|�@c)_@c�@b�X@b�m@b�R@b��@b�@b($@b@a��@aA @`��@`��@`H@`�@_��@_ƨ@_�F@_��@_=@_
=@^�@^u%@^J@]�T@]@]�@]}�@]+�@\��@[�Q@[��@[iD@Z�2@Z{@X�|@X��@Xz�@Xoi@X�@W��@W�@V�@U��@U��@Ue,@U�@T��@T"h@S�q@R�@R3�@Q�Z@Q��@Q�n@QO�@Q#�@P�`@PĜ@P(�@O{J@O�@N��@NR�@N-@N	@M��@MA @L�U@Lj@L*�@L�@K��@KRT@J�8@J�@J~�@Jl�@JZ�@J�@I�@I��@IX@I	l@H�@H��@He�@Hb@G�@G�Q@G��@G,�@F��@FOv@F{@E8�@D��@D��@DbN@D@C�m@C�*@CiD@CF�@C'�@C(@B�<@Bh
@A�^@Ahs@A[W@As�@AT�@Aq@@�E@@M@?��@?S�@?@>��@>��@>_@=��@=a�@==�@<�@<c�@<A�@;�}@;+@:��@:�@:�}@9�T@9�d@9�@9(�@8V�@81@7��@7��@7��@7S�@7@6�@6��@6q�@5�@5?}@4��@4�@4N�@4!@3�f@3,�@2��@2h
@2&�@1��@1`B@1�@0��@0ی@0�9@0:�@/a@.ߤ@.�@-��@-��@,�@,��@,u�@,<�@, �@+�6@+�@+1�@*�b@*=q@*�@)�N@)��@)�"@)-w@(�`@(��@(��@(|�@(Xy@(7@'�W@'�F@'��@'K�@'.I@'o@&�@&��@&V@&!�@%��@%�3@%�'@%u�@%\�@%0�@$��@$��@$��@$��@$V�@$~@#�&@#��@#�@#\)@"�8@"�<@"H�@"@!ԕ@!�@!�h@!:�@ ��@ �j@ ��@ oi@ D�@�W@�*@��@H�@&@S@��@R�@!�@�9@�=@a�@L�@2a@�@��@��@�Y@_@M@1'@�@�@_p@@O@�@�R@{�@Ov@C�@B[@	@�@�@��@��@p�@�@֡@�_@K^@1@��@33@(@�m@�F@@�@!�@{@	@ϫ@��@N<@�@�@��@��@�@q@V�@7�@x@�A@�;@� @t�@�@S@��@�x@��@C�@��@�#@�@�~@^�@?}@@@�U@��@`�@Q�@4n@�@��@��@Z�@�@��@^5@_@�^@�^@�H@��@?}@�@�@�[@��@�O@�@��@|�@PH@-�@"h@1'@$@��@��@�$@��@y�@n/@a@Mj@,�@(@
�M@
�2@
�@
��@
��@
xl@
:*@
�@	��@	�@	��@	f�@	B�@	�@	%@��@��@��@ѷ@��@�@j@Ft@2�@��@�Q@�@�q@��@j�@H�@)_@�@�@�B@�X@�@�\@l�@Ov@	@
�@�@�t@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AڙeAڧAښAڙ1A�{�AڌJA�^�A�=�A�'A�+A�!�A���A��BA��/A�ٴA���A��2A���A��vA��^A���A��tA���AٽqAٶFAٱ[A٬qA٩�A٠\AْA�b�A�[�A�%zA�V�A�҉A�FA�v+A�E�A��\A�T,A��A��LA��SA�+A��A�;�A�0�A�;0A��A��jA��A�� A��~A��A���A���A���A���A�_A�\�A��0A��A��mA�_�A��2A�j�A���A�o�A�Q�A���A��`A��DA��
A��A�PA���A}��Ax�-Aw\�Av��Au$As�Ar,=Ap�0Aoc�AjAa��A\��AW͟AVzxARA�AP9XAM�ZAK��AJ\�AGm]AF�AE��AD�A?��A<��A9}�A6��A3�AA.g�A,��A+��A)��A&.IA$�7A#c A"�LA!�A˒A��AMjA�$A  A��A	�A�AݘAI�A�4A!<�A!A ��A!��A#�3A%C-A$|�A#DgA#\�A"�A!rGA!-A��A�
AAU2AE9A�EAXyAA�AOvA�XA(�AD�A��A?A�)A\�A��Ae,A�AL�ASA�|A+kA�yA�AA��A� Aw2A�.A��A.�AѷA,=A�Ac�A
��A
�A	7LA�_AV�A�Ai�A��A>BA�A�"Au�A��AMAh
AW?A��A6�AbA �DA �ZA Z�A S�A �A �A ($@�j�@�,�@��?@�{@�=�@��@�]d@��W@��@��c@��D@��[@��2@���@�H@��@���@�>�@�@���@��@�y�@��@�@�*�@�@�w�@��@�/�@���@��@@�7�@�(�@���@���@�V@��Z@�	@��@莊@��@���@�q�@�q@��|@��M@�e@�=@�J�@��@�8�@�\�@ޜx@�O@ݮ@��8@���@ۗ$@�@�C-@�2a@آ4@��]@�6z@։�@���@�j�@��@��@�6z@ҽ<@�($@�u�@�ߤ@В�@�˒@�0�@�|�@��@�e�@̶�@��@���@ː�@�Y�@��@ʁo@��@�G�@��@��@ȔF@�W�@�@�@�<�@Ǯ@�҉@Ʈ}@ƌ@�tT@�Ta@�e@�G@���@�}�@��s@ę1@�PH@Ñh@°!@�!�@��@��@��@�|@� i@�}V@���@�1�@�/@��6@�?�@�  @���@�(�@�33@��j@�E�@�Vm@���@�U2@�Ta@���@���@�@�@��}@�x@�H�@�0�@��@��6@�&�@��
@�iD@��y@��@��1@�q�@�˒@��@�RT@���@�?�@�qv@���@�x�@�S�@�%F@�	l@��@��L@�kQ@���@��j@��)@��K@�˒@��[@�o�@�ی@���@��)@��]@�/�@�W?@��~@�a@��@�� @�[�@��@�S&@� \@��v@�Z�@�G@���@�rG@�L�@���@��@�N�@��@���@���@�e�@�-w@��@��1@�;�@�b@���@�Vm@��@��@�l�@�5?@��@���@�e�@��@��A@�.�@�(�@�$�@���@�>�@�@��@��@�+k@�}�@�+@��@��}@�n�@���@�J#@��@��@��@��@�n�@�I�@�=q@�#:@��W@���@�?}@�	l@��@��@�6@�-@��t@�e,@��@���@�m�@�I�@�*�@��n@�[W@�/�@��z@�Q@��@��)@���@�N<@��@��P@�~�@��@��z@���@��P@�Mj@�֡@��+@�~@���@�f�@�A @��@��h@���@���@�]d@���@���@�}�@�/@�%@���@���@��o@�J@��d@��k@�n/@�(�@���@��@��h@���@�_@��@���@�8�@���@���@�}V@�a|@�C�@��&@��C@���@��	@�a@��R@�m�@�<�@�	@��@��@�<6@��B@��j@��_@�V�@��D@��a@��k@���@���@��@�]�@��@�҉@��U@���@�i�@�1@���@��9@��*@���@�|�@�c�@�?}@��@��p@��4@���@�Ov@�@=@~��@~@}��@}�@}L�@|�K@|��@|/�@{��@{t�@{W?@{�@z�@z�@z&�@y�X@yp�@y \@xV�@w�	@v�m@v_�@u�@u�h@uS&@t�v@t/�@s�@s]�@r��@rȴ@r��@r�@r1�@q��@q=�@p��@p��@p�@o~�@o�@nZ�@m�t@mj@m@lH@k��@kiD@kF�@j��@jR�@i�)@i��@i}�@i�@h�@g�@f�@f)�@e�t@d�)@d�@c�@c|�@c)_@c�@b�X@b�m@b�R@b��@b�@b($@b@a��@aA @`��@`��@`H@`�@_��@_ƨ@_�F@_��@_=@_
=@^�@^u%@^J@]�T@]@]�@]}�@]+�@\��@[�Q@[��@[iD@Z�2@Z{@X�|@X��@Xz�@Xoi@X�@W��@W�@V�@U��@U��@Ue,@U�@T��@T"h@S�q@R�@R3�@Q�Z@Q��@Q�n@QO�@Q#�@P�`@PĜ@P(�@O{J@O�@N��@NR�@N-@N	@M��@MA @L�U@Lj@L*�@L�@K��@KRT@J�8@J�@J~�@Jl�@JZ�@J�@I�@I��@IX@I	l@H�@H��@He�@Hb@G�@G�Q@G��@G,�@F��@FOv@F{@E8�@D��@D��@DbN@D@C�m@C�*@CiD@CF�@C'�@C(@B�<@Bh
@A�^@Ahs@A[W@As�@AT�@Aq@@�E@@M@?��@?S�@?@>��@>��@>_@=��@=a�@==�@<�@<c�@<A�@;�}@;+@:��@:�@:�}@9�T@9�d@9�@9(�@8V�@81@7��@7��@7��@7S�@7@6�@6��@6q�@5�@5?}@4��@4�@4N�@4!@3�f@3,�@2��@2h
@2&�@1��@1`B@1�@0��@0ی@0�9@0:�@/a@.ߤ@.�@-��@-��@,�@,��@,u�@,<�@, �@+�6@+�@+1�@*�b@*=q@*�@)�N@)��@)�"@)-w@(�`@(��@(��@(|�@(Xy@(7@'�W@'�F@'��@'K�@'.I@'o@&�@&��@&V@&!�@%��@%�3@%�'@%u�@%\�@%0�@$��@$��@$��@$��@$V�@$~@#�&@#��@#�@#\)@"�8@"�<@"H�@"@!ԕ@!�@!�h@!:�@ ��@ �j@ ��@ oi@ D�@�W@�*@��@H�@&@S@��@R�@!�@�9@�=@a�@L�@2a@�@��@��@�Y@_@M@1'@�@�@_p@@O@�@�R@{�@Ov@C�@B[@	@�@�@��@��@p�@�@֡@�_@K^@1@��@33@(@�m@�F@@�@!�@{@	@ϫ@��@N<@�@�@��@��@�@q@V�@7�@x@�A@�;@� @t�@�@S@��@�x@��@C�@��@�#@�@�~@^�@?}@@@�U@��@`�@Q�@4n@�@��@��@Z�@�@��@^5@_@�^@�^@�H@��@?}@�@�@�[@��@�O@�@��@|�@PH@-�@"h@1'@$@��@��@�$@��@y�@n/@a@Mj@,�@(@
�M@
�2@
�@
��@
��@
xl@
:*@
�@	��@	�@	��@	f�@	B�@	�@	%@��@��@��@ѷ@��@�@j@Ft@2�@��@�Q@�@�q@��@j�@H�@)_@�@�@�B@�X@�@�\@l�@Ov@	@
�@�@�t@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BMB�BMBBB�B�BB�B%BmB�B�B�B�B%B?B?B%B%BYB�BtBB�B%BBB�B�BB
ңB
�(B
��B
��B
żB
ּB��BpBE�Bv+Bf�BCGB�B��B&�B1�B
BxBB��B��B�}B��B͹B�zB��B�tBpUB`vBK�B>]B+�B!B�B
�iB
��B
�-B
�	B
xRB
cnB
D�B
+�B
�B
mB
�B	�(B	ںB	�:B	͟B	�tB	��B	��B	�QB	�'B	�;B	TFB	9�B	;B	@B	 �B�B�B�`BߊB��B�B��BԯB��B��B�B�NB�?B�mB�)B��B�2B��Bx�Bw�Br�Bp;Bo BmBj�Bj�BkBlqBr�BvBxRB��B�eB	�B	1B	�B	�B	B[B	dZB	kB	g�B	}<B	�HB	�UB	V�B	=�B	4�B	HB	Q�B	[�B	`�B	kkB	vB	v+B	z�B	�:B	��B	��B	��B	��B	�tB	�TB	�*B	�mB	��B	��B	��B	��B	��B	��B	�0B	�*B	�B	��B	�-B	ǔB	ɺB	�B	ŢB	ƎB	�MB	��B	��B	��B	�B	��B	ªB	�B	�aB	B	�[B	��B	��B	��B	��B	�B	�	B	�+B	��B	��B	�B	�BB	�HB	��B	�?B	�1B	�lB	�DB	˒B	�0B	��B	�dB	�JB	ˬB	��B	ΥB	��B	�\B	�BB	�BB	�vB	ϫB	��B	�vB	��B	��B	бB	уB	�"B	̈́B	�jB	�BB	ҽB	�B	�oB	��B	�TB	�oB	ңB	ҽB	�B	��B	�[B	��B	�,B	ԕB	��B	�B	�gB	�,B	�FB	ӏB	�@B	� B	��B	��B	��B	ϫB	�B	��B	уB	ѷB	�hB	�oB	��B	҉B	��B	��B	ӏB	�@B	�B	ՁB	�B	�B	�yB	�yB	�+B	��B	�kB	�=B	یB	�B	�xB	��B	�5B	��B	޸B	�;B	�bB	��B	�B	��B	�B	�4B	�:B	�B	�B	�&B	�B	�zB	��B	��B	�B	�B	�eB	�B	�B	�"B	�B	�B	��B	�B	��B	��B	��B	��B	�?B	�nB	�B	�B	�B	�TB	�TB	�B	��B	�B	�'B	�AB	��B	��B	�5B	�B	�B	�B	�B	�yB	�B	��B	�B	�CB	�UB	�TB	�B	�%B	��B	�B	�FB	�+B	��B	�B	�`B	��B	��B	��B	��B	��B	�B	�DB	�B	��B	�>B	��B	�tB	�FB	��B	�RB	��B	��B	��B	��B	��B	�RB	��B	�rB	��B	��B	��B	�"B	�(B
 OB
�B
B
?B
	�B
�B
�B
~B
<B
vB
�B
�B
�B
HB
�B
 B
�B
NB
B
�B
TB
�B
�B
B
uB
�B
�B
aB
aB
FB
�B
�B
{B
B
B
2B
2B
�B
B
MB
9B
9B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
$B
�B
�B
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
�B
B
�B
�B
eB
B
1B
�B
�B
�B
�B
	B
#B
	B
�B
�B
�B
�B
�B
B
/B
~B
B
B
B
B
pB
�B
�B
�B
 B
 �B
 �B
!bB
!�B
!�B
!�B
!�B
"4B
"4B
"B
"�B
#TB
#�B
#�B
$B
$&B
$�B
$�B
$�B
$�B
%FB
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'B
'�B
'�B
(>B
(XB
(�B
(�B
(�B
(�B
)_B
)yB
)yB
)yB
)yB
*KB
*B
*�B
*�B
+B
+B
,qB
-)B
-]B
-�B
-�B
-�B
.cB
/ B
/B
/5B
/OB
/�B
0!B
0�B
0�B
0�B
1�B
2aB
2aB
2|B
2�B
2�B
2aB
2|B
2|B
2�B
3�B
4B
4B
4�B
5B
5?B
5�B
5�B
5�B
5�B
6FB
6zB
6�B
6�B
7B
7fB
72B
7�B
7�B
7�B
7�B
8�B
8lB
8�B
9$B
9�B
9�B
9�B
:*B
:DB
:�B
:�B
;0B
;0B
;0B
;B
;dB
;dB
;dB
;dB
;B
;dB
;B
;�B
;�B
;�B
;�B
<jB
<�B
<�B
=B
=�B
=�B
=qB
=VB
=�B
=�B
>]B
>]B
>BB
>�B
>BB
>�B
>�B
>�B
>�B
?�B
?�B
?�B
A;B
A B
A B
A�B
B�B
C�B
ESB
EmB
FB
E�B
F?B
FtB
F�B
F�B
GB
G+B
G_B
G_B
G_B
GzB
G�B
G�B
HB
G�B
H1B
HKB
HKB
HfB
HfB
H�B
I�B
J=B
J#B
J	B
I�B
I�B
H�B
H�B
H�B
H�B
IlB
I�B
J	B
JrB
K^B
K�B
K�B
K�B
LB
L~B
MB
NB
N�B
N�B
OB
OvB
O�B
O�B
O�B
O�B
PbB
P�B
QNB
Q�B
Q�B
Q�B
RB
RoB
R�B
S@B
S�B
S�B
S�B
S�B
T�B
T�B
UMB
U�B
U�B
U�B
VSB
VSB
VB
VB
VSB
VmB
V�B
V�B
WsB
W�B
W�B
XB
XEB
X�B
X�B
YB
ZB
ZB
Z�B
Z�B
ZQB
ZB
Z�B
Z�B
[#B
[=B
[=B
[=B
[=B
Z�B
Z7B
Z�B
[#B
[�B
[�B
\)B
\)B
\B
\CB
[�B
\B
[�B
\xB
\�B
]/B
^jB
^�B
_B
^�B
^�B
^�B
^�B
_!B
_!B
_!B
_�B
`'B
`BB
aB
aHB
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cTB
c B
c�B
c�B
d@B
d�B
ezB
ezB
e�B
e�B
fB
fB
f�B
f�B
f�B
f�B
f�B
gB
gB
gRB
h
B
h�B
h�B
i*B
h�B
i*B
h�B
iB
i�B
i�B
jeB
jKB
i�B
jKB
j�B
j�B
k�B
lWB
lWB
l�B
l�B
mB
mB
l�B
l�B
m)B
mCB
nB
m�B
n/B
n}B
n�B
oiB
o�B
pB
pUB
poB
p�B
qB
q'B
q[B
qvB
q�B
q�B
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tTB
t�B
t�B
u%B
u?B
uZB
uZB
u�B
u�B
vB
v+B
v+B
v+B
vFB
v`B
v`B
v+B
v`B
v`B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
xlB
x�B
xlB
x�B
y>B
yXB
yXB
y�B
y�B
z*B
z*B
z*B
zB
z�B
z�B
z�B
{0B
{dB
{B
{dB
{�B
{�B
{�B
|B
|B
|6B
|6B
|6B
|jB
|�B
|�B
}VB
}<B
}qB
~BB
~BB
~wB
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�4B
��B
��B
��B
�B
�UB
��B
��B
�AB
��B
��B
��B
�-B
�aB
�aB
�GB
�-B
�-B
�-B
�-B
�-B
�-B
��B
��B
��B
�B
��B
��B
��B
�gB
�gB
�gB
�gB
��B
�gB
��B
��B
�B
�9B
�9B
�mB
��B
��B
��B
��B
��B
�%B
�YB
��B
��B
��B
��B
�B
�B
��B
�+B
�_B
�EB
��B
��B
��B
��B
�KB
�KB
�1B
�KB
�fB
��B
��B
�B
�B
�RB
�RB
��B
��B
��B
��B
�=B
�#B
�rB
�rB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BMB�BMBBB�B�BB�B%BmB�B�B�B�B%B?B?B%B%BYB�BtBB�B%BBB�B�BB
ңB
�(B
��B
��B
żB
ּB��BpBE�Bv+Bf�BCGB�B��B&�B1�B
BxBB��B��B�}B��B͹B�zB��B�tBpUB`vBK�B>]B+�B!B�B
�iB
��B
�-B
�	B
xRB
cnB
D�B
+�B
�B
mB
�B	�(B	ںB	�:B	͟B	�tB	��B	��B	�QB	�'B	�;B	TFB	9�B	;B	@B	 �B�B�B�`BߊB��B�B��BԯB��B��B�B�NB�?B�mB�)B��B�2B��Bx�Bw�Br�Bp;Bo BmBj�Bj�BkBlqBr�BvBxRB��B�eB	�B	1B	�B	�B	B[B	dZB	kB	g�B	}<B	�HB	�UB	V�B	=�B	4�B	HB	Q�B	[�B	`�B	kkB	vB	v+B	z�B	�:B	��B	��B	��B	��B	�tB	�TB	�*B	�mB	��B	��B	��B	��B	��B	��B	�0B	�*B	�B	��B	�-B	ǔB	ɺB	�B	ŢB	ƎB	�MB	��B	��B	��B	�B	��B	ªB	�B	�aB	B	�[B	��B	��B	��B	��B	�B	�	B	�+B	��B	��B	�B	�BB	�HB	��B	�?B	�1B	�lB	�DB	˒B	�0B	��B	�dB	�JB	ˬB	��B	ΥB	��B	�\B	�BB	�BB	�vB	ϫB	��B	�vB	��B	��B	бB	уB	�"B	̈́B	�jB	�BB	ҽB	�B	�oB	��B	�TB	�oB	ңB	ҽB	�B	��B	�[B	��B	�,B	ԕB	��B	�B	�gB	�,B	�FB	ӏB	�@B	� B	��B	��B	��B	ϫB	�B	��B	уB	ѷB	�hB	�oB	��B	҉B	��B	��B	ӏB	�@B	�B	ՁB	�B	�B	�yB	�yB	�+B	��B	�kB	�=B	یB	�B	�xB	��B	�5B	��B	޸B	�;B	�bB	��B	�B	��B	�B	�4B	�:B	�B	�B	�&B	�B	�zB	��B	��B	�B	�B	�eB	�B	�B	�"B	�B	�B	��B	�B	��B	��B	��B	��B	�?B	�nB	�B	�B	�B	�TB	�TB	�B	��B	�B	�'B	�AB	��B	��B	�5B	�B	�B	�B	�B	�yB	�B	��B	�B	�CB	�UB	�TB	�B	�%B	��B	�B	�FB	�+B	��B	�B	�`B	��B	��B	��B	��B	��B	�B	�DB	�B	��B	�>B	��B	�tB	�FB	��B	�RB	��B	��B	��B	��B	��B	�RB	��B	�rB	��B	��B	��B	�"B	�(B
 OB
�B
B
?B
	�B
�B
�B
~B
<B
vB
�B
�B
�B
HB
�B
 B
�B
NB
B
�B
TB
�B
�B
B
uB
�B
�B
aB
aB
FB
�B
�B
{B
B
B
2B
2B
�B
B
MB
9B
9B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
$B
�B
�B
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
�B
B
�B
�B
eB
B
1B
�B
�B
�B
�B
	B
#B
	B
�B
�B
�B
�B
�B
B
/B
~B
B
B
B
B
pB
�B
�B
�B
 B
 �B
 �B
!bB
!�B
!�B
!�B
!�B
"4B
"4B
"B
"�B
#TB
#�B
#�B
$B
$&B
$�B
$�B
$�B
$�B
%FB
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'B
'�B
'�B
(>B
(XB
(�B
(�B
(�B
(�B
)_B
)yB
)yB
)yB
)yB
*KB
*B
*�B
*�B
+B
+B
,qB
-)B
-]B
-�B
-�B
-�B
.cB
/ B
/B
/5B
/OB
/�B
0!B
0�B
0�B
0�B
1�B
2aB
2aB
2|B
2�B
2�B
2aB
2|B
2|B
2�B
3�B
4B
4B
4�B
5B
5?B
5�B
5�B
5�B
5�B
6FB
6zB
6�B
6�B
7B
7fB
72B
7�B
7�B
7�B
7�B
8�B
8lB
8�B
9$B
9�B
9�B
9�B
:*B
:DB
:�B
:�B
;0B
;0B
;0B
;B
;dB
;dB
;dB
;dB
;B
;dB
;B
;�B
;�B
;�B
;�B
<jB
<�B
<�B
=B
=�B
=�B
=qB
=VB
=�B
=�B
>]B
>]B
>BB
>�B
>BB
>�B
>�B
>�B
>�B
?�B
?�B
?�B
A;B
A B
A B
A�B
B�B
C�B
ESB
EmB
FB
E�B
F?B
FtB
F�B
F�B
GB
G+B
G_B
G_B
G_B
GzB
G�B
G�B
HB
G�B
H1B
HKB
HKB
HfB
HfB
H�B
I�B
J=B
J#B
J	B
I�B
I�B
H�B
H�B
H�B
H�B
IlB
I�B
J	B
JrB
K^B
K�B
K�B
K�B
LB
L~B
MB
NB
N�B
N�B
OB
OvB
O�B
O�B
O�B
O�B
PbB
P�B
QNB
Q�B
Q�B
Q�B
RB
RoB
R�B
S@B
S�B
S�B
S�B
S�B
T�B
T�B
UMB
U�B
U�B
U�B
VSB
VSB
VB
VB
VSB
VmB
V�B
V�B
WsB
W�B
W�B
XB
XEB
X�B
X�B
YB
ZB
ZB
Z�B
Z�B
ZQB
ZB
Z�B
Z�B
[#B
[=B
[=B
[=B
[=B
Z�B
Z7B
Z�B
[#B
[�B
[�B
\)B
\)B
\B
\CB
[�B
\B
[�B
\xB
\�B
]/B
^jB
^�B
_B
^�B
^�B
^�B
^�B
_!B
_!B
_!B
_�B
`'B
`BB
aB
aHB
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cTB
c B
c�B
c�B
d@B
d�B
ezB
ezB
e�B
e�B
fB
fB
f�B
f�B
f�B
f�B
f�B
gB
gB
gRB
h
B
h�B
h�B
i*B
h�B
i*B
h�B
iB
i�B
i�B
jeB
jKB
i�B
jKB
j�B
j�B
k�B
lWB
lWB
l�B
l�B
mB
mB
l�B
l�B
m)B
mCB
nB
m�B
n/B
n}B
n�B
oiB
o�B
pB
pUB
poB
p�B
qB
q'B
q[B
qvB
q�B
q�B
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tTB
t�B
t�B
u%B
u?B
uZB
uZB
u�B
u�B
vB
v+B
v+B
v+B
vFB
v`B
v`B
v+B
v`B
v`B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
xlB
x�B
xlB
x�B
y>B
yXB
yXB
y�B
y�B
z*B
z*B
z*B
zB
z�B
z�B
z�B
{0B
{dB
{B
{dB
{�B
{�B
{�B
|B
|B
|6B
|6B
|6B
|jB
|�B
|�B
}VB
}<B
}qB
~BB
~BB
~wB
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�4B
��B
��B
��B
�B
�UB
��B
��B
�AB
��B
��B
��B
�-B
�aB
�aB
�GB
�-B
�-B
�-B
�-B
�-B
�-B
��B
��B
��B
�B
��B
��B
��B
�gB
�gB
�gB
�gB
��B
�gB
��B
��B
�B
�9B
�9B
�mB
��B
��B
��B
��B
��B
�%B
�YB
��B
��B
��B
��B
�B
�B
��B
�+B
�_B
�EB
��B
��B
��B
��B
�KB
�KB
�1B
�KB
�fB
��B
��B
�B
�B
�RB
�RB
��B
��B
��B
��B
�=B
�#B
�rB
�rB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220930094653  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220930094720  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220930094720  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220930094720                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220930184725  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220930184725  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220930100111                      G�O�G�O�G�O�                