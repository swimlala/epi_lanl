CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-08-11T00:35:21Z creation;2017-08-11T00:35:24Z conversion to V3.1;2019-12-19T08:00:23Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170811003521  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_148                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�u��� 1   @�vDDD�@4 ě��T�d�S&�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@333@�  @���@���AffA<��A^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B(  B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3D y�D ��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dй�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D�  D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�p�A�v�A�p�A�n�A�p�A�r�A�p�A�l�A�jA�jA�hsA�ffA�dZA�bNA�\)A�M�A��A�ȴA�I�A��A؛�A�AԃA��A�z�A�l�A�9XA�"�Aɕ�A��;A��A��AÙ�A�|�A�5?A�&�A�`BA��A�VA��HA�%A��`A� �A��A���A�`BA�t�A��
A���A�bNA�x�A�1A�VA��yA��wA��A��^A��9A�bNA�bA�A��FA���A��A���A�A���A�r�A��/A��^A�XA�jA��wA�33A��#A��\A�oA�x�A��!A�&�A��A��A�dZA�ZA��A�%A��A��A�1A�dZA���A��A�9XA���A��A�z�A�K�A�bA�&�A�I�A��A��;A�hsA��A���A��DA�oA}t�A|I�AzĜAx~�Aw�;Av��As�An�Ak&�AhbAe��Ad��Ac�-Abr�A`A�A_;dA^-A\��A[�;AY�hAU�ATĜAS��AR-AQ�wAQXAO��AM�TAKt�AJv�AIhsAH=qAG�AF�AE7LACG�AA��A@��A?�A>��A=�A<5?A:��A933A8-A7�hA5
=A3|�A2��A1t�A0v�A/�A/+A-�TA-��A-`BA,�A+C�A)dZA'�#A'A&�A%\)A$ffA"z�A ffA��A��A��AAƨA�AA�A��A�A1A&�A��Ax�A��AƨAhsA7LAoA�AffA�hA��A��A�^AVA{A?}A9XA�wA\)A	�A	��A�A��Ap�A�;A+Ax�A�RA��A�At�A�A|�AV@���@�&�@�Ĝ@�C�@��@���@�@�A�@���@���@��@��@�P@�h@�C�@�-@�@�;d@�/@ܣ�@�b@�S�@ڰ!@���@���@��@�=q@�{@�@��@��@١�@�r�@��;@���@�p�@�%@��@�+@�%@���@�Ĝ@�(�@�@�I�@�(�@��@���@���@��m@ǍP@�l�@��m@�b@�  @�p�@�"�@§�@��@��@�Q�@���@���@�@���@�O�@�j@�o@�J@��@�(�@���@�v�@�@��#@�x�@�Ĝ@��@���@���@�V@��^@�&�@�&�@�7L@�?}@�7L@��@�z�@��@���@�ff@�{@��T@��^@���@��@�p�@�O�@�G�@�?}@�/@��@��9@��@��;@�l�@�33@�
=@��@���@��+@�M�@�{@�G�@�V@��/@��@��@���@�z�@��@��;@�
=@��H@�ȴ@��!@�v�@�M�@��@�X@�?}@�&�@�%@��@��@�Z@�(�@���@���@�C�@��@���@��!@�~�@�-@���@��-@���@��h@�p�@��@��/@��j@�r�@�ƨ@�l�@�K�@�C�@�33@�o@���@���@��@���@���@���@�7L@���@��@�Q�@��@��m@��@�\)@��@��\@�V@���@�@���@�`B@�7L@�&�@��@���@��`@�Ĝ@��9@��D@�bN@�Q�@�1'@�1@��F@�t�@�;d@�@���@�M�@�5?@�@���@��^@���@���@��7@��@��@�`B@�7L@��@���@��@���@��@�Z@�1'@��@�  @��
@��w@��@��R@�~�@�-@�J@��T@��h@��9@��D@��@�z�@�A�@� �@��@�b@��w@��P@��@�l�@�;d@�
=@��y@���@��R@��!@���@�ff@�@��-@�`B@�&�@�V@���@���@�r�@�A�@�1@���@���@�;d@��@��@���@�$�@��@���@���@�p�@�X@�X@�V@���@���@��`@��9@��@�j@� �@�ƨ@��P@�C�@���@��!@��+@�V@���@��-@���@�x�@�hs@���@��j@��9@��D@�Q�@� �@��@�S�@�o@�@��@��@���@��R@��\@�^5@�V@�E�@��@���@�`B@�7L@��`@��j@�j@�A�@�  @;d@~�+@~@}`B@|�@{��@{ƨ@{�F@{�@{"�@z�@z��@y�#@x�u@w�P@w�@v��@v�y@vȴ@v�+@vV@v$�@v{@v@u��@up�@t�@t9X@st�@sdZ@s33@r��@r~�@r^5@rM�@r�@r�@rJ@q�#@q�@p��@pQ�@p1'@o�@o\)@n��@m�h@l�/@l�D@lI�@l9X@l�@l1@k��@kS�@j�!@i�@i�^@i&�@h1'@g�w@g\)@fȴ@fE�@e@e�@d�/@d�j@dj@c�m@c�@cdZ@cS�@cC�@b��@a��@a�7@ahs@a%@`Ĝ@`bN@` �@_|�@^ȴ@^�+@^V@^@]�T@]@\�@\�@\��@\Z@\9X@[��@[��@[dZ@[C�@Z�!@Z=q@Z�@Y��@Y�@Y�@Y��@Yhs@YX@YG�@Y%@XĜ@X�@W�w@Wl�@W\)@WK�@W
=@V�+@V$�@U��@U�-@Up�@U?}@U�@T��@T��@T�D@TI�@T�@S�
@SS�@R�!@R=q@Q�@Q��@Q&�@P��@P��@Pr�@PA�@Pb@O�;@O��@O;d@N�y@Nv�@N@M�@Mp�@M�@L��@LZ@L1@K�@KdZ@K33@Ko@J�H@J��@Jn�@J^5@J=q@J-@JJ@I�@I�#@I�7@IG�@I%@H�`@H�`@H�`@H�9@HQ�@H  @G�w@G��@G|�@GK�@F��@F�+@F$�@F{@F{@E�@E@E�@D�@D��@D�@D�D@D�@D1@C��@CS�@CS�@CC�@C33@C"�@B�@B��@B=q@A��@A�^@A��@A�7@A�7@A�7@Ax�@AX@A&�@A%@@�u@@1'@?K�@?+@?+@?
=@>ȴ@>V@>{@=�@=��@=�h@=/@<z�@;�F@;dZ@;"�@;@:��@:n�@:J@9�^@9��@9�7@9�7@9x�@9hs@9hs@9X@9X@9G�@9%@8Ĝ@8bN@8 �@7�P@6�+@5�T@5�h@4��@4�@3�m@3��@3t�@333@2-@1�^@1x�@1G�@1&�@1�@1%@0�`@0�u@0 �@0b@/�;@/;d@/
=@.�@.�R@.��@.�+@.5?@-��@-/@,�/@,�@,j@,(�@+��@+�F@+��@+��@+dZ@+33@+@*��@*^5@*�@)��@)�#@)�^@)��@)��@)G�@(��@(��@(r�@(bN@(bN@(A�@(  @'�P@';d@&�y@&��@&ff@&5?@&@%�-@%�h@%`B@%/@%/@%V@$��@$�@$��@$�D@$j@$j@$1@#��@#t�@#dZ@#S�@#C�@#33@#"�@#o@"�@"�H@"^5@"�@!��@!�@!�@!��@!�^@!��@!��@!�7@!x�@!G�@ ��@ r�@ Q�@ Q�@ A�@ 1'@   @�;@��@|�@;d@+@�@�@V@E�@5?@@�T@��@�@�@�j@�j@�@��@I�@9X@(�@(�@�@1@�
@��@�@C�@�H@��@��@M�@J@�#@��@�^@X@�@��@�9@r�@1'@�@��@�P@\)@+@��@�R@��@��@@�h@�@O�@/@�@��@��@�j@�@��@z�@j@I�@9X@(�@1@�
@��@��@��@�@�@�@t�@dZ@dZ@S�@@��@�!@�\@n�@^5@-@�@��@��@x�@&�@�`@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�p�A�v�A�p�A�n�A�p�A�r�A�p�A�l�A�jA�jA�hsA�ffA�dZA�bNA�\)A�M�A��A�ȴA�I�A��A؛�A�AԃA��A�z�A�l�A�9XA�"�Aɕ�A��;A��A��AÙ�A�|�A�5?A�&�A�`BA��A�VA��HA�%A��`A� �A��A���A�`BA�t�A��
A���A�bNA�x�A�1A�VA��yA��wA��A��^A��9A�bNA�bA�A��FA���A��A���A�A���A�r�A��/A��^A�XA�jA��wA�33A��#A��\A�oA�x�A��!A�&�A��A��A�dZA�ZA��A�%A��A��A�1A�dZA���A��A�9XA���A��A�z�A�K�A�bA�&�A�I�A��A��;A�hsA��A���A��DA�oA}t�A|I�AzĜAx~�Aw�;Av��As�An�Ak&�AhbAe��Ad��Ac�-Abr�A`A�A_;dA^-A\��A[�;AY�hAU�ATĜAS��AR-AQ�wAQXAO��AM�TAKt�AJv�AIhsAH=qAG�AF�AE7LACG�AA��A@��A?�A>��A=�A<5?A:��A933A8-A7�hA5
=A3|�A2��A1t�A0v�A/�A/+A-�TA-��A-`BA,�A+C�A)dZA'�#A'A&�A%\)A$ffA"z�A ffA��A��A��AAƨA�AA�A��A�A1A&�A��Ax�A��AƨAhsA7LAoA�AffA�hA��A��A�^AVA{A?}A9XA�wA\)A	�A	��A�A��Ap�A�;A+Ax�A�RA��A�At�A�A|�AV@���@�&�@�Ĝ@�C�@��@���@�@�A�@���@���@��@��@�P@�h@�C�@�-@�@�;d@�/@ܣ�@�b@�S�@ڰ!@���@���@��@�=q@�{@�@��@��@١�@�r�@��;@���@�p�@�%@��@�+@�%@���@�Ĝ@�(�@�@�I�@�(�@��@���@���@��m@ǍP@�l�@��m@�b@�  @�p�@�"�@§�@��@��@�Q�@���@���@�@���@�O�@�j@�o@�J@��@�(�@���@�v�@�@��#@�x�@�Ĝ@��@���@���@�V@��^@�&�@�&�@�7L@�?}@�7L@��@�z�@��@���@�ff@�{@��T@��^@���@��@�p�@�O�@�G�@�?}@�/@��@��9@��@��;@�l�@�33@�
=@��@���@��+@�M�@�{@�G�@�V@��/@��@��@���@�z�@��@��;@�
=@��H@�ȴ@��!@�v�@�M�@��@�X@�?}@�&�@�%@��@��@�Z@�(�@���@���@�C�@��@���@��!@�~�@�-@���@��-@���@��h@�p�@��@��/@��j@�r�@�ƨ@�l�@�K�@�C�@�33@�o@���@���@��@���@���@���@�7L@���@��@�Q�@��@��m@��@�\)@��@��\@�V@���@�@���@�`B@�7L@�&�@��@���@��`@�Ĝ@��9@��D@�bN@�Q�@�1'@�1@��F@�t�@�;d@�@���@�M�@�5?@�@���@��^@���@���@��7@��@��@�`B@�7L@��@���@��@���@��@�Z@�1'@��@�  @��
@��w@��@��R@�~�@�-@�J@��T@��h@��9@��D@��@�z�@�A�@� �@��@�b@��w@��P@��@�l�@�;d@�
=@��y@���@��R@��!@���@�ff@�@��-@�`B@�&�@�V@���@���@�r�@�A�@�1@���@���@�;d@��@��@���@�$�@��@���@���@�p�@�X@�X@�V@���@���@��`@��9@��@�j@� �@�ƨ@��P@�C�@���@��!@��+@�V@���@��-@���@�x�@�hs@���@��j@��9@��D@�Q�@� �@��@�S�@�o@�@��@��@���@��R@��\@�^5@�V@�E�@��@���@�`B@�7L@��`@��j@�j@�A�@�  @;d@~�+@~@}`B@|�@{��@{ƨ@{�F@{�@{"�@z�@z��@y�#@x�u@w�P@w�@v��@v�y@vȴ@v�+@vV@v$�@v{@v@u��@up�@t�@t9X@st�@sdZ@s33@r��@r~�@r^5@rM�@r�@r�@rJ@q�#@q�@p��@pQ�@p1'@o�@o\)@n��@m�h@l�/@l�D@lI�@l9X@l�@l1@k��@kS�@j�!@i�@i�^@i&�@h1'@g�w@g\)@fȴ@fE�@e@e�@d�/@d�j@dj@c�m@c�@cdZ@cS�@cC�@b��@a��@a�7@ahs@a%@`Ĝ@`bN@` �@_|�@^ȴ@^�+@^V@^@]�T@]@\�@\�@\��@\Z@\9X@[��@[��@[dZ@[C�@Z�!@Z=q@Z�@Y��@Y�@Y�@Y��@Yhs@YX@YG�@Y%@XĜ@X�@W�w@Wl�@W\)@WK�@W
=@V�+@V$�@U��@U�-@Up�@U?}@U�@T��@T��@T�D@TI�@T�@S�
@SS�@R�!@R=q@Q�@Q��@Q&�@P��@P��@Pr�@PA�@Pb@O�;@O��@O;d@N�y@Nv�@N@M�@Mp�@M�@L��@LZ@L1@K�@KdZ@K33@Ko@J�H@J��@Jn�@J^5@J=q@J-@JJ@I�@I�#@I�7@IG�@I%@H�`@H�`@H�`@H�9@HQ�@H  @G�w@G��@G|�@GK�@F��@F�+@F$�@F{@F{@E�@E@E�@D�@D��@D�@D�D@D�@D1@C��@CS�@CS�@CC�@C33@C"�@B�@B��@B=q@A��@A�^@A��@A�7@A�7@A�7@Ax�@AX@A&�@A%@@�u@@1'@?K�@?+@?+@?
=@>ȴ@>V@>{@=�@=��@=�h@=/@<z�@;�F@;dZ@;"�@;@:��@:n�@:J@9�^@9��@9�7@9�7@9x�@9hs@9hs@9X@9X@9G�@9%@8Ĝ@8bN@8 �@7�P@6�+@5�T@5�h@4��@4�@3�m@3��@3t�@333@2-@1�^@1x�@1G�@1&�@1�@1%@0�`@0�u@0 �@0b@/�;@/;d@/
=@.�@.�R@.��@.�+@.5?@-��@-/@,�/@,�@,j@,(�@+��@+�F@+��@+��@+dZ@+33@+@*��@*^5@*�@)��@)�#@)�^@)��@)��@)G�@(��@(��@(r�@(bN@(bN@(A�@(  @'�P@';d@&�y@&��@&ff@&5?@&@%�-@%�h@%`B@%/@%/@%V@$��@$�@$��@$�D@$j@$j@$1@#��@#t�@#dZ@#S�@#C�@#33@#"�@#o@"�@"�H@"^5@"�@!��@!�@!�@!��@!�^@!��@!��@!�7@!x�@!G�@ ��@ r�@ Q�@ Q�@ A�@ 1'@   @�;@��@|�@;d@+@�@�@V@E�@5?@@�T@��@�@�@�j@�j@�@��@I�@9X@(�@(�@�@1@�
@��@�@C�@�H@��@��@M�@J@�#@��@�^@X@�@��@�9@r�@1'@�@��@�P@\)@+@��@�R@��@��@@�h@�@O�@/@�@��@��@�j@�@��@z�@j@I�@9X@(�@1@�
@��@��@��@�@�@�@t�@dZ@dZ@S�@@��@�!@�\@n�@^5@-@�@��@��@x�@&�@�`@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BD�BC�BD�BD�BI�BL�BP�BS�BT�BT�BT�BT�BS�BR�BQ�BP�BL�BYBt�BjB_;BS�B=qBN�BR�BZBffBcTBe`BjBk�Bm�Bp�Bt�Bz�By�B�=B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B�uB�By�B~�B~�Bz�Bq�BbNBQ�BI�BH�B7LB�B�B%�BH�BL�BB�B&�BDB��B�B�B�B��BÖB�9B�B��B��B�7B|�Br�Be`BP�B9XB7LB.B�B1B
��B
�)B
��B
�3B
��B
�B
v�B
|�B
z�B
s�B
cTB
[#B
P�B
>wB
<jB
/B
�B	�B	��B	�!B	��B	��B	��B	��B	�uB	�bB	�7B	|�B	t�B	cTB	L�B	L�B	J�B	B�B	E�B	@�B	49B	%�B	�B	�B	{B	DB	+B	B��B�B�fB�TB�;B�
B��B��BÖBÖB�qB�XB��B�B�!B��B��B��B��B��B��B��B�hB�%B{�B� B~�B~�Bz�Bw�Bn�BiyBgmBiyBk�BiyBk�BgmBdZBe`BaHB_;B\)B]/BYBXBYB\)B]/B\)BZBVBQ�BR�BO�BT�BO�BH�BI�BE�BI�BG�B@�BG�BD�BB�B<jB5?B>wB49B:^B;dBB�BK�BZBgmBhsBiyBffBo�BjBaHBdZBhsBgmBdZB_;B\)BVBVB\)BT�BYBXBO�BP�B[#B\)B\)B_;BbNBm�Bu�B~�B�1B�DB�JB�DB�DB�7B�DB�=B�7B�oB�uB�JB�=B��B��B�uB�DB�\B�{B�uB�\B�PB�hB��B��B��B��B��B��B�{B��B��B��B�B�B�B�-B�XB��B�}B�wBBĜBǮB��B��B�B�B�)B�ZB�`B�fB�B��B��B��B	  B	%B	%B	1B	
=B	JB	VB	hB	�B	�B	�B	!�B	$�B	'�B	)�B	,B	/B	1'B	1'B	1'B	2-B	33B	5?B	:^B	?}B	A�B	C�B	E�B	E�B	F�B	I�B	H�B	N�B	P�B	Q�B	S�B	W
B	YB	ZB	]/B	]/B	dZB	e`B	e`B	e`B	gmB	iyB	jB	o�B	o�B	p�B	p�B	t�B	u�B	v�B	x�B	z�B	z�B	� B	�B	�B	�%B	�+B	�DB	�hB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�?B	�FB	�RB	�jB	�wB	�}B	��B	��B	ÖB	B	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�
B	�
B	�B	�B	�#B	�B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�NB	�NB	�NB	�NB	�HB	�ZB	�fB	�mB	�yB	�yB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
	7B
DB
JB
JB
JB
DB
PB
\B
VB
VB
VB
VB
\B
hB
oB
oB
oB
uB
oB
oB
oB
uB
uB
oB
hB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
 �B
!�B
!�B
"�B
!�B
"�B
"�B
!�B
 �B
!�B
"�B
#�B
#�B
"�B
"�B
"�B
%�B
&�B
'�B
(�B
(�B
(�B
'�B
'�B
&�B
'�B
)�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
.B
.B
.B
.B
/B
0!B
0!B
0!B
/B
.B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
49B
49B
49B
5?B
5?B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
6FB
7LB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
:^B
;dB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
=qB
>wB
?}B
?}B
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
D�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
I�B
I�B
L�B
L�B
L�B
L�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
O�B
O�B
O�B
O�B
O�B
N�B
P�B
Q�B
Q�B
R�B
Q�B
S�B
S�B
S�B
R�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
VB
W
B
XB
XB
W
B
XB
YB
YB
YB
YB
XB
XB
YB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
gmB
ffB
gmB
gmB
ffB
gmB
hsB
hsB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BD�BC�BD�BD�BI�BL�BP�BTBUBT�BT�BT�BT,BS&BRTBRBO�B[�Bu�Bm�BcnBY1BB�BRTBWYB^OBiBgBi*BnBn/BpoBsMBw�B}�B}VB��B�EB��B��B��B�OB��B�tB�XB��B�*B��B� B��B�B�fB��B��B��B�QB��B��B|PB�BcB{�Bs�Be�BU�BL~BK�B:xB!B�B&LBJ#BN�BFB+�B�B�.B�B�"BۦBӏB�?B��B��B��B�B��BBt�Bg�BT�B<�B8�B/iB!HBDB
�8B
��B
ŢB
��B
�dB
�+B
xRB
}VB
{�B
uZB
f2B
\�B
R�B
A B
=�B
1vB
�B	�B	�BB	��B	��B	�kB	�KB	��B	�B	��B	��B	~�B	v�B	f�B	QB	NB	LJB	D3B	FYB	A�B	6zB	(�B	xB	�B	�B	�B	�B	�B�B��B�$B��B��BخBөB��BżB�SB��B��B�B��B�vB��B�B��B��B��B�7B�EB��B��B~wB��B�OB�B|�By�BqABl"Bi�Bj�Bl�Bj�Bl"BhsBe�BfLBb�B`vB]~B^OBZ�BYKBZB\�B]�B\�BZ�BW
BS@BT,BQBUgBQ BJ=BJ�BGBJ�BH�BB[BHfBE�BC�B>BB7fB?�B6zB;�B<�BCBK�BZ7Bg�Bi_BkBh$Bp!Bk�Bb�Be,BjKBiBf�BabB]�BX+BW$B]~BV�BZBX�BQ�BRTB[�B\�B\�B_�Bb�Bm�Bu�BB�KB�^B�dB��B��B�#B��B�B�XB��B��B��B��B��B��B�FB��B�bB��B��B�bB��B� B��B��B��B�
B�mB�pB��B�CB��B�LB��B��B�B��B��B��B�iB�}B�aB�SB�fB�PBϫB�mB�kB��B��B�2B�B��B�B�FB�8B	 B	%B	YB	KB	
�B	�B	B	 B	�B	�B	�B	!�B	%B	(
B	*0B	,"B	/5B	1[B	1[B	1[B	2aB	3�B	5�B	:�B	?�B	A�B	C�B	E�B	E�B	GB	J	B	IRB	O(B	Q4B	R:B	TB	W$B	YeB	ZkB	]~B	]�B	dtB	ezB	ezB	e�B	g�B	i�B	kB	o�B	o�B	p�B	p�B	t�B	u�B	v�B	y$B	{B	{JB	�OB	�9B	�9B	�tB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	�	B	�IB	�B	�B	�0B	�0B	�B	�6B	�kB	�WB	�GB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�	B	��B	�B	��B	�B	�B	��B	�B	� B	�B	�B	�B	�,B	�B	�9B	�gB	�?B	�YB	�?B	�_B	�eB	�=B	�kB	�kB	�CB	�dB	�dB	�IB	�IB	�IB	�dB	�]B	�~B	�jB	�pB	�\B	�\B	�\B	�\B	�B	�hB	�B	�B	��B	�B	�B	�B	�B	�B	��B	�$B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�	B	�	B	�	B	�8B	�B	�0B	�B	�B	�PB	�"B	�.B	�HB	�wB
UB
AB
-B
[B
3B
3B
GB
9B
B
SB
mB
mB
9B
SB
�B
YB
_B
_B
fB
	�B
	�B
	�B
xB
dB
dB
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
!�B
 �B
 �B
!�B
!�B
"�B
!�B
"�B
#B
!�B
!-B
"B
#B
#�B
$B
#:B
#:B
#:B
&2B
'B
(
B
)B
)*B
)B
(>B
($B
'RB
(>B
*0B
)DB
)DB
*KB
+6B
+QB
+QB
,WB
,WB
.IB
.IB
.cB
.IB
/5B
0;B
0;B
0UB
/OB
.cB
0UB
1AB
1[B
1AB
1vB
2aB
2|B
2|B
4TB
4TB
4TB
5ZB
5tB
4�B
5tB
6`B
6zB
6`B
6zB
6�B
7�B
7�B
6zB
7�B
9rB
9rB
9rB
9rB
9rB
9rB
:xB
9�B
9�B
9�B
9�B
9�B
9�B
:xB
:xB
:�B
9�B
:�B
:�B
;B
;B
;B
;B
;B
;�B
:xB
;B
:�B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
=�B
=�B
>�B
>�B
=�B
>�B
?�B
?�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
D�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
I�B
J#B
L�B
L�B
L�B
L�B
LB
L�B
L�B
L�B
MB
LB
LB
LB
NB
OB
OB
OB
OB
N�B
O�B
Q B
Q B
P�B
P�B
Q B
RB
Q B
P�B
Q B
PB
PB
PB
P.B
PHB
O\B
Q4B
R:B
R:B
S&B
RTB
T,B
T,B
TFB
SuB
U2B
V9B
W?B
W?B
W$B
W$B
W?B
V9B
W?B
X+B
XEB
W?B
X+B
Y1B
Y1B
Y1B
YKB
XEB
X_B
YeB
ZQB
ZQB
[=B
[=B
[WB
[WB
\)B
\]B
\CB
\CB
\CB
\]B
\xB
\]B
]IB
]IB
^OB
^OB
]IB
]IB
]~B
^jB
_VB
_;B
_VB
_VB
^OB
^�B
_pB
_VB
`vB
`\B
`vB
a|B
a|B
abB
a|B
bhB
bNB
bhB
b�B
bhB
b�B
cnB
b�B
cnB
b�B
b�B
c�B
dtB
dtB
dZB
dtB
dZB
dtB
dtB
d�B
c�B
dtB
ezB
ezB
e`B
ezB
ezB
e`B
ezB
e`B
e�B
e�B
e�B
e�B
f�B
g�B
g�B
gmB
f�B
g�B
g�B
f�B
g�B
h�B
h�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
j�B
jB
jB
j�B
j�B
k�B
j�B
k�B
k�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.1(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708150034422017081500344220170815003442201806221317312018062213173120180622131731201804050719462018040507194620180405071946  JA  ARFMdecpA19c                                                                20170811093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170811003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170811003523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170811003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170811003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170811003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170811003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170811003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170811003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170811003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20170811005518                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170811153850  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20170811153850  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20170811153850  CV  LATITUDE        G�O�G�O�A��                JM  ARGQJMQC2.0                                                                 20170811153850  CV  LONGITUDE       G�O�G�O��$�7                JM  ARCAJMQC2.0                                                                 20170814153442  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170814153442  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221946  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041731  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                