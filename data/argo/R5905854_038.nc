CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:51:21Z creation;2022-06-04T17:51:21Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175121  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               &A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @���2�1   @��=��@0�fffff�c@�9Xb1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@���A��A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4L�C5�fC8  C:  C<  C>  C@  CB�CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Db��Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���A   AffA>ffA^ffA~ffA�33A�33A�33A�33A�ffA�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�fgB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���Bߙ�B㙚B���B���B���B���B���B���B���C�fC�fC�fC�fC
  C  C�fC�fC�fC��C�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC.  C/�fC1�fC433C5��C7�fC9�fC;�fC=�fC?�fCB  CC��CE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCn  Cp  Cq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3D y�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DE� DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DW  DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dbs4Db�4Dcs4Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�D{  D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�,qA�,�A�+�A�,qA�.IA�/�A�/A�/�A�/�A�/�A�1[A�0�A�"4A��A��A��A��A��A�hA��A�bA��A��A�	7A�
	A�
rA�A��A�DA��A�PA��A��A��A�7A�XyA�v�A���A��pA��tAȼ�AȦ�A�x�A�3�Aǝ�A�	lA�+kA�{�A�ȴA���A��HA��A���A�r|A�2-A���A���A���A�.}A�W?A�VA���A��A��A�A��A�XyA�6FA�4�A��[A�lWA��A���A���A�G�A���A��A��SA�(�A��:A�0!A�0�A���A�qA�GEA��A���A��"A�L0A���A��	A��A�k�A~��A|��At��AlĜAa��A\�\AX��AW@AR�eAN+�AJ�jAI��AI_AH�AF�|A@XyA=VA;YKA:� A:3�A:�A:�A:�A:(�A:'RA8�jA7�A6��A6
=A5
�A4\�A11A.�`A,҉A,!-A,VA,��A-�A/�[A/��A/l�A/��A/+kA.��A-y�A*FtA)}�A(v�A&�1A%oA$��A%4A%YA%�A#��A"OA ��A!Z�A!�A hsA0UA�Ai�Ae�Ac A��A($AA5?A�AdZA+�A~�A��A0�A�FA�FA��A�
A��Ac�A�A�!AAsA:*A��A�0A�nAOvA1�A$A�A	A��AK�A�oA��AffAK^AGA�A�mA�jA��A($A��A�AA}VA1�AA�A�<AOvAe,A(�A�A�A
h�A
0UA	��A	�A�4A#�AiDA�AdZA�XA$A�FA�-A��A�+A]dA6�A'RAI�A ��A �rA -A MA c A [�A <�A �@���@�!-@���@���@���@�_�@�\�@�<6@���@���@�9X@�{@���@���@���@�`B@��@�q�@��}@�3�@��@��@��@@��@�^5@�Ta@�@�֡@�tT@�q�@�j�@�V@��Z@�A@��@�1@�_p@�0U@�33@�ߤ@�\)@�%�@�|�@�v�@�&�@�Q�@ރ�@�&�@���@�;d@܉�@�A�@�bN@�X�@���@���@ٲ�@�@O@ڽ<@�k�@ٞ�@٣n@؍�@��@���@נ'@�j�@��@֕�@���@�^�@�_p@�RT@�6z@���@Ԓ�@��@��@�$�@ѳ�@�dZ@�(�@�w�@�ϫ@�v`@�a|@̏\@��@�1'@��&@ˋ�@�>�@ʺ�@�`�@�ԕ@�)_@�s�@��A@��6@��@Č@�R�@�GE@�h�@�(@��)@ĭ�@�C-@��@��@À4@� \@@��@�<6@���@��@�L0@� �@��K@��2@�V�@��@��@��@�\)@�@�@��6@��	@�k�@��@�c�@��@�o�@�b�@�b�@�Vm@��,@�N�@��d@���@���@�YK@��
@��*@���@�d�@�Z@��@�~�@�0�@��@�=q@��@�a@�͟@���@���@�U2@��@�J@��@���@�7L@��@��4@�~�@�(�@��@��'@�4@��M@���@���@���@���@�[�@���@�S&@�&�@���@���@��@��1@�{�@�M�@��)@���@���@�=�@��M@��9@��z@���@�S�@��o@��q@�A @��@���@���@���@�K^@��3@��h@�v`@�%F@��@��@�ߤ@���@�Q�@��@�u�@�<6@�&@�+@�
=@��M@��@���@�J@��#@���@���@�x@�a@�!-@��@�:�@�L�@��@���@�7@��A@�t�@�Y@��@��p@���@�
�@���@��@���@�9X@���@��~@�&@��@��M@��@�h�@�c @�|�@�}V@�#:@�ݘ@��@�@���@���@�#�@��y@�D�@���@���@�|@��@�҉@�^5@��@���@�'�@��F@�M�@�&�@�7@�_@��a@�zx@�/@���@���@���@�_�@�V�@�*�@��@���@���@�u�@�RT@�N<@�$t@���@�	@�˒@���@�c@�rG@�Q�@�'�@�V@���@���@��I@�l�@�-�@�u@��Q@���@�S�@�2a@�@��@���@��<@���@��+@�Xy@�!�@�خ@��F@���@��~@��f@��P@���@��'@��X@��@�a@�@��v@�֡@���@���@�l�@�YK@�/�@��A@��3@���@���@�"�@�o@��P@��}@�� @�j@�+k@�@��@��)@��@���@��@���@���@��,@��'@��<@���@��x@���@�c�@�4@���@��@�n/@�a@�A @�+@��@���@���@���@�{�@�2�@� �@�q@~�H@~:*@~_@}�o@}��@}#�@|��@|�j@|tT@|:�@{��@{�@z��@y�C@yIR@x�_@w��@w8@v�y@v�}@v� @vs�@vu@u�@uDg@t��@t��@tbN@s��@s��@s�@r��@r�L@r�1@r}V@qp�@p�5@p�o@o��@o�k@o�@oo�@o�@n��@n�}@n��@n��@n@l�K@l7@k�a@kY@j��@j8�@iϫ@i��@i�@i�h@i%F@h�@h��@h	�@g~�@f�H@f��@fn�@f�@e�S@e/@d�`@d�9@d��@dr�@d"h@d	�@cݘ@cC�@b�]@b	@a�@`��@`r�@`%�@_��@_��@_��@_�P@_4�@^�8@^�\@^)�@]7L@\�@\��@[�}@Z�F@Y��@X�.@W��@W�@Vߤ@Vv�@V8�@V�@V4@U�@U�z@U�=@U�@T�u@TG@S)_@R8�@Q�)@Q��@Q/@Q@@Q�@Q�@Q�@PɆ@O�@O�{@O�@N�r@M�@M&�@L�p@Lq@K�}@Kqv@KY@J��@Jv�@Jd�@Jp;@Ja|@JE�@J.�@Iԕ@I&�@H�.@Hb@G��@G9�@F�H@F��@F��@F^5@F0U@E�@EF@D�I@Dg8@D6@C��@Co�@B�B@Bq�@A�@AY�@@�@@�e@@�u@@c�@@,=@?�+@?��@?��@?t�@?e�@?+@>��@>\�@>�@=�z@=zx@=c�@=IR@=!�@=@<�@<�U@<�@< �@;�;@;1�@:�@:͟@:�R@:�A@:5?@9�z@9S&@9�@8�K@8��@8S�@7�]@7��@7�{@7@6�r@6H�@6�@5��@5��@5+@4�@4�_@3�g@3�V@3E9@31�@3�@3�@3@2ȴ@2�r@1��@1Dg@1�@0�@0�j@0�I@0��@0Z@/��@/�f@/iD@/C�@.�,@.#:@-�X@-N<@-�@,�@,��@,6@+�@+�	@+x@+a@+9�@+�@*��@)�@)|@)N<@(�@(��@(�Y@(�@'��@'{J@'o@&�@&a|@&=q@&	@&�@%�#@%rG@%O�@%/@%V@$��@$��@$��@$m�@$D�@$%�@#�@#�@#a@#@O@#�@"�8@"��@"	@!�@!�X@!�h@!�M@!s�@!F@!7L@!#�@ �	@ �@ �@ �U@ ��@ `�@ �@�g@�a@�@��@�*@�$@�$@�:@o�@a@Mj@$t@�@�B@��@i�@0U@�@�D@�T@�^@zx@O�@7L@q@�@��@��@~(@Xy@D�@9X@-�@�K@dZ@$t@ i@�8@��@�+@8�@��@��@�@��@��@7L@�p@��@�@V�@$@�@��@��@�$@_p@33@
=@�H@ȴ@-@�^@�7@N<@4@�U@z�@m�@Xy@H@9X@!@��@RT@Y@��@��@��@=q@3�@+k@-@$�@
�@�3@p�@0�@��@�9@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�,qA�,�A�+�A�,qA�.IA�/�A�/A�/�A�/�A�/�A�1[A�0�A�"4A��A��A��A��A��A�hA��A�bA��A��A�	7A�
	A�
rA�A��A�DA��A�PA��A��A��A�7A�XyA�v�A���A��pA��tAȼ�AȦ�A�x�A�3�Aǝ�A�	lA�+kA�{�A�ȴA���A��HA��A���A�r|A�2-A���A���A���A�.}A�W?A�VA���A��A��A�A��A�XyA�6FA�4�A��[A�lWA��A���A���A�G�A���A��A��SA�(�A��:A�0!A�0�A���A�qA�GEA��A���A��"A�L0A���A��	A��A�k�A~��A|��At��AlĜAa��A\�\AX��AW@AR�eAN+�AJ�jAI��AI_AH�AF�|A@XyA=VA;YKA:� A:3�A:�A:�A:�A:(�A:'RA8�jA7�A6��A6
=A5
�A4\�A11A.�`A,҉A,!-A,VA,��A-�A/�[A/��A/l�A/��A/+kA.��A-y�A*FtA)}�A(v�A&�1A%oA$��A%4A%YA%�A#��A"OA ��A!Z�A!�A hsA0UA�Ai�Ae�Ac A��A($AA5?A�AdZA+�A~�A��A0�A�FA�FA��A�
A��Ac�A�A�!AAsA:*A��A�0A�nAOvA1�A$A�A	A��AK�A�oA��AffAK^AGA�A�mA�jA��A($A��A�AA}VA1�AA�A�<AOvAe,A(�A�A�A
h�A
0UA	��A	�A�4A#�AiDA�AdZA�XA$A�FA�-A��A�+A]dA6�A'RAI�A ��A �rA -A MA c A [�A <�A �@���@�!-@���@���@���@�_�@�\�@�<6@���@���@�9X@�{@���@���@���@�`B@��@�q�@��}@�3�@��@��@��@@��@�^5@�Ta@�@�֡@�tT@�q�@�j�@�V@��Z@�A@��@�1@�_p@�0U@�33@�ߤ@�\)@�%�@�|�@�v�@�&�@�Q�@ރ�@�&�@���@�;d@܉�@�A�@�bN@�X�@���@���@ٲ�@�@O@ڽ<@�k�@ٞ�@٣n@؍�@��@���@נ'@�j�@��@֕�@���@�^�@�_p@�RT@�6z@���@Ԓ�@��@��@�$�@ѳ�@�dZ@�(�@�w�@�ϫ@�v`@�a|@̏\@��@�1'@��&@ˋ�@�>�@ʺ�@�`�@�ԕ@�)_@�s�@��A@��6@��@Č@�R�@�GE@�h�@�(@��)@ĭ�@�C-@��@��@À4@� \@@��@�<6@���@��@�L0@� �@��K@��2@�V�@��@��@��@�\)@�@�@��6@��	@�k�@��@�c�@��@�o�@�b�@�b�@�Vm@��,@�N�@��d@���@���@�YK@��
@��*@���@�d�@�Z@��@�~�@�0�@��@�=q@��@�a@�͟@���@���@�U2@��@�J@��@���@�7L@��@��4@�~�@�(�@��@��'@�4@��M@���@���@���@���@�[�@���@�S&@�&�@���@���@��@��1@�{�@�M�@��)@���@���@�=�@��M@��9@��z@���@�S�@��o@��q@�A @��@���@���@���@�K^@��3@��h@�v`@�%F@��@��@�ߤ@���@�Q�@��@�u�@�<6@�&@�+@�
=@��M@��@���@�J@��#@���@���@�x@�a@�!-@��@�:�@�L�@��@���@�7@��A@�t�@�Y@��@��p@���@�
�@���@��@���@�9X@���@��~@�&@��@��M@��@�h�@�c @�|�@�}V@�#:@�ݘ@��@�@���@���@�#�@��y@�D�@���@���@�|@��@�҉@�^5@��@���@�'�@��F@�M�@�&�@�7@�_@��a@�zx@�/@���@���@���@�_�@�V�@�*�@��@���@���@�u�@�RT@�N<@�$t@���@�	@�˒@���@�c@�rG@�Q�@�'�@�V@���@���@��I@�l�@�-�@�u@��Q@���@�S�@�2a@�@��@���@��<@���@��+@�Xy@�!�@�خ@��F@���@��~@��f@��P@���@��'@��X@��@�a@�@��v@�֡@���@���@�l�@�YK@�/�@��A@��3@���@���@�"�@�o@��P@��}@�� @�j@�+k@�@��@��)@��@���@��@���@���@��,@��'@��<@���@��x@���@�c�@�4@���@��@�n/@�a@�A @�+@��@���@���@���@�{�@�2�@� �@�q@~�H@~:*@~_@}�o@}��@}#�@|��@|�j@|tT@|:�@{��@{�@z��@y�C@yIR@x�_@w��@w8@v�y@v�}@v� @vs�@vu@u�@uDg@t��@t��@tbN@s��@s��@s�@r��@r�L@r�1@r}V@qp�@p�5@p�o@o��@o�k@o�@oo�@o�@n��@n�}@n��@n��@n@l�K@l7@k�a@kY@j��@j8�@iϫ@i��@i�@i�h@i%F@h�@h��@h	�@g~�@f�H@f��@fn�@f�@e�S@e/@d�`@d�9@d��@dr�@d"h@d	�@cݘ@cC�@b�]@b	@a�@`��@`r�@`%�@_��@_��@_��@_�P@_4�@^�8@^�\@^)�@]7L@\�@\��@[�}@Z�F@Y��@X�.@W��@W�@Vߤ@Vv�@V8�@V�@V4@U�@U�z@U�=@U�@T�u@TG@S)_@R8�@Q�)@Q��@Q/@Q@@Q�@Q�@Q�@PɆ@O�@O�{@O�@N�r@M�@M&�@L�p@Lq@K�}@Kqv@KY@J��@Jv�@Jd�@Jp;@Ja|@JE�@J.�@Iԕ@I&�@H�.@Hb@G��@G9�@F�H@F��@F��@F^5@F0U@E�@EF@D�I@Dg8@D6@C��@Co�@B�B@Bq�@A�@AY�@@�@@�e@@�u@@c�@@,=@?�+@?��@?��@?t�@?e�@?+@>��@>\�@>�@=�z@=zx@=c�@=IR@=!�@=@<�@<�U@<�@< �@;�;@;1�@:�@:͟@:�R@:�A@:5?@9�z@9S&@9�@8�K@8��@8S�@7�]@7��@7�{@7@6�r@6H�@6�@5��@5��@5+@4�@4�_@3�g@3�V@3E9@31�@3�@3�@3@2ȴ@2�r@1��@1Dg@1�@0�@0�j@0�I@0��@0Z@/��@/�f@/iD@/C�@.�,@.#:@-�X@-N<@-�@,�@,��@,6@+�@+�	@+x@+a@+9�@+�@*��@)�@)|@)N<@(�@(��@(�Y@(�@'��@'{J@'o@&�@&a|@&=q@&	@&�@%�#@%rG@%O�@%/@%V@$��@$��@$��@$m�@$D�@$%�@#�@#�@#a@#@O@#�@"�8@"��@"	@!�@!�X@!�h@!�M@!s�@!F@!7L@!#�@ �	@ �@ �@ �U@ ��@ `�@ �@�g@�a@�@��@�*@�$@�$@�:@o�@a@Mj@$t@�@�B@��@i�@0U@�@�D@�T@�^@zx@O�@7L@q@�@��@��@~(@Xy@D�@9X@-�@�K@dZ@$t@ i@�8@��@�+@8�@��@��@�@��@��@7L@�p@��@�@V�@$@�@��@��@�$@_p@33@
=@�H@ȴ@-@�^@�7@N<@4@�U@z�@m�@Xy@H@9X@!@��@RT@Y@��@��@��@=q@3�@+k@-@$�@
�@�3@p�@0�@��@�9@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�B��B�)B�)B�B��B��B�CB��B��B��B�/B��B��B��B��B�!B��B�5B�B��B��B��B��B��B��B��B�'B��B��B�\B��B	.B	:�B	bNB	��B	��B
 'B
(>B
,�B
3�B
;�B
?�B
J�B
��B
�uB
�9B
�'B
�BB1�BWsBh�BxB��B��B��B�oB��B�B�BKB�BB=B1B�ByBB#B�B9BbB0B+B B�B�:BΊB��B�B�Bu�BR�B+kB�B
�yB
��B
\�B
<�B
*�B
TB	��B	ݘB	�B	aB	$�B	�B	�B��B��B�IB��BˬB��B�aB�BāBªB�fB̘B�bBՁB��B�B�dB��B�^B	�B	�B	�B	VB	�B��B�TB�B�]B�8B	JB	=B	s�B	�\B	��B	��B	�~B	�	B	�9B	�1B	�B	�6B	��B	�B	��B	�B	�mB	��B	�hB	�RB	{B	�B	��B	��B	�(B	|B	�lB	�0B	��B	��B	��B	�xB	�mB	w�B	u�B	p�B	vzB	�iB	�YB	�aB	��B	��B	�GB	��B	�aB	��B	��B	��B	��B	��B	�(B	ðB	��B	��B	�pB	�\B	��B	�B	��B	�aB	҉B	��B	�gB	ٚB	�hB	�B	�B	��B	�B	��B	�B	��B	�nB	�B	�FB	��B	�B	�?B	��B	�B	��B	��B	��B	�$B	��B	��B	�tB	�B	�B	�B	�@B	�hB	�`B	�XB	�B	�B	�DB	��B	�B	�B	�8B	�B	�hB	�B	��B	�iB	�UB	��B	��B	�B	�'B	�GB	�GB	�oB	�B	�sB	�B	��B	�vB	�oB	��B	�}B	�-B	�B	�aB	�-B	�B	��B	��B	�IB	��B	��B	�B	�RB	��B	�B	��B	�;B	�7B	�FB	�B	�HB	�TB	�sB	�1B	�B	�B	՛B	�SB	��B	�B	��B	�oB	�B	�wB	�B	�B	�sB	��B	��B	�B	�DB	�B	��B	�B	�RB	�=B	�fB	�>B	�+B	�<B
�B
'B
;B
B
 �B
 �B
 B	�.B	�BB	�VB	�qB	�qB	�"B	��B	�jB	�(B	�PB	��B	��B	�B	��B	�`B	�B	�|B	�B	�B	��B	�B	�B	�!B	�GB	�'B	�;B	�OB	��B	�B	��B	�zB	��B	�B	�mB	�>B	��B	�B	�?B	��B	��B	�>B	�	B	��B	�	B	��B	�rB	��B	�*B	��B	��B	�xB	��B	��B	��B	�+B	�zB	�nB	�aB	�AB	�B	��B	�B	��B	�GB	�aB	��B	��B	�nB	��B	��B	��B	��B	�B	�hB	�ZB	��B	��B	�HB
�B
B
aB
{B
GB
B
aB
-B
{B
�B
�B
�B
�B
�B
�B
�B
�B
3B
3B
MB
3B
MB
B
�B
B
�B
�B
�B
�B
B
?B
tB
�B
YB
�B
�B
B
�B
EB
B
B
zB
�B
�B
KB
1B
fB
fB
�B
�B
�B
�B
	7B
	B
	B
	B
	B
	RB
	7B
	lB

#B

�B

�B
)B
DB
�B
DB

�B
)B
^B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
bB
�B
�B
<B
BB
�B
\B
BB
�B
\B
B
�B
�B
.B
 B
�B
@B
�B
@B
�B
,B
B
?B
�B
�B
KB
kB
�B
qB
#B
B
�B
QB
�B
sB
+B
B
B
B
�B
�B
KB
1B
1B
1B
�B
B
�B
�B
�B
B
B
B
kB
�B
	B
qB
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
5B
�B
�B
�B
B
!B
;B
VB
�B
�B
�B
 'B
 �B
!�B
"4B
"4B
"NB
# B
"�B
"�B
"�B
#B
#TB
#�B
$@B
$ZB
$�B
%,B
%`B
%�B
&B
&LB
&�B
&�B
'RB
'�B
'mB
'mB
'�B
'�B
'�B
'�B
'�B
(
B
(XB
(XB
(�B
)�B
)�B
)�B
*�B
*�B
+6B
+QB
+�B
,"B
-B
-)B
.�B
/iB
/�B
/�B
/�B
0B
0B
0!B
0;B
0!B
0�B
1B
1vB
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
3B
3B
2�B
2�B
1�B
1�B
1�B
1�B
2|B
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
5ZB
5�B
5�B
5�B
5�B
5�B
6B
6FB
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7fB
7fB
72B
8RB
8RB
8�B
9�B
9�B
9�B
9�B
:B
:^B
:xB
:�B
:xB
;JB
<�B
<�B
<�B
=�B
=�B
>BB
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?}B
?�B
@ B
@B
@B
@iB
@�B
@�B
@�B
@�B
A B
AB
A;B
AB
AB
AUB
A�B
A�B
BB
BuB
BuB
BuB
BuB
BuB
BuB
B[B
BuB
B�B
B�B
B�B
C{B
C-B
CGB
C�B
D�B
D�B
E�B
FB
FB
E�B
FYB
FYB
FYB
FtB
F�B
F�B
F�B
GB
F�B
F�B
G+B
G_B
G_B
G�B
HB
HfB
H�B
H�B
H�B
H�B
IB
IRB
I�B
J	B
J�B
J�B
J�B
KDB
K�B
LdB
L�B
M6B
MPB
MPB
M6B
M6B
MB
MB
MPB
MjB
M�B
NB
N<B
NpB
N�B
N�B
N�B
OB
O(B
O\B
O�B
P.B
PHB
PHB
P�B
PHB
P�B
P�B
QB
QB
Q�B
Q�B
RB
R B
R�B
R�B
R�B
SB
S&B
SB
SuB
S�B
TFB
TaB
T�B
UgB
UgB
U�B
U�B
U�B
U�B
V9B
V�B
WYB
W�B
W�B
XB
X+B
X+B
XEB
W�B
W�B
X�B
X�B
YKB
Y�B
Z7B
ZB
Z7B
ZQB
Z�B
[qB
[�B
[�B
[�B
\B
\]B
\CB
\�B
]IB
]~B
]�B
]�B
^B
]�B
^B
^B
^B
^�B
_B
_!B
_;B
_VB
_VB
_;B
_pB
_�B
_�B
_�B
_�B
`�B
a|B
a�B
b4B
bhB
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
eFB
eFB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g�B
g�B
g�B
h
B
h$B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
iDB
iDB
iyB
iyB
i�B
i�B
j0B
jKB
jeB
jeB
kB
kkB
k�B
k�B
k�B
k�B
k�B
lB
lB
l"B
l=B
lWB
lWB
lWB
lqB
l�B
l�B
mB
mB
mB
mB
m)B
mCB
mCB
mCB
m]B
m]B
mwB
m�B
m�B
m�B
m�B
nIB
n�B
n}B
n�B
n�B
n�B
oB
oB
oOB
oOB
oiB
o�B
o�B
oiB
o�B
o�B
o�B
o�B
pB
p;B
poB
p�B
p�B
p�B
qB
qAB
q[B
qvB
q�B
q�B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
uB
u%B
u%B
vB
vB
vFB
vFB
vFB
v+B
v+B
v`B
wB
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
xRB
x�B
y	B
yXB
y�B
y�B
z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�B��B�)B�)B�B��B��B�CB��B��B��B�/B��B��B��B��B�!B��B�5B�B��B��B��B��B��B��B��B�'B��B��B�\B��B	.B	:�B	bNB	��B	��B
 'B
(>B
,�B
3�B
;�B
?�B
J�B
��B
�uB
�9B
�'B
�BB1�BWsBh�BxB��B��B��B�oB��B�B�BKB�BB=B1B�ByBB#B�B9BbB0B+B B�B�:BΊB��B�B�Bu�BR�B+kB�B
�yB
��B
\�B
<�B
*�B
TB	��B	ݘB	�B	aB	$�B	�B	�B��B��B�IB��BˬB��B�aB�BāBªB�fB̘B�bBՁB��B�B�dB��B�^B	�B	�B	�B	VB	�B��B�TB�B�]B�8B	JB	=B	s�B	�\B	��B	��B	�~B	�	B	�9B	�1B	�B	�6B	��B	�B	��B	�B	�mB	��B	�hB	�RB	{B	�B	��B	��B	�(B	|B	�lB	�0B	��B	��B	��B	�xB	�mB	w�B	u�B	p�B	vzB	�iB	�YB	�aB	��B	��B	�GB	��B	�aB	��B	��B	��B	��B	��B	�(B	ðB	��B	��B	�pB	�\B	��B	�B	��B	�aB	҉B	��B	�gB	ٚB	�hB	�B	�B	��B	�B	��B	�B	��B	�nB	�B	�FB	��B	�B	�?B	��B	�B	��B	��B	��B	�$B	��B	��B	�tB	�B	�B	�B	�@B	�hB	�`B	�XB	�B	�B	�DB	��B	�B	�B	�8B	�B	�hB	�B	��B	�iB	�UB	��B	��B	�B	�'B	�GB	�GB	�oB	�B	�sB	�B	��B	�vB	�oB	��B	�}B	�-B	�B	�aB	�-B	�B	��B	��B	�IB	��B	��B	�B	�RB	��B	�B	��B	�;B	�7B	�FB	�B	�HB	�TB	�sB	�1B	�B	�B	՛B	�SB	��B	�B	��B	�oB	�B	�wB	�B	�B	�sB	��B	��B	�B	�DB	�B	��B	�B	�RB	�=B	�fB	�>B	�+B	�<B
�B
'B
;B
B
 �B
 �B
 B	�.B	�BB	�VB	�qB	�qB	�"B	��B	�jB	�(B	�PB	��B	��B	�B	��B	�`B	�B	�|B	�B	�B	��B	�B	�B	�!B	�GB	�'B	�;B	�OB	��B	�B	��B	�zB	��B	�B	�mB	�>B	��B	�B	�?B	��B	��B	�>B	�	B	��B	�	B	��B	�rB	��B	�*B	��B	��B	�xB	��B	��B	��B	�+B	�zB	�nB	�aB	�AB	�B	��B	�B	��B	�GB	�aB	��B	��B	�nB	��B	��B	��B	��B	�B	�hB	�ZB	��B	��B	�HB
�B
B
aB
{B
GB
B
aB
-B
{B
�B
�B
�B
�B
�B
�B
�B
�B
3B
3B
MB
3B
MB
B
�B
B
�B
�B
�B
�B
B
?B
tB
�B
YB
�B
�B
B
�B
EB
B
B
zB
�B
�B
KB
1B
fB
fB
�B
�B
�B
�B
	7B
	B
	B
	B
	B
	RB
	7B
	lB

#B

�B

�B
)B
DB
�B
DB

�B
)B
^B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
bB
�B
�B
<B
BB
�B
\B
BB
�B
\B
B
�B
�B
.B
 B
�B
@B
�B
@B
�B
,B
B
?B
�B
�B
KB
kB
�B
qB
#B
B
�B
QB
�B
sB
+B
B
B
B
�B
�B
KB
1B
1B
1B
�B
B
�B
�B
�B
B
B
B
kB
�B
	B
qB
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
5B
�B
�B
�B
B
!B
;B
VB
�B
�B
�B
 'B
 �B
!�B
"4B
"4B
"NB
# B
"�B
"�B
"�B
#B
#TB
#�B
$@B
$ZB
$�B
%,B
%`B
%�B
&B
&LB
&�B
&�B
'RB
'�B
'mB
'mB
'�B
'�B
'�B
'�B
'�B
(
B
(XB
(XB
(�B
)�B
)�B
)�B
*�B
*�B
+6B
+QB
+�B
,"B
-B
-)B
.�B
/iB
/�B
/�B
/�B
0B
0B
0!B
0;B
0!B
0�B
1B
1vB
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
3B
3B
2�B
2�B
1�B
1�B
1�B
1�B
2|B
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
5ZB
5�B
5�B
5�B
5�B
5�B
6B
6FB
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7fB
7fB
72B
8RB
8RB
8�B
9�B
9�B
9�B
9�B
:B
:^B
:xB
:�B
:xB
;JB
<�B
<�B
<�B
=�B
=�B
>BB
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?}B
?�B
@ B
@B
@B
@iB
@�B
@�B
@�B
@�B
A B
AB
A;B
AB
AB
AUB
A�B
A�B
BB
BuB
BuB
BuB
BuB
BuB
BuB
B[B
BuB
B�B
B�B
B�B
C{B
C-B
CGB
C�B
D�B
D�B
E�B
FB
FB
E�B
FYB
FYB
FYB
FtB
F�B
F�B
F�B
GB
F�B
F�B
G+B
G_B
G_B
G�B
HB
HfB
H�B
H�B
H�B
H�B
IB
IRB
I�B
J	B
J�B
J�B
J�B
KDB
K�B
LdB
L�B
M6B
MPB
MPB
M6B
M6B
MB
MB
MPB
MjB
M�B
NB
N<B
NpB
N�B
N�B
N�B
OB
O(B
O\B
O�B
P.B
PHB
PHB
P�B
PHB
P�B
P�B
QB
QB
Q�B
Q�B
RB
R B
R�B
R�B
R�B
SB
S&B
SB
SuB
S�B
TFB
TaB
T�B
UgB
UgB
U�B
U�B
U�B
U�B
V9B
V�B
WYB
W�B
W�B
XB
X+B
X+B
XEB
W�B
W�B
X�B
X�B
YKB
Y�B
Z7B
ZB
Z7B
ZQB
Z�B
[qB
[�B
[�B
[�B
\B
\]B
\CB
\�B
]IB
]~B
]�B
]�B
^B
]�B
^B
^B
^B
^�B
_B
_!B
_;B
_VB
_VB
_;B
_pB
_�B
_�B
_�B
_�B
`�B
a|B
a�B
b4B
bhB
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
eFB
eFB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
g8B
g�B
g�B
g�B
h
B
h$B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
iDB
iDB
iyB
iyB
i�B
i�B
j0B
jKB
jeB
jeB
kB
kkB
k�B
k�B
k�B
k�B
k�B
lB
lB
l"B
l=B
lWB
lWB
lWB
lqB
l�B
l�B
mB
mB
mB
mB
m)B
mCB
mCB
mCB
m]B
m]B
mwB
m�B
m�B
m�B
m�B
nIB
n�B
n}B
n�B
n�B
n�B
oB
oB
oOB
oOB
oiB
o�B
o�B
oiB
o�B
o�B
o�B
o�B
pB
p;B
poB
p�B
p�B
p�B
qB
qAB
q[B
qvB
q�B
q�B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
uB
u%B
u%B
vB
vB
vFB
vFB
vFB
v+B
v+B
v`B
wB
v�B
w2B
wfB
w�B
w�B
xB
xB
xB
xB
xRB
x�B
y	B
yXB
y�B
y�B
z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104950  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175121  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175121  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175121                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025129  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025129  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                