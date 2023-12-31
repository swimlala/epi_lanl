CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-31T03:48:00Z creation;2022-08-31T03:48:00Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220831034800  20220831040559  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�� 4��1   @�� |�/�@-o��-V�c}&�x��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi33Bq33Bw��B��B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C33C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CLL�CN�CO�fCR  CT  CU��CX  CY��C\�C^�C`  Cb33Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@34@�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bh��Bp��Bw34B34B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B���B�  Bә�Bי�B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�C	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCL33CN  CO��CQ�fCS�fCU� CW�fCY�3C\  C^  C_�fCb�Cc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�4Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D!  D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Do  Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؟�Aؓ@A؈fA�m�A�Q�A�%FA��A�
	A��A��A�A�A��A��A�AA� �A���A��"A���A��A��rA��2A��)A��&A�҉Aק�A�	7A�9�AӮIA�p�A��[AѭA���A�<jA�CA�OvA��A�U�A�]�A�=<A��LA�A��;A�@�A���A�Q�A��A�t�A�@�A�Y�A�&A�\]A��	A��A�D�A���A�0�A��qA�-�A�	lA���A�A�DA�E9A�ԕA�|�A��6A��#A�!-A��A���A�JA���A�l�A���A���A�0�A��A�?�A��A��eA�=A�1�A��cA��A��A�K)A���Ay�CAw!-ArW?Am�AiAc�A`dZA];AVȴATT�AQ��AJ��AH"hAF��AE�AC��AA�cA?`�A=��A<>BA;r�A8��A64�A4��A4A3�$A2�A/��A.��A.6A-B�A,M�A,4A+C�A*a|A)��A)��A)4A(H�A&��A%N<A$h�A"��A��A��A�A�2A�AGEA q�A!XA �PA�AA*�A��A	�A��A/�A�-AdZA�+AѷA;�A��A�VAsAD�A��A��AsA�A�`AoA҉A�ZA�.A��A2aA�~A��AیA��A��Al�AS�A0�AیAA�mA��A	��A	��A	�zA	��A	�A��A�CA]dAYKA6zA��AzxA*�A�fAVAL�A_A�A��A�AȴA<6A|�A>�A&A ��@��C@�V@���@��@�H�@�@�\�@�J�@�o@�=�@��@�F�@�%@��@�1�@�f@���@�Y@��@��|@�@�~�@�u�@�4n@ﯸ@@�bN@�6�@��@�o @���@�a@�ȴ@�c�@�O�@�R@�^5@���@��@���@�kQ@��@�!-@�4@��Z@��H@᯸@�L�@��8@��@��c@��W@�^�@�J�@�@ܭ�@��d@�5�@�z�@�Ĝ@�tT@��T@�@O@��s@ج�@�oi@�ϫ@�U�@֞@��@ռ�@�`B@�@ԭ�@� �@�t�@�33@��]@�[�@���@�_p@�@@�s�@�_@���@χ�@���@��D@͙�@͆�@�2a@̉�@��D@ˤ@@�)_@�n�@ɩ�@�0�@���@���@ȫ6@�g8@�-�@��Z@Ǵ�@��P@Ɗr@�^5@�M�@�4n@�	@��o@�ƨ@�v`@�*0@�u�@��j@��a@�+�@�l�@�O@�1@���@�ݘ@�Y�@��@���@��W@��h@�`B@��@�N�@��@�.I@��@�c�@��+@��M@�@���@�C�@��@��
@�w2@��@�|�@��@��@���@�S�@��@���@��@���@�a�@��P@��L@�n�@�M@�~�@�L�@�@���@�~�@�I�@�@�@�C�@��@�X@�+@��5@���@���@�8�@�o @��@��$@��+@�b@��H@�:�@��c@��@���@��@�k�@�:�@���@�,=@���@��*@�K�@�
=@���@�K^@�b@��:@���@���@��@�A�@�O@��H@�"�@�GE@�>B@�'R@���@�n/@�\)@�
=@��'@���@���@��+@���@�Mj@�-w@��@��@� �@��0@���@�>�@��@��1@�O@��@�?}@��@��@�q@�$�@���@���@�ȴ@���@��@��@���@�g8@�~@�ƨ@�b�@�Ɇ@�tT@�2�@�  @��@��V@�33@��s@���@�n�@��@���@�rG@�5�@��|@��$@��u@���@��o@�kQ@�u@��V@�u�@�J�@�/@��5@��@���@�M@��w@�O@�(�@��"@��2@���@���@�h�@�Ft@��3@�|�@�e,@�X�@�Mj@�Dg@��@���@��@�\�@��]@�S�@�	l@���@�Q�@��@��*@�o @�҉@��@��_@�n�@�	@�@��@�l�@�0�@�ں@��@��o@�Xy@� �@���@���@���@�_p@�A @�:�@�o@���@���@���@��@�{�@�oi@�2�@��W@��&@��K@���@�X@�/@�@��`@��,@��I@�z@�M�@��@���@�}�@�,�@�֡@���@��@�H�@���@��h@�J#@���@���@�|�@�q�@�Xy@��@�$@8@~��@~.�@}s�@|�@|�e@|_@| �@{�V@{Y@z�x@zM�@y�h@y0�@y%@x�p@x�Y@x_@w�g@wJ#@v��@v6�@u��@u(�@t�@tM@st�@r�@rQ@q�.@q��@p�	@ph�@o��@n�'@n�@m�N@mp�@l�@l�D@l_@l,=@k��@kx@j�2@jZ�@jGE@i�d@h�@h�u@h%�@g��@g�$@g)_@f�@f�X@f��@f�A@f;�@e�@e�~@e��@e-w@d�@d�U@d��@dtT@d	�@c�K@ce�@c;d@c1�@b��@b�@a��@a�h@aY�@a/@`�P@`��@`oi@`C-@_��@_&@^ߤ@^�x@^c @^u@]�@]@]�H@]X@]�@\�?@\�4@\H@[ƨ@[v`@[1�@Z��@Z�6@Z� @Z5?@Y�>@Y�~@Y�@X�@X9X@W�@W�4@W33@Vߤ@V�@Vq�@V+k@U��@U�@UDg@T��@T�j@Tc�@T	�@S�q@Ss@So@R�R@Rh
@Q�@Q�-@Q�M@Q!�@PɆ@PtT@Pb@O�@O��@O��@Oe�@OP�@OF�@N�@N��@N\�@Ne@Mԕ@M�#@M��@M`B@L��@Ly>@K��@K��@K�@KRT@Jߤ@J� @J�@I��@IrG@I�@Hu�@H7�@H1@G�;@G�w@G��@G�*@G�f@GP�@G�@F�R@F?@E�Z@E�@D�$@Du�@D1'@C�@B��@B�@A�j@AN<@@�p@@��@@��@@[�@@�@?>�@>��@>?@=�@=��@=��@=\�@=�@<tT@<2�@;��@;� @;��@:�s@:�A@:4@9��@9��@9^�@8��@8��@8Z@8  @7��@7 i@6��@6^5@6e@5��@5N<@54@4�v@4Ĝ@4�Y@3��@3��@3�@3��@3v`@3]�@2��@2@�@2.�@1��@1�'@17L@0��@0*�@/�w@/j�@/9�@.��@.n�@-��@-��@-!�@,��@,tT@+�@+_p@+"�@*��@*��@*�x@*#:@)@)��@)f�@)+@(��@(�@'ƨ@'��@'g�@''�@&ߤ@&��@&C�@&
�@%�9@%�C@%&�@$��@$g8@#خ@#��@#U�@"͟@"�!@"�\@"kQ@!�@!�-@!O�@!-w@ �`@ ��@�g@�@��@��@o�@F�@@O@�@(@�@1�@	@��@��@��@x�@q@��@��@N�@�@e�@A�@��@�B@�h@��@M�@��@�9@�S@x�@hs@^�@X@�	@�I@D�@"h@ݘ@�g@�}@�K@��@��@iD@Z�@Z�@J#@@��@��@��@��@ff@@�9@�N@�H@��@rG@k�@*0@�e@V�@	�@~�@o�@b�@iD@o�@�$@�:@o@��@~�@s�@kQ@5?@	@_@�o@�h@J�@�@�_@`�@N�@@��@�
@��@��@{J@8@�@�@�@ȴ@�!@�r@l�@YK@1�@�@@
�@��@�=@a�@j@|@o @e,@G�@/@�K@�.@e�@V�@N�@M@>B@��@�
@�w@�@@��@~�@Mj@&@
��@
�8@
҉@
�}@
�A@
YK@
1�@

�@	��@	�#@	�H@	�@	hs@	X@	#�@��@�E@ѷ@�z@�o@~(@D�@(�@�r@�@��@��@�V@��@o�@b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؟�Aؓ@A؈fA�m�A�Q�A�%FA��A�
	A��A��A�A�A��A��A�AA� �A���A��"A���A��A��rA��2A��)A��&A�҉Aק�A�	7A�9�AӮIA�p�A��[AѭA���A�<jA�CA�OvA��A�U�A�]�A�=<A��LA�A��;A�@�A���A�Q�A��A�t�A�@�A�Y�A�&A�\]A��	A��A�D�A���A�0�A��qA�-�A�	lA���A�A�DA�E9A�ԕA�|�A��6A��#A�!-A��A���A�JA���A�l�A���A���A�0�A��A�?�A��A��eA�=A�1�A��cA��A��A�K)A���Ay�CAw!-ArW?Am�AiAc�A`dZA];AVȴATT�AQ��AJ��AH"hAF��AE�AC��AA�cA?`�A=��A<>BA;r�A8��A64�A4��A4A3�$A2�A/��A.��A.6A-B�A,M�A,4A+C�A*a|A)��A)��A)4A(H�A&��A%N<A$h�A"��A��A��A�A�2A�AGEA q�A!XA �PA�AA*�A��A	�A��A/�A�-AdZA�+AѷA;�A��A�VAsAD�A��A��AsA�A�`AoA҉A�ZA�.A��A2aA�~A��AیA��A��Al�AS�A0�AیAA�mA��A	��A	��A	�zA	��A	�A��A�CA]dAYKA6zA��AzxA*�A�fAVAL�A_A�A��A�AȴA<6A|�A>�A&A ��@��C@�V@���@��@�H�@�@�\�@�J�@�o@�=�@��@�F�@�%@��@�1�@�f@���@�Y@��@��|@�@�~�@�u�@�4n@ﯸ@@�bN@�6�@��@�o @���@�a@�ȴ@�c�@�O�@�R@�^5@���@��@���@�kQ@��@�!-@�4@��Z@��H@᯸@�L�@��8@��@��c@��W@�^�@�J�@�@ܭ�@��d@�5�@�z�@�Ĝ@�tT@��T@�@O@��s@ج�@�oi@�ϫ@�U�@֞@��@ռ�@�`B@�@ԭ�@� �@�t�@�33@��]@�[�@���@�_p@�@@�s�@�_@���@χ�@���@��D@͙�@͆�@�2a@̉�@��D@ˤ@@�)_@�n�@ɩ�@�0�@���@���@ȫ6@�g8@�-�@��Z@Ǵ�@��P@Ɗr@�^5@�M�@�4n@�	@��o@�ƨ@�v`@�*0@�u�@��j@��a@�+�@�l�@�O@�1@���@�ݘ@�Y�@��@���@��W@��h@�`B@��@�N�@��@�.I@��@�c�@��+@��M@�@���@�C�@��@��
@�w2@��@�|�@��@��@���@�S�@��@���@��@���@�a�@��P@��L@�n�@�M@�~�@�L�@�@���@�~�@�I�@�@�@�C�@��@�X@�+@��5@���@���@�8�@�o @��@��$@��+@�b@��H@�:�@��c@��@���@��@�k�@�:�@���@�,=@���@��*@�K�@�
=@���@�K^@�b@��:@���@���@��@�A�@�O@��H@�"�@�GE@�>B@�'R@���@�n/@�\)@�
=@��'@���@���@��+@���@�Mj@�-w@��@��@� �@��0@���@�>�@��@��1@�O@��@�?}@��@��@�q@�$�@���@���@�ȴ@���@��@��@���@�g8@�~@�ƨ@�b�@�Ɇ@�tT@�2�@�  @��@��V@�33@��s@���@�n�@��@���@�rG@�5�@��|@��$@��u@���@��o@�kQ@�u@��V@�u�@�J�@�/@��5@��@���@�M@��w@�O@�(�@��"@��2@���@���@�h�@�Ft@��3@�|�@�e,@�X�@�Mj@�Dg@��@���@��@�\�@��]@�S�@�	l@���@�Q�@��@��*@�o @�҉@��@��_@�n�@�	@�@��@�l�@�0�@�ں@��@��o@�Xy@� �@���@���@���@�_p@�A @�:�@�o@���@���@���@��@�{�@�oi@�2�@��W@��&@��K@���@�X@�/@�@��`@��,@��I@�z@�M�@��@���@�}�@�,�@�֡@���@��@�H�@���@��h@�J#@���@���@�|�@�q�@�Xy@��@�$@8@~��@~.�@}s�@|�@|�e@|_@| �@{�V@{Y@z�x@zM�@y�h@y0�@y%@x�p@x�Y@x_@w�g@wJ#@v��@v6�@u��@u(�@t�@tM@st�@r�@rQ@q�.@q��@p�	@ph�@o��@n�'@n�@m�N@mp�@l�@l�D@l_@l,=@k��@kx@j�2@jZ�@jGE@i�d@h�@h�u@h%�@g��@g�$@g)_@f�@f�X@f��@f�A@f;�@e�@e�~@e��@e-w@d�@d�U@d��@dtT@d	�@c�K@ce�@c;d@c1�@b��@b�@a��@a�h@aY�@a/@`�P@`��@`oi@`C-@_��@_&@^ߤ@^�x@^c @^u@]�@]@]�H@]X@]�@\�?@\�4@\H@[ƨ@[v`@[1�@Z��@Z�6@Z� @Z5?@Y�>@Y�~@Y�@X�@X9X@W�@W�4@W33@Vߤ@V�@Vq�@V+k@U��@U�@UDg@T��@T�j@Tc�@T	�@S�q@Ss@So@R�R@Rh
@Q�@Q�-@Q�M@Q!�@PɆ@PtT@Pb@O�@O��@O��@Oe�@OP�@OF�@N�@N��@N\�@Ne@Mԕ@M�#@M��@M`B@L��@Ly>@K��@K��@K�@KRT@Jߤ@J� @J�@I��@IrG@I�@Hu�@H7�@H1@G�;@G�w@G��@G�*@G�f@GP�@G�@F�R@F?@E�Z@E�@D�$@Du�@D1'@C�@B��@B�@A�j@AN<@@�p@@��@@��@@[�@@�@?>�@>��@>?@=�@=��@=��@=\�@=�@<tT@<2�@;��@;� @;��@:�s@:�A@:4@9��@9��@9^�@8��@8��@8Z@8  @7��@7 i@6��@6^5@6e@5��@5N<@54@4�v@4Ĝ@4�Y@3��@3��@3�@3��@3v`@3]�@2��@2@�@2.�@1��@1�'@17L@0��@0*�@/�w@/j�@/9�@.��@.n�@-��@-��@-!�@,��@,tT@+�@+_p@+"�@*��@*��@*�x@*#:@)@)��@)f�@)+@(��@(�@'ƨ@'��@'g�@''�@&ߤ@&��@&C�@&
�@%�9@%�C@%&�@$��@$g8@#خ@#��@#U�@"͟@"�!@"�\@"kQ@!�@!�-@!O�@!-w@ �`@ ��@�g@�@��@��@o�@F�@@O@�@(@�@1�@	@��@��@��@x�@q@��@��@N�@�@e�@A�@��@�B@�h@��@M�@��@�9@�S@x�@hs@^�@X@�	@�I@D�@"h@ݘ@�g@�}@�K@��@��@iD@Z�@Z�@J#@@��@��@��@��@ff@@�9@�N@�H@��@rG@k�@*0@�e@V�@	�@~�@o�@b�@iD@o�@�$@�:@o@��@~�@s�@kQ@5?@	@_@�o@�h@J�@�@�_@`�@N�@@��@�
@��@��@{J@8@�@�@�@ȴ@�!@�r@l�@YK@1�@�@@
�@��@�=@a�@j@|@o @e,@G�@/@�K@�.@e�@V�@N�@M@>B@��@�
@�w@�@@��@~�@Mj@&@
��@
�8@
҉@
�}@
�A@
YK@
1�@

�@	��@	�#@	�H@	�@	hs@	X@	#�@��@�E@ѷ@�z@�o@~(@D�@(�@�r@�@��@��@�V@��@o�@b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
�B
��B
�1B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�7B
�kB
�kB
��B
��B
�kB
�B
��B
�B
�B
��B
��B
�,B
��B
��B
��B
��B
��B
�0B[#Bx�B!�BQ4Bo BrBt�ButB}B��B�B��B�B�XB�_B�.BncBX�BS&BW�BVSBB�B.cB($B�BBPB	7B�B BoB�RB�BҽB��B�"B�`B�FB�uBg�BN�B(>BNB
��B
ܒB
ҽB
ȀB
��B
b�B
G+B
*eB
CB
BB	�B	ϑB	�	B	�=B	��B	b�B	QB	<�B	�B	�B	�B��B��B�B�RBߊBۦB��B�NB��B�hB�B�B�kB�B��B��B��B��B��B	�B	hB	(�B	.�B	;0B	=�B	=�B	C-B	G_B	F%B	G�B	NVB	G�B	;�B	5�B	6B	>�B	G�B	m)B	��B	�B	��B	��B	�qB	� B	��B	�B	�)B	��B	�B	��B	�uB	ƨB	�iB	�]B	�}B	� B	��B	�B	��B	��B	�=B	�XB	��B	��B	�B	��B	�IB	�B	�"B	�B	��B	��B	�}B	�cB	�5B	��B	��B	�!B	�+B	ΊB	��B	׍B	��B	�_B	��B	��B	��B	ϫB	�(B	��B	�?B	ҽB	�B	�^B	�B	�~B	�B	�jB	��B	��B	��B	�	B	��B	�lB	�B	�B	�dB	ΥB	�B	�\B	ϫB	�(B	��B	��B	ҽB	��B	��B	�[B	�[B	�uB	��B	өB	�uB	�uB	�[B	ԕB	��B	ԕB	ևB	��B	�B	׍B	�sB	׍B	��B	��B	�eB	�B	�B	�=B	��B	�=B	��B	�/B	�OB	ޞB	߾B	�B	��B	��B	�vB	�B	�IB	�~B	�~B	��B	�;B	�B	��B	�~B	��B	ۦB	��B	ںB	�;B	�vB	�BB	��B	�4B	�NB	��B	�B	�ZB	�B	��B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�}B	�}B	��B	�B	��B	��B	�'B	�GB	�aB	�9B	�B	��B	��B	��B	�ZB	��B	��B	�`B	��B	�8B	�RB	�8B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�6B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	�6B	�B	��B	��B	��B	�"B	�"B	�"B	�qB	��B	��B	�qB	�VB	��B	��B	��B	�.B	��B	��B	�]B	��B	�cB
 B
uB
�B
[B
�B
-B
�B
�B
�B
�B
aB
aB
aB
3B
B
B
�B
B
�B
�B
MB
MB
B
�B
�B
�B
zB
B
SB
tB
EB
�B
+B
	�B
	7B
xB
)B

#B
^B

�B

#B
	�B
	�B

�B
B
JB
)B
	�B
)B
�B
^B

�B
B
�B
B
�B
VB
bB
�B
VB
VB
�B
}B
�B
�B
TB
&B
@B
FB
B
B
�B
{B
B
{B
�B
�B
�B
gB
�B
B

B
�B
EB
+B
�B
yB
�B
�B
�B
�B
�B
�B
yB
�B
1B
KB
B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
/B
IB
�B
�B
B
B
�B
jB
�B
�B
 B
 B
 B
 \B
!HB
 �B
!B
!�B
"B
"B
"B
"B
!�B
"NB
"�B
#B
"�B
#�B
#nB
#�B
#�B
$@B
$�B
$�B
%`B
&2B
&B
&B
&B
&LB
&�B
&�B
'8B
'�B
'mB
'�B
(>B
(XB
(XB
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
*B
*�B
*�B
*�B
+B
+�B
+QB
+�B
,"B
,=B
,qB
,�B
,�B
,�B
-B
-)B
-�B
.cB
.�B
.�B
/iB
/�B
/�B
/�B
0�B
0�B
0�B
1�B
2-B
2�B
3hB
3�B
4B
4TB
4�B
4�B
4�B
5tB
5�B
5�B
6B
6FB
6+B
6�B
72B
7�B
7�B
8lB
8�B
8lB
8�B
8�B
8�B
8�B
8�B
9$B
9>B
9rB
9�B
9�B
:DB
:xB
;B
;JB
;�B
<B
<PB
<B
;�B
<6B
<B
;�B
;�B
;�B
<B
<B
<PB
<�B
="B
>�B
?B
?.B
@ B
@�B
A�B
BB
B[B
B�B
B�B
B�B
CB
CB
C{B
CGB
C�B
D3B
DB
DgB
D�B
D�B
D�B
E�B
E�B
E�B
F?B
FtB
F?B
F�B
G�B
G�B
G�B
G�B
G�B
HB
HfB
H�B
H�B
I7B
I�B
I�B
I�B
I�B
J#B
J#B
J#B
J	B
JrB
JrB
J�B
J�B
KB
KDB
KDB
K�B
K�B
K�B
K�B
LB
LB
LJB
L~B
L�B
MPB
M6B
MjB
M�B
M�B
N"B
N"B
NpB
N�B
N�B
OB
OBB
O(B
OvB
O�B
O�B
PB
PbB
P}B
P�B
Q4B
QB
Q4B
QhB
Q�B
RB
R:B
RTB
RoB
R�B
R�B
R�B
R�B
S@B
S&B
SuB
SuB
S@B
S�B
S�B
T,B
T{B
TaB
UgB
U�B
U�B
U�B
U�B
U�B
VB
V9B
V9B
VB
U�B
UgB
U�B
VB
VSB
V�B
V�B
V�B
XB
XEB
X�B
X_B
XyB
X�B
YeB
YKB
YeB
X�B
X�B
YB
YB
YB
Y�B
ZkB
Z�B
[	B
Z�B
[WB
[	B
[WB
[�B
[�B
[�B
\B
\�B
\�B
\�B
\�B
]B
]~B
^B
^B
^�B
_!B
_!B
_;B
_�B
_pB
_�B
`\B
aB
abB
a�B
a�B
bB
bNB
bNB
b�B
cB
b�B
c B
cTB
cnB
cnB
c�B
c�B
d&B
c�B
dB
c�B
d�B
dZB
eB
e�B
fLB
f�B
g8B
gRB
g�B
g�B
h>B
h>B
h�B
h>B
h�B
h�B
i_B
i�B
i�B
jB
jB
j�B
j�B
j�B
kB
kB
j�B
j�B
j�B
kB
k6B
k�B
k�B
l"B
lB
l=B
l=B
l=B
m�B
l�B
mCB
m�B
o B
o�B
o�B
p!B
p;B
p�B
p�B
p�B
qB
qAB
qvB
p�B
qB
qB
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
q�B
rB
rGB
rGB
r�B
r�B
sB
sMB
s�B
tB
tnB
t�B
u?B
u�B
vB
v+B
vzB
v+B
v`B
v+B
v`B
vzB
v�B
wLB
v�B
w�B
w�B
w�B
x�B
yXB
y�B
y�B
zDB
{0B
{B
{dB
{B
{�B
|�B
|�B
|�B
}VB
}qB
}qB
}�B
~B
~B
~(B
~]B
~wB
~]B
~�B
~�B
~]B
}�B
}�B
~B
~B
~(B
~BB
HB
�B
��B
�B
�B
�B
� B
�B
�;B
�B
�;B
��B
�oB
��B
�'B
�[B
�uB
��B
��B
��B
��B
��B
�-B
�-B
�GB
�-B
�GB
�GB
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
��B
�B
�9B
�9B
�mB
�SB
��B
�9B
��B
��B
��B
��B
��B
��B
��B
��B
�?B
�?B
��B
��B
��B
��B
�B
�zB
��B
�B
�B
�KB
�B
�B
�fB
�fB
��B
��B
�B
�B
��B
�B
��B
�B
�B
��B
��B
��B
��B
�#B
��B
��B
��B
��B
��B
�0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
�B
��B
�1B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�7B
�kB
�kB
��B
��B
�kB
�B
��B
�B
�B
��B
��B
�,B
��B
��B
��B
��B
��B
�0B[#Bx�B!�BQ4Bo BrBt�ButB}B��B�B��B�B�XB�_B�.BncBX�BS&BW�BVSBB�B.cB($B�BBPB	7B�B BoB�RB�BҽB��B�"B�`B�FB�uBg�BN�B(>BNB
��B
ܒB
ҽB
ȀB
��B
b�B
G+B
*eB
CB
BB	�B	ϑB	�	B	�=B	��B	b�B	QB	<�B	�B	�B	�B��B��B�B�RBߊBۦB��B�NB��B�hB�B�B�kB�B��B��B��B��B��B	�B	hB	(�B	.�B	;0B	=�B	=�B	C-B	G_B	F%B	G�B	NVB	G�B	;�B	5�B	6B	>�B	G�B	m)B	��B	�B	��B	��B	�qB	� B	��B	�B	�)B	��B	�B	��B	�uB	ƨB	�iB	�]B	�}B	� B	��B	�B	��B	��B	�=B	�XB	��B	��B	�B	��B	�IB	�B	�"B	�B	��B	��B	�}B	�cB	�5B	��B	��B	�!B	�+B	ΊB	��B	׍B	��B	�_B	��B	��B	��B	ϫB	�(B	��B	�?B	ҽB	�B	�^B	�B	�~B	�B	�jB	��B	��B	��B	�	B	��B	�lB	�B	�B	�dB	ΥB	�B	�\B	ϫB	�(B	��B	��B	ҽB	��B	��B	�[B	�[B	�uB	��B	өB	�uB	�uB	�[B	ԕB	��B	ԕB	ևB	��B	�B	׍B	�sB	׍B	��B	��B	�eB	�B	�B	�=B	��B	�=B	��B	�/B	�OB	ޞB	߾B	�B	��B	��B	�vB	�B	�IB	�~B	�~B	��B	�;B	�B	��B	�~B	��B	ۦB	��B	ںB	�;B	�vB	�BB	��B	�4B	�NB	��B	�B	�ZB	�B	��B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�}B	�}B	��B	�B	��B	��B	�'B	�GB	�aB	�9B	�B	��B	��B	��B	�ZB	��B	��B	�`B	��B	�8B	�RB	�8B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�6B	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	�6B	�B	��B	��B	��B	�"B	�"B	�"B	�qB	��B	��B	�qB	�VB	��B	��B	��B	�.B	��B	��B	�]B	��B	�cB
 B
uB
�B
[B
�B
-B
�B
�B
�B
�B
aB
aB
aB
3B
B
B
�B
B
�B
�B
MB
MB
B
�B
�B
�B
zB
B
SB
tB
EB
�B
+B
	�B
	7B
xB
)B

#B
^B

�B

#B
	�B
	�B

�B
B
JB
)B
	�B
)B
�B
^B

�B
B
�B
B
�B
VB
bB
�B
VB
VB
�B
}B
�B
�B
TB
&B
@B
FB
B
B
�B
{B
B
{B
�B
�B
�B
gB
�B
B

B
�B
EB
+B
�B
yB
�B
�B
�B
�B
�B
�B
yB
�B
1B
KB
B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
/B
IB
�B
�B
B
B
�B
jB
�B
�B
 B
 B
 B
 \B
!HB
 �B
!B
!�B
"B
"B
"B
"B
!�B
"NB
"�B
#B
"�B
#�B
#nB
#�B
#�B
$@B
$�B
$�B
%`B
&2B
&B
&B
&B
&LB
&�B
&�B
'8B
'�B
'mB
'�B
(>B
(XB
(XB
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
*B
*�B
*�B
*�B
+B
+�B
+QB
+�B
,"B
,=B
,qB
,�B
,�B
,�B
-B
-)B
-�B
.cB
.�B
.�B
/iB
/�B
/�B
/�B
0�B
0�B
0�B
1�B
2-B
2�B
3hB
3�B
4B
4TB
4�B
4�B
4�B
5tB
5�B
5�B
6B
6FB
6+B
6�B
72B
7�B
7�B
8lB
8�B
8lB
8�B
8�B
8�B
8�B
8�B
9$B
9>B
9rB
9�B
9�B
:DB
:xB
;B
;JB
;�B
<B
<PB
<B
;�B
<6B
<B
;�B
;�B
;�B
<B
<B
<PB
<�B
="B
>�B
?B
?.B
@ B
@�B
A�B
BB
B[B
B�B
B�B
B�B
CB
CB
C{B
CGB
C�B
D3B
DB
DgB
D�B
D�B
D�B
E�B
E�B
E�B
F?B
FtB
F?B
F�B
G�B
G�B
G�B
G�B
G�B
HB
HfB
H�B
H�B
I7B
I�B
I�B
I�B
I�B
J#B
J#B
J#B
J	B
JrB
JrB
J�B
J�B
KB
KDB
KDB
K�B
K�B
K�B
K�B
LB
LB
LJB
L~B
L�B
MPB
M6B
MjB
M�B
M�B
N"B
N"B
NpB
N�B
N�B
OB
OBB
O(B
OvB
O�B
O�B
PB
PbB
P}B
P�B
Q4B
QB
Q4B
QhB
Q�B
RB
R:B
RTB
RoB
R�B
R�B
R�B
R�B
S@B
S&B
SuB
SuB
S@B
S�B
S�B
T,B
T{B
TaB
UgB
U�B
U�B
U�B
U�B
U�B
VB
V9B
V9B
VB
U�B
UgB
U�B
VB
VSB
V�B
V�B
V�B
XB
XEB
X�B
X_B
XyB
X�B
YeB
YKB
YeB
X�B
X�B
YB
YB
YB
Y�B
ZkB
Z�B
[	B
Z�B
[WB
[	B
[WB
[�B
[�B
[�B
\B
\�B
\�B
\�B
\�B
]B
]~B
^B
^B
^�B
_!B
_!B
_;B
_�B
_pB
_�B
`\B
aB
abB
a�B
a�B
bB
bNB
bNB
b�B
cB
b�B
c B
cTB
cnB
cnB
c�B
c�B
d&B
c�B
dB
c�B
d�B
dZB
eB
e�B
fLB
f�B
g8B
gRB
g�B
g�B
h>B
h>B
h�B
h>B
h�B
h�B
i_B
i�B
i�B
jB
jB
j�B
j�B
j�B
kB
kB
j�B
j�B
j�B
kB
k6B
k�B
k�B
l"B
lB
l=B
l=B
l=B
m�B
l�B
mCB
m�B
o B
o�B
o�B
p!B
p;B
p�B
p�B
p�B
qB
qAB
qvB
p�B
qB
qB
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
q�B
rB
rGB
rGB
r�B
r�B
sB
sMB
s�B
tB
tnB
t�B
u?B
u�B
vB
v+B
vzB
v+B
v`B
v+B
v`B
vzB
v�B
wLB
v�B
w�B
w�B
w�B
x�B
yXB
y�B
y�B
zDB
{0B
{B
{dB
{B
{�B
|�B
|�B
|�B
}VB
}qB
}qB
}�B
~B
~B
~(B
~]B
~wB
~]B
~�B
~�B
~]B
}�B
}�B
~B
~B
~(B
~BB
HB
�B
��B
�B
�B
�B
� B
�B
�;B
�B
�;B
��B
�oB
��B
�'B
�[B
�uB
��B
��B
��B
��B
��B
�-B
�-B
�GB
�-B
�GB
�GB
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
��B
�B
�9B
�9B
�mB
�SB
��B
�9B
��B
��B
��B
��B
��B
��B
��B
��B
�?B
�?B
��B
��B
��B
��B
�B
�zB
��B
�B
�B
�KB
�B
�B
�fB
�fB
��B
��B
�B
�B
��B
�B
��B
�B
�B
��B
��B
��B
��B
�#B
��B
��B
��B
��B
��B
�0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220831034758  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220831034800  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220831034800  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220831034800                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220831124805  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220831124805  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220831040559                      G�O�G�O�G�O�                