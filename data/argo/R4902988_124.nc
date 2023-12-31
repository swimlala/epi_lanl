CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-03T15:42:57Z creation;2023-03-03T15:42:59Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230303154257  20230303155650  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               |A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @����s1   @��-��@;w�O�;d�c�-1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C�fC  C�C  C
�C�C�C  C  C  C  C  C  C  C  C�fC"  C$  C%�fC'�fC)�fC,  C.  C0�C2  C3�fC6  C7�fC:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP�CR�CT  CV  CX  CZ�C[�fC^  C`  Cb  Cc�fCf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C��C��C��3C��3C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	�fD
fD
� D  Dy�D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DD��DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DV��DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{y�D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D���D���D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ DŃ3D�� D�  D�@ Dƀ D�� D���D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�D�|�Dɼ�D���D�@ Dʀ Dʼ�D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�3D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�3D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D��3D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���@���AffA>ffA`  A�  A�33A�33A�33A�33A�33A�33A�33A�ffB��B��B��B��B'��B/��B7��B?34BG34BO��BW��B_��Bg��Bo��Bw��B��B�  B���B���B�  B�  B���B���B���B���B�  B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B�B���B���B���C��C�fC  C�fC
  C  C  C�fC�fC�fC�fC�fC�fC�fC�fC��C!�fC#�fC%��C'��C)��C+�fC-�fC0  C1�fC3��C5�fC7��C9�fC;�fC=�fC?�fCA�fCC�fCF  CH  CI�fCK�fCM�fCP  CR  CS�fCU�fCW�fCZ  C[��C]�fC_�fCa�fCc��Ce�fCg�fCi��Ck�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��fC��fC�  C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C�  C��3C��3C�  C��3C��3C�  C��3C��3C��3C�  C�  C��fC��fC��3C��3C��3C�  C�  C��3C��3C��3C��fC��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C�  C��3C��3D y�D ��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D	  D	� D
  D
y�D
��Ds4D��Dy�D��Dy�D��Ds4D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Ds4D�4Dy�D��Dy�D��Dy�D�4Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D<  D<� D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCs4DC��DDy�DD�4DEy�DE��DFy�DF��DG� DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DM  DMy�DM��DNy�DN��DOy�DO��DPs4DP��DQy�DQ��DRy�DR�4DSy�DS��DTy�DT��DUy�DU��DVy�DV�4DWy�DW�4DXy�DX��DYy�DY��DZy�DZ��D[y�D[�4D\s4D\��D]y�D]��D^y�D^��D_y�D_��D`� D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhs4Dh�4Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqs4Dq�4Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvs4Dv��Dwy�Dw��Dxy�Dx�4Dyy�Dy��Dzy�Dz��D{s4D{�4D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�9�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D�� D�  D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�@ D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�Dŀ Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�9�D�y�Dɹ�D���D�<�D�|�Dʹ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�y�Dͼ�D�  D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D�  D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�@ D�|�Dּ�D���D�9�D�|�D׼�D���D�<�D�|�Dؼ�D���D�@ D�|�Dټ�D�  D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D�  D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�y�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�9�D�|�D�� D�  D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D�  D�<�D�|�D��D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D���D���D�<�D�|�D���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�uA��A��A��A�IA�!-A�"hA�"4A�"�A�#nA�#�A�"hA�	A�+A��A��A�eA�A�CA��A��A��A�xA��A�CA�A�1A�YA��A�eA��A��A�A�bA��A��A��A�o A��A��A��RA�wfA�6A�IA�_A���A���A�Z�A�C�A�-CA�
	A�҉A���A�Y�A�uA��kA��'A��rA��tA�bA�uZA�'�A��<A���A���A�uA��fA���A��A��2A��,A���A��qA��A�W�A�H�A�бA�!bA��]A���A�Q�A���A�MA�E�A���A��+A��2A�B'A�YKA���A�FA�Q�A�dA�A~CA|�Ay�AxL�Aw:�Av0UAu�AurGAt��Aq��Ao�rAn��Al�cAk��Ai��AhAg4Af��Af4�Ad�Ab��Aa?�A_�fA^�4A]$tA[W�AZK�AY�NAY�AXK�AW�NAV��AVy>AV@�AV4nAV)_AV�AV�AUݘAU�jAU��AUFAS��ARG�AQy>AQJ#AP�)AP��AO�9AN�8AN�AM�IAL�:AK��AJ��AJ�FAJ�AI�HAFd�AD�FAC��ACZAB�ABC�AA��A@�"A?VA>\)A> �A=�A=͟A=��A<K�A:��A9�A9.IA7�uA6��A6��A6|�A6b�A5�|A51�A4z�A4�A3xlA1��A0��A/|A.�zA.MA.@A-��A-W�A-�A,�XA,C�A+ѷA*��A)��A(��A'S�A&n�A%�A%J�A$��A#cA"خA!�A ��A �uA7�A�A�CA3�A~A��Al"A�\A�A]dA��A��A�A;dA)�A+A�A��AVA\�A �A�aA
-�A	�=A��A�?AS�A\)Ag8A�rAH�AM�A ��A 7L@�!�@���@�u%@��2@��@��@�[@� i@��@�@�Q�@�(@�1@�A�@�@���@�F@�&�@�$t@�"h@睲@��,@��@��@��@�z�@�6�@��g@�@O@��@�x�@ޱ�@��@�\)@܋D@���@�x@�<�@�~�@Ӂ@��@���@ѷ�@ѧ�@ы�@�iD@�6z@�V@��&@η�@���@�~�@��@ʰ!@�-�@ȔF@Ǿw@ǂ�@�8�@��@�p;@��A@�U�@�e,@��@�!-@��A@�@��M@���@���@�~�@�ff@�K^@�7@��@�X@��H@�ff@���@���@�?�@�$@� �@��@@�O�@�7L@�*0@���@�R�@���@�qv@��@���@�O@��:@�&@���@�6@�o�@��c@� �@��@�Xy@��C@��|@���@���@�/@���@��b@�4@���@�'�@�@���@���@��r@�J�@���@��b@��N@���@��'@��X@���@���@�a|@�+k@���@�a|@�W?@��R@��@�q@�q@�_@�GE@� �@���@��~@�a�@�K�@��@���@���@��e@��@���@�{�@��a@�=@���@��_@��@��x@�s�@�'R@��A@��Q@��@�{J@�o@��@�u�@�Z�@�R�@�M�@�H�@�:*@�-@�~@�_@��@��m@���@���@��
@�t�@�9�@��@�ff@���@���@���@��~@�*0@��6@�~(@�J@��6@�7@��@���@���@�|@�e,@�H�@�7L@�$t@��@�V@� i@��|@�֡@�}V@�a�@��@���@�@���@���@���@�$�@��T@���@� \@���@�]d@�	�@���@��{@�hs@�T�@���@�z�@�I�@�;�@�:*@�:�@�6@�+k@�{@�:@4�@~��@~�@~��@~i�@~J�@}�@}��@}u�@}2a@|�@|�@|]d@{�6@z)�@yԕ@y�d@y��@y�^@y�@y�t@y��@y*0@x�z@xV�@x	�@w��@wn/@w=@w�@w�@v҉@vff@v+k@u�@u�z@u�~@u \@tی@t��@t�D@t �@s��@s��@sX�@r��@q�'@q�@p�)@p�z@p��@pU2@p"h@p7@o��@oj�@o�@n��@n	@mc@m?}@l�@ltT@l7�@l@k�w@j�@i \@h��@h�U@h��@hr�@hK^@g��@g��@gC@f��@fff@f($@e��@e�7@ehs@eL�@d��@c�@c�[@ciD@b�<@bs�@b5?@a�@a�@a�'@a�@ao @aY�@aF@a�@`�f@`�9@_��@_E9@^�"@^��@^��@^��@]�S@]S&@]!�@\�P@\�@\�`@\��@\�@\m�@\PH@\'R@\~@[��@[C�@ZE�@Y&�@XS�@X�@W��@W�@W�f@Wy�@Wb�@WE9@W�@V��@V�@U�S@U=�@T�v@T�.@T�@St�@Sj�@R��@R�@Q�H@Qc@Q}�@Q}�@Qc�@PɆ@P<�@O�K@O9�@N��@M�@M�@L��@L@K�Q@K�g@K�a@K��@KS�@KMj@KMj@KE9@K6z@K/�@K!-@K@J��@Jc @Jc @J6�@I��@I�@I�3@I�t@I�C@I��@I�~@Ik�@I?}@I�@H�@H'R@G�Q@G��@GRT@F��@FC�@E�@E�M@ES&@E�@D_@C+@Bh
@B�@A��@A�-@Ae,@A@@@�@@��@@z�@@]d@@I�@@C-@@7�@@/�@@7@@1@?��@?�@?j�@?Mj@>ߤ@>��@>�L@>�@>��@>�@>M�@=�^@=/@=@= \@=!�@=&�@=&�@=/@<��@<U2@<]d@<Z@;P�@:��@:�+@:0U@9�o@9�C@8��@8z�@7��@7g�@7$t@7�@6��@6�@6҉@6��@6�@6�R@6�b@6��@6d�@6L0@6E�@61�@5�o@5�3@5�~@5�@5}�@5m]@5Q�@5(�@4�@4�$@3�k@2�@1�h@1k�@1f�@1\�@1Q�@1=�@1@0�|@0��@/�W@/+@/�@.��@.�}@.�L@.��@.��@.Ta@.8�@.-@.#:@.$�@.�@.�@-�D@-�o@-�@-`B@-|@-x�@-T�@,�P@,֡@,Ĝ@,�$@,�z@,I�@+�Q@+��@+��@+�4@+�4@*��@*Z�@*Ta@*:*@)L�@(U2@'��@'+@'�@&��@&#:@& �@%�@%�@%��@%�@$��@$u�@$N�@$*�@#��@#�@#g�@#"�@"�@"��@"H�@!��@!��@!��@!�n@!��@!|@!F@!&�@!@@!�@ �@ Ɇ@ �@ ��@ �Y@ ~(@ e�@ 1@	@}�@Vm@O�@?}@5�@+�@�@%@�D@�W@˒@�f@�@S@�2@��@�@�<@�R@�h@��@��@{�@p;@_�@6�@�@��@��@�j@`B@2a@+�@*0@+�@+�@-w@2a@0�@2a@4@5�@#�@�@V@�@@�	@�5@��@�O@w�@'R@�P@��@YK@V@B[@=q@@�@8�@4@�@p�@ѷ@��@_@C-@-�@	�@�&@��@��@��@dZ@S�@4�@҉@8�@�@��@c@*0@�?@��@Z@�$@�$@�	@,�@�8@�2@��@�]@�H@�M@��@��@�"@�c@�b@��@��@��@�L@��@�r@�x@��@�@zx@u�@%@!@خ@��@�k@��@y�@j�@U�@
�]@
�@
v�@
1�@	�@	k�@�@�@��@��@K^@7@�Q@�[@��@��@y�@X�@6z@�y@i�@+k@�@�#@��@s�@Q�@0�@&�@#�@%F@!�@�@��@��@��@��@w�@u�@r�@bN@S�@6@�&@�*@A�@�@��@��@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�uA��A��A��A�IA�!-A�"hA�"4A�"�A�#nA�#�A�"hA�	A�+A��A��A�eA�A�CA��A��A��A�xA��A�CA�A�1A�YA��A�eA��A��A�A�bA��A��A��A�o A��A��A��RA�wfA�6A�IA�_A���A���A�Z�A�C�A�-CA�
	A�҉A���A�Y�A�uA��kA��'A��rA��tA�bA�uZA�'�A��<A���A���A�uA��fA���A��A��2A��,A���A��qA��A�W�A�H�A�бA�!bA��]A���A�Q�A���A�MA�E�A���A��+A��2A�B'A�YKA���A�FA�Q�A�dA�A~CA|�Ay�AxL�Aw:�Av0UAu�AurGAt��Aq��Ao�rAn��Al�cAk��Ai��AhAg4Af��Af4�Ad�Ab��Aa?�A_�fA^�4A]$tA[W�AZK�AY�NAY�AXK�AW�NAV��AVy>AV@�AV4nAV)_AV�AV�AUݘAU�jAU��AUFAS��ARG�AQy>AQJ#AP�)AP��AO�9AN�8AN�AM�IAL�:AK��AJ��AJ�FAJ�AI�HAFd�AD�FAC��ACZAB�ABC�AA��A@�"A?VA>\)A> �A=�A=͟A=��A<K�A:��A9�A9.IA7�uA6��A6��A6|�A6b�A5�|A51�A4z�A4�A3xlA1��A0��A/|A.�zA.MA.@A-��A-W�A-�A,�XA,C�A+ѷA*��A)��A(��A'S�A&n�A%�A%J�A$��A#cA"خA!�A ��A �uA7�A�A�CA3�A~A��Al"A�\A�A]dA��A��A�A;dA)�A+A�A��AVA\�A �A�aA
-�A	�=A��A�?AS�A\)Ag8A�rAH�AM�A ��A 7L@�!�@���@�u%@��2@��@��@�[@� i@��@�@�Q�@�(@�1@�A�@�@���@�F@�&�@�$t@�"h@睲@��,@��@��@��@�z�@�6�@��g@�@O@��@�x�@ޱ�@��@�\)@܋D@���@�x@�<�@�~�@Ӂ@��@���@ѷ�@ѧ�@ы�@�iD@�6z@�V@��&@η�@���@�~�@��@ʰ!@�-�@ȔF@Ǿw@ǂ�@�8�@��@�p;@��A@�U�@�e,@��@�!-@��A@�@��M@���@���@�~�@�ff@�K^@�7@��@�X@��H@�ff@���@���@�?�@�$@� �@��@@�O�@�7L@�*0@���@�R�@���@�qv@��@���@�O@��:@�&@���@�6@�o�@��c@� �@��@�Xy@��C@��|@���@���@�/@���@��b@�4@���@�'�@�@���@���@��r@�J�@���@��b@��N@���@��'@��X@���@���@�a|@�+k@���@�a|@�W?@��R@��@�q@�q@�_@�GE@� �@���@��~@�a�@�K�@��@���@���@��e@��@���@�{�@��a@�=@���@��_@��@��x@�s�@�'R@��A@��Q@��@�{J@�o@��@�u�@�Z�@�R�@�M�@�H�@�:*@�-@�~@�_@��@��m@���@���@��
@�t�@�9�@��@�ff@���@���@���@��~@�*0@��6@�~(@�J@��6@�7@��@���@���@�|@�e,@�H�@�7L@�$t@��@�V@� i@��|@�֡@�}V@�a�@��@���@�@���@���@���@�$�@��T@���@� \@���@�]d@�	�@���@��{@�hs@�T�@���@�z�@�I�@�;�@�:*@�:�@�6@�+k@�{@�:@4�@~��@~�@~��@~i�@~J�@}�@}��@}u�@}2a@|�@|�@|]d@{�6@z)�@yԕ@y�d@y��@y�^@y�@y�t@y��@y*0@x�z@xV�@x	�@w��@wn/@w=@w�@w�@v҉@vff@v+k@u�@u�z@u�~@u \@tی@t��@t�D@t �@s��@s��@sX�@r��@q�'@q�@p�)@p�z@p��@pU2@p"h@p7@o��@oj�@o�@n��@n	@mc@m?}@l�@ltT@l7�@l@k�w@j�@i \@h��@h�U@h��@hr�@hK^@g��@g��@gC@f��@fff@f($@e��@e�7@ehs@eL�@d��@c�@c�[@ciD@b�<@bs�@b5?@a�@a�@a�'@a�@ao @aY�@aF@a�@`�f@`�9@_��@_E9@^�"@^��@^��@^��@]�S@]S&@]!�@\�P@\�@\�`@\��@\�@\m�@\PH@\'R@\~@[��@[C�@ZE�@Y&�@XS�@X�@W��@W�@W�f@Wy�@Wb�@WE9@W�@V��@V�@U�S@U=�@T�v@T�.@T�@St�@Sj�@R��@R�@Q�H@Qc@Q}�@Q}�@Qc�@PɆ@P<�@O�K@O9�@N��@M�@M�@L��@L@K�Q@K�g@K�a@K��@KS�@KMj@KMj@KE9@K6z@K/�@K!-@K@J��@Jc @Jc @J6�@I��@I�@I�3@I�t@I�C@I��@I�~@Ik�@I?}@I�@H�@H'R@G�Q@G��@GRT@F��@FC�@E�@E�M@ES&@E�@D_@C+@Bh
@B�@A��@A�-@Ae,@A@@@�@@��@@z�@@]d@@I�@@C-@@7�@@/�@@7@@1@?��@?�@?j�@?Mj@>ߤ@>��@>�L@>�@>��@>�@>M�@=�^@=/@=@= \@=!�@=&�@=&�@=/@<��@<U2@<]d@<Z@;P�@:��@:�+@:0U@9�o@9�C@8��@8z�@7��@7g�@7$t@7�@6��@6�@6҉@6��@6�@6�R@6�b@6��@6d�@6L0@6E�@61�@5�o@5�3@5�~@5�@5}�@5m]@5Q�@5(�@4�@4�$@3�k@2�@1�h@1k�@1f�@1\�@1Q�@1=�@1@0�|@0��@/�W@/+@/�@.��@.�}@.�L@.��@.��@.Ta@.8�@.-@.#:@.$�@.�@.�@-�D@-�o@-�@-`B@-|@-x�@-T�@,�P@,֡@,Ĝ@,�$@,�z@,I�@+�Q@+��@+��@+�4@+�4@*��@*Z�@*Ta@*:*@)L�@(U2@'��@'+@'�@&��@&#:@& �@%�@%�@%��@%�@$��@$u�@$N�@$*�@#��@#�@#g�@#"�@"�@"��@"H�@!��@!��@!��@!�n@!��@!|@!F@!&�@!@@!�@ �@ Ɇ@ �@ ��@ �Y@ ~(@ e�@ 1@	@}�@Vm@O�@?}@5�@+�@�@%@�D@�W@˒@�f@�@S@�2@��@�@�<@�R@�h@��@��@{�@p;@_�@6�@�@��@��@�j@`B@2a@+�@*0@+�@+�@-w@2a@0�@2a@4@5�@#�@�@V@�@@�	@�5@��@�O@w�@'R@�P@��@YK@V@B[@=q@@�@8�@4@�@p�@ѷ@��@_@C-@-�@	�@�&@��@��@��@dZ@S�@4�@҉@8�@�@��@c@*0@�?@��@Z@�$@�$@�	@,�@�8@�2@��@�]@�H@�M@��@��@�"@�c@�b@��@��@��@�L@��@�r@�x@��@�@zx@u�@%@!@خ@��@�k@��@y�@j�@U�@
�]@
�@
v�@
1�@	�@	k�@�@�@��@��@K^@7@�Q@�[@��@��@y�@X�@6z@�y@i�@+k@�@�#@��@s�@Q�@0�@&�@#�@%F@!�@�@��@��@��@��@w�@u�@r�@bN@S�@6@�&@�*@A�@�@��@��@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B'�B(>B(
B'�B($B(
B(
B(>B(
B(
B'�B'�B'�B&�B'mB'RB'�B'�B'�B'B&B&2B%�B%zB%B$�B%B$ZB$&B$@B$tB$@B#�B# B"hB BB�BB�B�B�(BqvBj�Be`BcTBV�B+kB;B�B�BYB�BbB0B�B �B��B�)BѷBĶB�fB��B�`B�!B�+B�BuB\xBV�BM�BJ�BH�BGBE9B.IB BB<B�6B��B��B�IB�TB�B��B�%BtBk�Bc�BXyBJrB2GBB�B B
��B
��B
�mB
��B
�vB
�B
B
�wB
�qB
�JB
�;B
�pB
��B
��B
�}B
��B
~]B
v`B
s�B
o�B
e�B
_;B
PB
GEB
=B
5tB
/5B
(�B
'�B
%�B
!HB
!B
	B
�B
�B
B
�B
�B
�B
�B
hB
�B
6B
B	��B	��B	�*B	�8B	�zB	�B	�GB	��B	�B	�B	�nB	�jB	��B	ںB	�B	ϑB	��B	��B	�.B	��B	��B	��B	��B	�qB	��B	��B	��B	��B	��B	�:B	�KB	�gB	��B	��B	��B	��B	�	B	��B	�7B	��B	�AB	~�B	~BB	w�B	s�B	nB	l�B	jeB	h�B	h�B	g8B	e`B	c�B	bhB	^�B	]B	VSB	U2B	N�B	K�B	HKB	FYB	C-B	?�B	<jB	9�B	5tB	2B	.}B	*KB	"�B	�B	�B	�B	]B	YB	�B	oB	�B	pB	�B	 B�cB��B�]B��B�jB��B��B�`B�B�B�B��B�B��B�B��BߤB޸B�B��B��B�B��B�SB��BՁB�[B�TB��B�NBуB�}B��B�\B�BB��BοBΥB��B�BBΥBΥB��B�pB��B�B�B�~B��B��B�0B��B�0B��BˬB�rB��B�PB��B�HB� B�oBҽBҽB��BҽB�:B�B�&B�{B�YB��B�$B֡B�BخBخBخB��B��B�)B�~B��B�4B��B�B�B��B�@B�B��B�B�2B�2B�fB�B��B�B��B�2B��B�8B�lB��B�DB��B�JB�JB�B�B��B	  B	 �B	 B	�B	�B	B	SB	B		B		�B	0B	<B	�B	�B	SB	B	!B	#TB	%�B	'B	)�B	+�B	,�B	-B	.�B	/�B	/OB	0UB	/�B	1�B	1vB	2�B	:�B	D�B	KDB	P�B	QNB	R B	S�B	\)B	abB	fB	ffB	h
B	g�B	h$B	h�B	i_B	kQB	k�B	l�B	mB	nIB	nB	o B	oiB	s�B	y>B	z�B	~�B	��B	��B	�B	��B	�@B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�nB	�nB	��B	��B	�B	�ZB	��B	�,B	�fB	�B	��B	��B	��B	�B	��B	�eB	��B	��B	�JB	�;B	āB	�tB	ɺB	��B	уB	یB	�~B	��B	�;B	ߤB	�vB	�B	�B	��B	�TB	�B	�B	�B	�B	��B	�fB	��B	�[B	�MB	�nB	��B	��B	��B
 iB
-B
�B
	RB
xB
�B
 B
�B
9B
�B
�B
5B
"NB
#�B
$&B
$&B
$&B
$ZB
$�B
%B
'mB
(�B
)_B
*0B
*�B
+6B
+�B
,�B
-�B
.cB
/�B
0�B
1'B
2�B
4nB
9rB
9�B
:B
:DB
:DB
:DB
:DB
:�B
;�B
=qB
>(B
>�B
@�B
@�B
AUB
BAB
B�B
C�B
E�B
FtB
GzB
H1B
I�B
J�B
K�B
LB
L�B
NVB
O\B
P.B
P�B
S�B
W�B
YB
Z7B
Z�B
[	B
[�B
\]B
\xB
]B
^�B
_VB
`�B
b�B
c�B
d�B
e�B
f�B
g�B
g�B
h�B
nB
p;B
p�B
q'B
q�B
rB
r|B
s�B
t�B
vB
wLB
xB
x�B
y$B
zDB
z�B
z�B
}B
~�B
�B
��B
��B
��B
��B
��B
�%B
��B
��B
�+B
�zB
��B
�B
��B
�lB
�B
�VB
�(B
�vB
��B
�}B
�B
��B
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�	B
��B
��B
��B
��B
�B
�-B
�bB
��B
��B
�4B
�TB
�@B
��B
��B
��B
��B
�kB
�)B
��B
��B
�UB
�'B
��B
��B
��B
�-B
�hB
��B
��B
��B
�RB
�*B
�6B
��B
�wB
��B
��B
��B
�.B
��B
��B
��B
� B
�B
�4B
�4B
�OB
��B
��B
�B
B
�B
�GB
�aB
�{B
ÖB
ðB
ðB
�B
ĜB
��B
�SB
�EB
ǮB
��B
��B
ʦB
�xB
�dB
�6B
̈́B
��B
�BB
ѷB
�B
өB
��B
�B
�{B
�B
յB
�B
�SB
ևB
֡B
ּB
��B
��B
�$B
�$B
��B
��B
�+B
�EB
��B
��B
�1B
�B
��B
��B
ٚB
��B
�)B
�xB
�]B
�]B
�CB
�]B
��B
ބB
�!B
�B
ޞB
�|B
�B
�B
�nB
�B
�ZB
��B
�B
�B
��B
�sB
�sB
�B
��B
��B
�*B
�*B
�*B
�DB
�B
�B
��B
��B
�B
�B
��B
�QB
�kB
�kB
�B
�B
��B
�B
��B
�IB
�B
�B
��B
��B
��B
��B
��B
�-B
�GB
�B
�B
��B
��B
�tB
��B
��B
��B
��B
�+B
�`B
�FB
�`B
�FB
�`B
�FB
�zB
�`B
��B
�B
��B
��B
�LB
�B
�lB
�lB
�lB
�lB
�>B
��B
�^B
�^B
�DB
�B
��B
��B
��B
��B
��B
�(B
�}B  B B �BoB�B�B�B�B�BGB{B{B{B�B3BMB�BB�B%B�BzBzBzB_B�B�BB1BKB�B�B�B	B	7B	7B	7B	�BdBdB�B�B�B�B�B�B�B�BVBVB�B(BvB�BB.B�B}B�B�B BB4BhB�BB:B�BTB�B�B�B�B�B�B�B�B�B�B�B�BB,B,BFBFBFBaB{B�BBgBB�B�B�B�B�B�B�B+B_B�B�BQB�B�B�B	B=BqB�B�B�B�B�BxBIB�BBB�B;BpB�B!B �B �B!-B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!|B!�B"NB"4B!�B"NB#:B$@B$�B%,B%,B%`B%�B%�B&�B&�B&�B&�B&�B'�B(XB(�B(�B(�B)DB)yB)�B*0B*KB*KB*B*�B*�B+B,B,WB,qB,�B-)B-wB-�B-�B.B./B.B.B.IB.}B.�B.�B/iB/�B/�B/�B/�B/�B/�B0;B0�B1[B1AB1vB1�B1�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B'�B(>B(
B'�B($B(
B(
B(>B(
B(
B'�B'�B'�B&�B'mB'RB'�B'�B'�B'B&B&2B%�B%zB%B$�B%B$ZB$&B$@B$tB$@B#�B# B"hB BB�BB�B�B�(BqvBj�Be`BcTBV�B+kB;B�B�BYB�BbB0B�B �B��B�)BѷBĶB�fB��B�`B�!B�+B�BuB\xBV�BM�BJ�BH�BGBE9B.IB BB<B�6B��B��B�IB�TB�B��B�%BtBk�Bc�BXyBJrB2GBB�B B
��B
��B
�mB
��B
�vB
�B
B
�wB
�qB
�JB
�;B
�pB
��B
��B
�}B
��B
~]B
v`B
s�B
o�B
e�B
_;B
PB
GEB
=B
5tB
/5B
(�B
'�B
%�B
!HB
!B
	B
�B
�B
B
�B
�B
�B
�B
hB
�B
6B
B	��B	��B	�*B	�8B	�zB	�B	�GB	��B	�B	�B	�nB	�jB	��B	ںB	�B	ϑB	��B	��B	�.B	��B	��B	��B	��B	�qB	��B	��B	��B	��B	��B	�:B	�KB	�gB	��B	��B	��B	��B	�	B	��B	�7B	��B	�AB	~�B	~BB	w�B	s�B	nB	l�B	jeB	h�B	h�B	g8B	e`B	c�B	bhB	^�B	]B	VSB	U2B	N�B	K�B	HKB	FYB	C-B	?�B	<jB	9�B	5tB	2B	.}B	*KB	"�B	�B	�B	�B	]B	YB	�B	oB	�B	pB	�B	 B�cB��B�]B��B�jB��B��B�`B�B�B�B��B�B��B�B��BߤB޸B�B��B��B�B��B�SB��BՁB�[B�TB��B�NBуB�}B��B�\B�BB��BοBΥB��B�BBΥBΥB��B�pB��B�B�B�~B��B��B�0B��B�0B��BˬB�rB��B�PB��B�HB� B�oBҽBҽB��BҽB�:B�B�&B�{B�YB��B�$B֡B�BخBخBخB��B��B�)B�~B��B�4B��B�B�B��B�@B�B��B�B�2B�2B�fB�B��B�B��B�2B��B�8B�lB��B�DB��B�JB�JB�B�B��B	  B	 �B	 B	�B	�B	B	SB	B		B		�B	0B	<B	�B	�B	SB	B	!B	#TB	%�B	'B	)�B	+�B	,�B	-B	.�B	/�B	/OB	0UB	/�B	1�B	1vB	2�B	:�B	D�B	KDB	P�B	QNB	R B	S�B	\)B	abB	fB	ffB	h
B	g�B	h$B	h�B	i_B	kQB	k�B	l�B	mB	nIB	nB	o B	oiB	s�B	y>B	z�B	~�B	��B	��B	�B	��B	�@B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�nB	�nB	��B	��B	�B	�ZB	��B	�,B	�fB	�B	��B	��B	��B	�B	��B	�eB	��B	��B	�JB	�;B	āB	�tB	ɺB	��B	уB	یB	�~B	��B	�;B	ߤB	�vB	�B	�B	��B	�TB	�B	�B	�B	�B	��B	�fB	��B	�[B	�MB	�nB	��B	��B	��B
 iB
-B
�B
	RB
xB
�B
 B
�B
9B
�B
�B
5B
"NB
#�B
$&B
$&B
$&B
$ZB
$�B
%B
'mB
(�B
)_B
*0B
*�B
+6B
+�B
,�B
-�B
.cB
/�B
0�B
1'B
2�B
4nB
9rB
9�B
:B
:DB
:DB
:DB
:DB
:�B
;�B
=qB
>(B
>�B
@�B
@�B
AUB
BAB
B�B
C�B
E�B
FtB
GzB
H1B
I�B
J�B
K�B
LB
L�B
NVB
O\B
P.B
P�B
S�B
W�B
YB
Z7B
Z�B
[	B
[�B
\]B
\xB
]B
^�B
_VB
`�B
b�B
c�B
d�B
e�B
f�B
g�B
g�B
h�B
nB
p;B
p�B
q'B
q�B
rB
r|B
s�B
t�B
vB
wLB
xB
x�B
y$B
zDB
z�B
z�B
}B
~�B
�B
��B
��B
��B
��B
��B
�%B
��B
��B
�+B
�zB
��B
�B
��B
�lB
�B
�VB
�(B
�vB
��B
�}B
�B
��B
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�	B
��B
��B
��B
��B
�B
�-B
�bB
��B
��B
�4B
�TB
�@B
��B
��B
��B
��B
�kB
�)B
��B
��B
�UB
�'B
��B
��B
��B
�-B
�hB
��B
��B
��B
�RB
�*B
�6B
��B
�wB
��B
��B
��B
�.B
��B
��B
��B
� B
�B
�4B
�4B
�OB
��B
��B
�B
B
�B
�GB
�aB
�{B
ÖB
ðB
ðB
�B
ĜB
��B
�SB
�EB
ǮB
��B
��B
ʦB
�xB
�dB
�6B
̈́B
��B
�BB
ѷB
�B
өB
��B
�B
�{B
�B
յB
�B
�SB
ևB
֡B
ּB
��B
��B
�$B
�$B
��B
��B
�+B
�EB
��B
��B
�1B
�B
��B
��B
ٚB
��B
�)B
�xB
�]B
�]B
�CB
�]B
��B
ބB
�!B
�B
ޞB
�|B
�B
�B
�nB
�B
�ZB
��B
�B
�B
��B
�sB
�sB
�B
��B
��B
�*B
�*B
�*B
�DB
�B
�B
��B
��B
�B
�B
��B
�QB
�kB
�kB
�B
�B
��B
�B
��B
�IB
�B
�B
��B
��B
��B
��B
��B
�-B
�GB
�B
�B
��B
��B
�tB
��B
��B
��B
��B
�+B
�`B
�FB
�`B
�FB
�`B
�FB
�zB
�`B
��B
�B
��B
��B
�LB
�B
�lB
�lB
�lB
�lB
�>B
��B
�^B
�^B
�DB
�B
��B
��B
��B
��B
��B
�(B
�}B  B B �BoB�B�B�B�B�BGB{B{B{B�B3BMB�BB�B%B�BzBzBzB_B�B�BB1BKB�B�B�B	B	7B	7B	7B	�BdBdB�B�B�B�B�B�B�B�BVBVB�B(BvB�BB.B�B}B�B�B BB4BhB�BB:B�BTB�B�B�B�B�B�B�B�B�B�B�B�BB,B,BFBFBFBaB{B�BBgBB�B�B�B�B�B�B�B+B_B�B�BQB�B�B�B	B=BqB�B�B�B�B�BxBIB�BBB�B;BpB�B!B �B �B!-B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!|B!�B"NB"4B!�B"NB#:B$@B$�B%,B%,B%`B%�B%�B&�B&�B&�B&�B&�B'�B(XB(�B(�B(�B)DB)yB)�B*0B*KB*KB*B*�B*�B+B,B,WB,qB,�B-)B-wB-�B-�B.B./B.B.B.IB.}B.�B.�B/iB/�B/�B/�B/�B/�B/�B0;B0�B1[B1AB1vB1�B1�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230303154254  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230303154257  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230303154258  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230303154259                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230303154259  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230303154259  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230303155650                      G�O�G�O�G�O�                