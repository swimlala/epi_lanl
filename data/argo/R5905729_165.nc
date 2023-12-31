CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-11-02T09:01:42Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߜ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20221102090142  20221102090142  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���P�J�1   @����$�@+5\(��d���+1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BV��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B���B�ffB���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'34B/��B7��B?��BG��BO��BVfgB_��Bg��Bo��Bw��B��B���B���B���B�  B�  B���B���B���B���B���B���B�fgB�33B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D+  D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6�4D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA�4DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�9�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�C3D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�^5A�`BA�bNA�dZA�dZA�ffA�hsA�bNA�E�A�(�A�bA��A��`A��;A��/A��/A��/A��HA��`A��yA��A��A��A���A��A��
AޑhA�9XA�^5A��/A�p�A�`BA��`AځA��TAذ!A��A��;A��;AѓuA�XA�p�A�jA���A�jA���A�\)A�=qA��A��uA�^5A�K�A��^A�7LA���A��uA�G�A��PA�+A���A�l�A�7LA�S�A��A�|�A�9XA�A��A�5?A���A���A��A�G�A�E�A���A�$�Az1'As;dAp�DAl��Ah{Af�jAe`BAb�/A`�A\�+A[AY��AW&�AT�RAS`BAR�AQXAO��AM��AJJAE�AB�A=
=A61A2Q�A1ƨA0�A/dZA-�^A-7LA,�9A+�A+�7A+�A)��A(��A(VA(I�A(�A'�A'7LA&�A&�DA%A%7LA$��A$jA#�mA#��A#dZA#�A"I�A!+A {AXA��Av�A�Ax�AĜA$�AƨAXA��A�DA5?A�A��A��A$�A�wA��Al�A��A��AbA��A�AG�A��A�RA�A��Ap�A?}A��A�Az�A5?A�A�FAK�A�9AE�A��A��A�A��A(�A�
A��A&�A
M�A	��A	|�A��A9XA�A�^Al�A/A��A�\A�\A�+A^5AbAx�AG�A�+A�A��AS�A"�A
=A�`AȴA��A-A�TA�A&�A �\@��@�x�@�&�@��@��@��@���@��@� �@���@��@�t�@�"�@�{@��9@��P@�M�@�I�@�R@�-@��@�F@�S�@�G�@�Q�@���@�S�@�C�@�M�@���@�O�@���@䛦@�j@�Q�@�1@�+@�ȴ@�5?@�X@�j@�\)@ޏ\@��@�A�@۾w@�
=@ڏ\@�@��@�  @��#@Ցh@Չ7@Չ7@Չ7@�?}@���@Ӆ@ҏ\@���@ёh@�X@�?}@�?}@�7L@�7L@�&�@�hs@��#@љ�@�7L@У�@�1@���@ϝ�@�+@Ο�@ͺ^@�X@��`@̋D@�I�@���@˾w@ˍP@�;d@�ȴ@��@���@�p�@�/@�%@ȃ@�A�@�|�@��@���@�ȴ@Ɵ�@Ƈ+@�n�@�ff@�ff@�^5@�J@��T@�/@�z�@�9X@��@��
@�
=@�~�@�E�@�=q@��@���@�O�@��@�%@���@���@��9@� �@��w@�;d@��!@�=q@��T@���@���@��h@�hs@���@���@���@�Ĝ@��j@��@��@�K�@��y@�~�@�E�@�5?@�5?@��T@�X@��@��`@���@��@�j@�1@�l�@��R@�{@��7@�X@�G�@�%@�Ĝ@�b@�S�@���@�E�@�hs@��@��m@���@�;d@��y@��R@��\@�=q@��@��@�@���@�&�@�z�@���@���@���@�S�@�@��!@�V@�$�@��^@��`@�Q�@���@���@�t�@�33@��@��!@�J@��^@���@�?}@�/@�V@���@��9@�(�@���@�C�@�@���@�v�@�^5@�-@���@�/@�I�@��m@�ƨ@��@��P@��@�o@�
=@��H@���@���@���@�~�@�-@��7@�/@�Ĝ@��@�A�@�b@��
@���@�\)@�@��@��H@���@�E�@�$�@��T@�x�@�X@���@��D@�z�@�bN@�A�@� �@��@��@�t�@�+@�"�@�
=@���@��y@�ȴ@���@���@�~�@�-@���@�X@�V@�V@�%@���@�Ĝ@��D@��@�r�@�Z@�A�@�(�@�1@��m@��P@�"�@�"�@��@�
=@���@�5?@���@�O�@�%@��9@���@���@���@���@��u@��u@��@�bN@��@�C�@�C�@�33@�"�@�o@��@���@�$�@���@�x�@��`@��u@�A�@��@��@��
@��@�t�@�dZ@�S�@�"�@�
=@�V@���@���@�hs@�/@�%@���@��@���@�bN@�  @��@�
=@��\@���@�V@��j@�Z@��@��
@�K�@�+@���@��R@��T@�&�@��/@��j@��@���@��@�j@�bN@�9X@�  @|�@
=@~5?@}��@}�-@}��@}O�@}�@|��@|�D@|9X@{�F@{��@{dZ@z��@zJ@yG�@xĜ@xr�@xb@w��@w��@w�w@wl�@w+@v�y@v�R@v��@vV@u`B@t�@t9X@sdZ@r�\@rM�@q�@q��@qhs@q�@p�u@p �@o�@ol�@n�y@nv�@mp�@l��@lZ@kƨ@kdZ@j�@j=q@i�@h1'@g��@g�P@g
=@f$�@ep�@e/@d��@d�@c�m@c�F@cC�@b�H@b�\@bM�@a��@a��@a��@ahs@a%@`�9@`Q�@_�@_�w@_K�@^�R@^�+@^5?@]�@]V@\��@\I�@[�m@[S�@Z�H@Z�\@ZM�@Z-@Y��@Y%@X��@X��@X�`@X��@XQ�@W�@W�;@W�w@Wl�@W+@W
=@V�R@V��@V$�@U`B@T��@T��@Tz�@T�@S�
@S��@SC�@So@S33@R�@R��@R�!@Rn�@RJ@Qx�@P�`@P�9@P1'@O�w@O|�@Ol�@O+@O
=@N��@N�@N��@Nff@N$�@M��@M`B@MV@L�D@L9X@K�
@K33@J�!@J~�@J=q@I��@IX@IG�@I%@H�`@HĜ@HQ�@H �@G��@G�@G�P@G�@F�R@Fff@F$�@F5?@E��@D�D@C�m@C�
@C��@B�!@A��@A�#@AX@@�`@@�9@@1'@?��@?+@>��@>E�@=�T@=�@=?}@<�@<z�@<I�@;�@;S�@;o@:�H@:�!@:�!@:�\@:~�@:n�@:M�@9�@9�^@9x�@9&�@8�9@8r�@8A�@7��@7l�@7
=@6v�@65?@6{@5�T@5p�@4�/@4j@4(�@3ƨ@3@2�!@2n�@2J@1�^@1hs@0��@0��@0Ĝ@0Ĝ@0��@0�u@0�@0r�@0bN@0 �@/�;@/��@/l�@/�@.�y@.��@.�y@.�y@.�@.�R@.v�@.@-��@-O�@-�@,�j@,1@+ƨ@+S�@+@*�\@*M�@*�@)��@)��@)hs@)G�@)%@(��@(A�@(b@'��@'|�@';d@'�@&ȴ@&�+@&V@&@%��@%�h@%/@%�@%V@$��@$�@$�/@$��@$��@$�@$Z@$(�@$�@#ƨ@#t�@#C�@#"�@#o@"�H@"�\@"n�@"M�@"=q@"J@!�@!�^@!�7@!x�@!x�@!x�@!hs@!G�@!�@ ��@ Ĝ@ �9@ �9@ �9@ Q�@  �@��@�P@|�@l�@\)@K�@K�@K�@+@�y@�@�@��@E�@$�@��@p�@/@��@��@��@��@��@�j@��@z�@I�@1@��@�
@ƨ@��@dZ@@��@�\@^5@=q@�@�@��@��@x�@&�@�@��@��@�u@r�@A�@�@�;@��@\)@;d@��@�@�@��@V@�@��@@@�-@�@`B@O�@O�@?}@�@�D@j@Z@I�@9X@�@1@�m@ƨ@��@dZ@33@o@@@�@�@�H@��@~�@=q@J@�^@7L@��@�9@�@A�@  @�@��@;d@�@
=@�y@�@��@ff@E�@{@��@�h@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ZA�^5A�`BA�bNA�dZA�dZA�ffA�hsA�bNA�E�A�(�A�bA��A��`A��;A��/A��/A��/A��HA��`A��yA��A��A��A���A��A��
AޑhA�9XA�^5A��/A�p�A�`BA��`AځA��TAذ!A��A��;A��;AѓuA�XA�p�A�jA���A�jA���A�\)A�=qA��A��uA�^5A�K�A��^A�7LA���A��uA�G�A��PA�+A���A�l�A�7LA�S�A��A�|�A�9XA�A��A�5?A���A���A��A�G�A�E�A���A�$�Az1'As;dAp�DAl��Ah{Af�jAe`BAb�/A`�A\�+A[AY��AW&�AT�RAS`BAR�AQXAO��AM��AJJAE�AB�A=
=A61A2Q�A1ƨA0�A/dZA-�^A-7LA,�9A+�A+�7A+�A)��A(��A(VA(I�A(�A'�A'7LA&�A&�DA%A%7LA$��A$jA#�mA#��A#dZA#�A"I�A!+A {AXA��Av�A�Ax�AĜA$�AƨAXA��A�DA5?A�A��A��A$�A�wA��Al�A��A��AbA��A�AG�A��A�RA�A��Ap�A?}A��A�Az�A5?A�A�FAK�A�9AE�A��A��A�A��A(�A�
A��A&�A
M�A	��A	|�A��A9XA�A�^Al�A/A��A�\A�\A�+A^5AbAx�AG�A�+A�A��AS�A"�A
=A�`AȴA��A-A�TA�A&�A �\@��@�x�@�&�@��@��@��@���@��@� �@���@��@�t�@�"�@�{@��9@��P@�M�@�I�@�R@�-@��@�F@�S�@�G�@�Q�@���@�S�@�C�@�M�@���@�O�@���@䛦@�j@�Q�@�1@�+@�ȴ@�5?@�X@�j@�\)@ޏ\@��@�A�@۾w@�
=@ڏ\@�@��@�  @��#@Ցh@Չ7@Չ7@Չ7@�?}@���@Ӆ@ҏ\@���@ёh@�X@�?}@�?}@�7L@�7L@�&�@�hs@��#@љ�@�7L@У�@�1@���@ϝ�@�+@Ο�@ͺ^@�X@��`@̋D@�I�@���@˾w@ˍP@�;d@�ȴ@��@���@�p�@�/@�%@ȃ@�A�@�|�@��@���@�ȴ@Ɵ�@Ƈ+@�n�@�ff@�ff@�^5@�J@��T@�/@�z�@�9X@��@��
@�
=@�~�@�E�@�=q@��@���@�O�@��@�%@���@���@��9@� �@��w@�;d@��!@�=q@��T@���@���@��h@�hs@���@���@���@�Ĝ@��j@��@��@�K�@��y@�~�@�E�@�5?@�5?@��T@�X@��@��`@���@��@�j@�1@�l�@��R@�{@��7@�X@�G�@�%@�Ĝ@�b@�S�@���@�E�@�hs@��@��m@���@�;d@��y@��R@��\@�=q@��@��@�@���@�&�@�z�@���@���@���@�S�@�@��!@�V@�$�@��^@��`@�Q�@���@���@�t�@�33@��@��!@�J@��^@���@�?}@�/@�V@���@��9@�(�@���@�C�@�@���@�v�@�^5@�-@���@�/@�I�@��m@�ƨ@��@��P@��@�o@�
=@��H@���@���@���@�~�@�-@��7@�/@�Ĝ@��@�A�@�b@��
@���@�\)@�@��@��H@���@�E�@�$�@��T@�x�@�X@���@��D@�z�@�bN@�A�@� �@��@��@�t�@�+@�"�@�
=@���@��y@�ȴ@���@���@�~�@�-@���@�X@�V@�V@�%@���@�Ĝ@��D@��@�r�@�Z@�A�@�(�@�1@��m@��P@�"�@�"�@��@�
=@���@�5?@���@�O�@�%@��9@���@���@���@���@��u@��u@��@�bN@��@�C�@�C�@�33@�"�@�o@��@���@�$�@���@�x�@��`@��u@�A�@��@��@��
@��@�t�@�dZ@�S�@�"�@�
=@�V@���@���@�hs@�/@�%@���@��@���@�bN@�  @��@�
=@��\@���@�V@��j@�Z@��@��
@�K�@�+@���@��R@��T@�&�@��/@��j@��@���@��@�j@�bN@�9X@�  @|�@
=@~5?@}��@}�-@}��@}O�@}�@|��@|�D@|9X@{�F@{��@{dZ@z��@zJ@yG�@xĜ@xr�@xb@w��@w��@w�w@wl�@w+@v�y@v�R@v��@vV@u`B@t�@t9X@sdZ@r�\@rM�@q�@q��@qhs@q�@p�u@p �@o�@ol�@n�y@nv�@mp�@l��@lZ@kƨ@kdZ@j�@j=q@i�@h1'@g��@g�P@g
=@f$�@ep�@e/@d��@d�@c�m@c�F@cC�@b�H@b�\@bM�@a��@a��@a��@ahs@a%@`�9@`Q�@_�@_�w@_K�@^�R@^�+@^5?@]�@]V@\��@\I�@[�m@[S�@Z�H@Z�\@ZM�@Z-@Y��@Y%@X��@X��@X�`@X��@XQ�@W�@W�;@W�w@Wl�@W+@W
=@V�R@V��@V$�@U`B@T��@T��@Tz�@T�@S�
@S��@SC�@So@S33@R�@R��@R�!@Rn�@RJ@Qx�@P�`@P�9@P1'@O�w@O|�@Ol�@O+@O
=@N��@N�@N��@Nff@N$�@M��@M`B@MV@L�D@L9X@K�
@K33@J�!@J~�@J=q@I��@IX@IG�@I%@H�`@HĜ@HQ�@H �@G��@G�@G�P@G�@F�R@Fff@F$�@F5?@E��@D�D@C�m@C�
@C��@B�!@A��@A�#@AX@@�`@@�9@@1'@?��@?+@>��@>E�@=�T@=�@=?}@<�@<z�@<I�@;�@;S�@;o@:�H@:�!@:�!@:�\@:~�@:n�@:M�@9�@9�^@9x�@9&�@8�9@8r�@8A�@7��@7l�@7
=@6v�@65?@6{@5�T@5p�@4�/@4j@4(�@3ƨ@3@2�!@2n�@2J@1�^@1hs@0��@0��@0Ĝ@0Ĝ@0��@0�u@0�@0r�@0bN@0 �@/�;@/��@/l�@/�@.�y@.��@.�y@.�y@.�@.�R@.v�@.@-��@-O�@-�@,�j@,1@+ƨ@+S�@+@*�\@*M�@*�@)��@)��@)hs@)G�@)%@(��@(A�@(b@'��@'|�@';d@'�@&ȴ@&�+@&V@&@%��@%�h@%/@%�@%V@$��@$�@$�/@$��@$��@$�@$Z@$(�@$�@#ƨ@#t�@#C�@#"�@#o@"�H@"�\@"n�@"M�@"=q@"J@!�@!�^@!�7@!x�@!x�@!x�@!hs@!G�@!�@ ��@ Ĝ@ �9@ �9@ �9@ Q�@  �@��@�P@|�@l�@\)@K�@K�@K�@+@�y@�@�@��@E�@$�@��@p�@/@��@��@��@��@��@�j@��@z�@I�@1@��@�
@ƨ@��@dZ@@��@�\@^5@=q@�@�@��@��@x�@&�@�@��@��@�u@r�@A�@�@�;@��@\)@;d@��@�@�@��@V@�@��@@@�-@�@`B@O�@O�@?}@�@�D@j@Z@I�@9X@�@1@�m@ƨ@��@dZ@33@o@@@�@�@�H@��@~�@=q@J@�^@7L@��@�9@�@A�@  @�@��@;d@�@
=@�y@�@��@ff@E�@{@��@�h@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�VB
�PB
�PB
�PB
�VB
�VB
�VB
�PB
�JB
�\B
�hB
�uB
��B
��B
��B
��B
��B
��B
�B
�?B
�dB
ƨB
��B
�
B
�/B
�TB
�B
��BB$�B.B49B33B0!B>wB9XB�B �BPB
��B
��B'�B�%B�B�wB�fB�#BB�B�B�B�BB��B�TB��B��B�'B��B�1B�=By�Bt�BdZBQ�B;dB�BB
�B
ɺB
�qB
��B
t�B
cTB
6FB
  B	�B	�3B	�B	�{B	{�B	r�B	bNB	L�B	49B	#�B	#�B	!�B	oB		7B	+B	+B��B�B�BB��B�?B�-B�B�B�)B	oB	�B	�B	'�B	@�B	F�B	J�B	VB	W
B	VB	]/B	�+B	��B	��B	��B	��B	��B	��B	��B	�BB	�sB	�B	�B	�B	�B	�B	�mB	�mB	�B	�B	��B	�B	�sB	�HB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
%B
DB
DB
VB
hB
oB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
\B

=B
B	��B
  B	��B	��B	��B	�B	�sB	�NB	�BB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�BB	�HB	�BB	�/B	�)B	�B	�B	�B	��B	�/B	�BB	�;B	�5B	�#B	�B	�#B	�#B	�5B	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
	7B
	7B

=B
DB
DB
JB
JB
DB
	7B
DB

=B
JB
oB
uB
hB
\B
hB
{B
�B
{B
hB
oB
{B
�B
�B
{B
oB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
�B
�B
�B
�B
�B
uB
oB
oB
uB
bB
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
%�B
&�B
&�B
(�B
(�B
(�B
(�B
&�B
%�B
&�B
'�B
&�B
(�B
)�B
(�B
'�B
&�B
%�B
+B
.B
.B
.B
-B
/B
/B
/B
/B
0!B
0!B
.B
-B
,B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
2-B
1'B
/B
1'B
0!B
/B
1'B
/B
1'B
33B
33B
2-B
2-B
2-B
2-B
33B
2-B
49B
49B
49B
49B
49B
49B
49B
33B
1'B
0!B
33B
49B
6FB
6FB
5?B
49B
49B
6FB
6FB
5?B
5?B
5?B
5?B
5?B
49B
33B
7LB
6FB
5?B
33B
2-B
33B
6FB
7LB
8RB
9XB
:^B
:^B
:^B
9XB
9XB
8RB
7LB
5?B
9XB
;dB
;dB
:^B
:^B
9XB
8RB
7LB
9XB
8RB
9XB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
=qB
;dB
=qB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
@�B
>wB
?}B
>wB
>wB
<jB
=qB
>wB
=qB
>wB
>wB
>wB
@�B
?}B
>wB
;dB
>wB
A�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
E�B
F�B
F�B
E�B
F�B
F�B
E�B
F�B
G�B
H�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
J�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
H�B
I�B
K�B
J�B
K�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
O�B
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
O�B
Q�B
S�B
S�B
S�B
R�B
S�B
VB
VB
T�B
VB
VB
VB
VB
W
B
W
B
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
YB
ZB
[#B
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
aHB
aHB
aHB
aHB
`BB
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
hsB
gmB
hsB
iyB
hsB
gmB
hsB
jB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
o�B
p�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
�B
� B
�B
�B
� B
� B
� B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�=B
�DB
�DB
�DB
�=B
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�\B
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�uB
�{B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�VB
�PB
�PB
�PB
�VB
�VB
�VB
�PB
�JB
�\B
�hB
�uB
��B
��B
��B
��B
��B
��B
�B
�?B
�dB
ƨB
��B
�
B
�/B
�TB
�B
��BB$�B.B49B33B0!B>wB9XB�B �BPB
��B
��B'�B�%B�B�wB�fB�#BB�B�B�B�BB��B�TB��B��B�'B��B�1B�=By�Bt�BdZBQ�B;dB�BB
�B
ɺB
�qB
��B
t�B
cTB
6FB
  B	�B	�3B	�B	�{B	{�B	r�B	bNB	L�B	49B	#�B	#�B	!�B	oB		7B	+B	+B��B�B�BB��B�?B�-B�B�B�)B	oB	�B	�B	'�B	@�B	F�B	J�B	VB	W
B	VB	]/B	�+B	��B	��B	��B	��B	��B	��B	��B	�BB	�sB	�B	�B	�B	�B	�B	�mB	�mB	�B	�B	��B	�B	�sB	�HB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
%B
DB
DB
VB
hB
oB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
\B

=B
B	��B
  B	��B	��B	��B	�B	�sB	�NB	�BB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�BB	�HB	�BB	�/B	�)B	�B	�B	�B	��B	�/B	�BB	�;B	�5B	�#B	�B	�#B	�#B	�5B	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
	7B
	7B

=B
DB
DB
JB
JB
DB
	7B
DB

=B
JB
oB
uB
hB
\B
hB
{B
�B
{B
hB
oB
{B
�B
�B
{B
oB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
�B
�B
�B
�B
�B
uB
oB
oB
uB
bB
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
%�B
&�B
&�B
(�B
(�B
(�B
(�B
&�B
%�B
&�B
'�B
&�B
(�B
)�B
(�B
'�B
&�B
%�B
+B
.B
.B
.B
-B
/B
/B
/B
/B
0!B
0!B
.B
-B
,B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
2-B
1'B
/B
1'B
0!B
/B
1'B
/B
1'B
33B
33B
2-B
2-B
2-B
2-B
33B
2-B
49B
49B
49B
49B
49B
49B
49B
33B
1'B
0!B
33B
49B
6FB
6FB
5?B
49B
49B
6FB
6FB
5?B
5?B
5?B
5?B
5?B
49B
33B
7LB
6FB
5?B
33B
2-B
33B
6FB
7LB
8RB
9XB
:^B
:^B
:^B
9XB
9XB
8RB
7LB
5?B
9XB
;dB
;dB
:^B
:^B
9XB
8RB
7LB
9XB
8RB
9XB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
=qB
;dB
=qB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
@�B
>wB
?}B
>wB
>wB
<jB
=qB
>wB
=qB
>wB
>wB
>wB
@�B
?}B
>wB
;dB
>wB
A�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
E�B
F�B
F�B
E�B
F�B
F�B
E�B
F�B
G�B
H�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
J�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
H�B
I�B
K�B
J�B
K�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
O�B
O�B
P�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
O�B
Q�B
S�B
S�B
S�B
R�B
S�B
VB
VB
T�B
VB
VB
VB
VB
W
B
W
B
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
YB
ZB
[#B
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
aHB
aHB
aHB
aHB
`BB
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
iyB
iyB
hsB
gmB
hsB
iyB
hsB
gmB
hsB
jB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
o�B
p�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
�B
� B
�B
�B
� B
� B
� B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�=B
�DB
�DB
�DB
�=B
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�\B
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�bB
�bB
�bB
�bB
�hB
�bB
�bB
�oB
�oB
�oB
�oB
�oB
�hB
�oB
�oB
�oB
�uB
�{B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221102090142                              AO  ARCAADJP                                                                    20221102090142    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221102090142  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221102090142  QCF$                G�O�G�O�G�O�4000            