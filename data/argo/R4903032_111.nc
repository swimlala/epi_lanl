CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-08T09:00:50Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210708090050  20210708090050  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               oA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @قS*I�1   @قS�l �@;z��vȴ�c�I�^51   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         oA   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC  C�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Ds4D��Dy�D��Dy�D  Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D   D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<� D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D�� D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D�� D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D��3D��g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA��A�1ȂhAˁA�Q�A�"�A�ƨA�p�A�O�A�I�A�A�A�9XA�+A���A���Aɡ�AǏ\A���A��/A���A���A��A���A�1'A���A��#A��A��A�A��RA��+A�Q�A��RA��PA�1A��A��+A��+A�ffA�9XA���A�K�A���A�33A�t�A��A�7LA�ĜA�G�A���A�jA�=qA���A�t�A��A�?}A��-A�ZA� �A�M�A���A�bNA�ƨA�ZA��
A��A��;A�&�A�S�A�ĜA�  A� �A��-A�bNA���A�?}A�-A���A��wA��A�`BA���A�oA�A��FA�%A�1'A��7A�^5A��yA�jA���A�/A���A���A�t�A�$�A���A�&�A�dZA�z�A���A�M�A}�A|�A{�7Ax�/Av�\AudZAtjAr��ApbAm�;Al��AkC�Aix�Af�Ae33AdM�AbA_�TA^Q�A]��A\��A\-A[oAZA�AY`BAYoAXAWhsAV{AT~�AS�#AR^5AP�/AO�-AN��AM�;AKAJ�AI"�AH�yAH��AH��AF�/ADȴAC�PAB��AA��A@�HA?/A>jA=&�A;��A;/A:r�A9�A9oA8ĜA8�\A8E�A7��A7�wA7��A7hsA7oA6v�A5�-A3ƨA2�jA2�\A2VA1�A17LA01A/�PA.�`A-�TA,�yA,$�A+|�A*��A*(�A)��A)�PA)t�A)`BA)/A)VA(��A(�A'�PA'S�A%hsA%C�A%VA%
=A%A$�/A$Q�A#�-A#p�A"�yA!��A!t�A!7LA (�A�AI�A��Al�A�9AE�A�TA33A�A��A5?A33AĜA5?A�
A��A+AdZA�!A��A��A�mA7LA��A�A+AAA	;dA�A��A�DAz�AE�A�wAn�AƨA`BA�`A��AbNA1A��A��A@��F@�
=@�v�@���@�b@�+@�p�@���@�@��-@���@�O�@���@��@��y@�O�@�\)@�+@���@��@�ff@��@���@��@���@�+@�p�@��@�%@�bN@޸R@ݡ�@�ƨ@�E�@�?}@�~�@�I�@�\)@җ�@�O�@ϕ�@�?}@�  @ȼj@ŉ7@��m@�ȴ@�=q@�hs@���@��w@�M�@�p�@��/@�r�@�9X@� �@�  @���@�K�@�
=@�^5@�%@�  @�z�@��@�$�@��^@���@�A�@�ƨ@�"�@�$�@��7@�%@��@�33@���@�V@��-@�O�@�&�@��@��D@���@�33@�=q@���@�X@���@�Q�@���@��y@��+@��#@�x�@�7L@��/@�Q�@���@�o@�@���@�ff@��7@���@���@��@��@���@�M�@�J@���@���@�&�@��@���@��9@��D@��@���@�A�@��F@�S�@��@��+@�V@�=q@�{@�@�?}@�Ĝ@��!@���@��j@���@��D@��j@��@�r�@� �@���@�l�@��@��y@���@�E�@�{@��T@�x�@���@��D@�j@�Z@�Z@�1@�b@�  @�l�@�+@�@��@��R@�ff@�M�@�=q@�=q@��@�@���@��@�%@��@��@��D@�Q�@���@�=q@�$�@�@��-@��7@��@�x�@�x�@�G�@�/@��u@��@��D@�Z@�1@��m@�33@��!@���@���@���@�M�@���@��h@��@��@��@�`B@��@���@���@��@�j@�b@~��@}V@}/@}�@|�@|1@{dZ@z�H@z~�@zn�@z=q@z=q@z-@z-@z�@y��@y��@yx�@yX@y7L@x��@xr�@xbN@xbN@xbN@x �@w��@v�@v��@v��@v��@v��@v{@t�/@t(�@s��@sƨ@sƨ@s�F@s�F@s��@sdZ@r�H@rn�@r-@q�#@q�7@qhs@qG�@q�@p��@pĜ@p��@p��@p�u@q��@q��@p�u@p �@o+@n��@n�R@n��@mp�@m�@m�T@n5?@n�+@n5?@m�-@m��@m?}@m�@m�h@mV@l��@lz�@l1@k�
@k�F@k��@k�F@k��@j�@j�\@j�@i��@i��@i��@h��@h1'@g�;@g|�@f��@d�@b��@bM�@a��@a&�@`Ĝ@`A�@`1'@_|�@_;d@^��@^�+@]�@]��@]`B@]�@\�j@\z�@\j@\Z@\9X@[�m@[S�@Z�@Z��@Z�!@Z�\@Z~�@Z^5@ZM�@Z^5@Z��@Z�\@Z�\@Z~�@Zn�@Z^5@ZM�@Z=q@Z=q@Z=q@Z=q@Z-@Z�@ZJ@ZJ@Y��@Y��@YX@XQ�@X1'@X1'@X1'@Xb@W��@W�P@W�@V�R@VE�@V@U`B@UV@T�@T9X@S��@S�m@S�
@S�
@Sƨ@S��@SC�@R��@R�\@Rn�@R=q@Q�@Q��@Q&�@P��@PQ�@Pb@O��@Ol�@OK�@O�@N��@Nȴ@NE�@Mp�@L��@L�@Lj@L1@Kt�@KS�@K"�@J�H@J��@J=q@I�#@IX@I%@HbN@Hb@G�w@G+@Fff@F{@E�T@E?}@D��@D�j@D��@D(�@C�
@C@C@C@B��@B��@B��@B��@B=q@BJ@A��@A�7@AG�@A&�@@Q�@?�;@?�P@?+@>ȴ@>��@>v�@>v�@>v�@>V@>E�@>$�@=�@=@=��@=/@<Z@;��@;S�@:�@:^5@:�@9�#@9��@9hs@9G�@9�@8�9@8�u@8A�@7�;@7+@6ȴ@65?@5�T@5@5�h@5p�@5?}@5/@5V@4��@4�@4�D@4Z@4Z@4(�@41@3ƨ@333@2�H@2~�@2�@2J@1�@1�^@1hs@1&�@0��@0�@0 �@0b@0  @/��@/��@/l�@/+@/+@/
=@.��@.�y@.��@.ff@.5?@.$�@.{@-��@-p�@,��@,��@,��@,9X@+ƨ@+t�@+S�@+C�@+@*��@*�!@*~�@*-@*J@)��@)�@)�^@)��@)x�@)G�@)�@)%@)%@)%@(�9@(�u@(bN@(A�@(b@'�;@'��@'l�@';d@';d@'
=@&�@&ȴ@&�R@&�+@&5?@%�@%�@%?}@%�@$��@$�/@$��@$j@$Z@$Z@$9X@#�
@#�@#dZ@#"�@"��@"^5@!�@!�#@!��@!�^@!��@!hs@!7L@!&�@!�@ Ĝ@ r�@ bN@ A�@ A�@ 1'@   @�;@\)@��@��@��@
=@�y@��@V@@��@p�@O�@O�@/@�@�j@��@Z@(�@�@1@��@�m@�
@�@S�@o@��@�!@�!@��@�\@~�@n�@-@�@��@hs@&�@%@��@�`@��@�@1'@1'@�;@�w@��@\)@+@��@��@@@�@O�@�/@z�@I�@(�@(�@��@�m@�
@ƨ@ƨ@�F@��@��@��@��@��@C�@��@�\@~�@n�@�@��@�^@�7@hs@X@G�@G�@7L@&�@��@Q�@�@��@�w@�P@;d@
=@��@��@E�@��@��@�@�@`B@?}@�@��@�/@�/@��@��@��@�D@I�@�m@ƨ@�F@�@@
��@
�!@
��@
�!@
��@
��@
�\@
~�@
M�@
=q@
�@	��@	��@	&�@��@��@��@��@��@�9@Q�@  @�;@�@�@��@�P@\)@
=@
=@
=@��@��@�y@�y@�y@�@ȴ@ȴ@�R@��@v�@ff@ff@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�7LA��A�1ȂhAˁA�Q�A�"�A�ƨA�p�A�O�A�I�A�A�A�9XA�+A���A���Aɡ�AǏ\A���A��/A���A���A��A���A�1'A���A��#A��A��A�A��RA��+A�Q�A��RA��PA�1A��A��+A��+A�ffA�9XA���A�K�A���A�33A�t�A��A�7LA�ĜA�G�A���A�jA�=qA���A�t�A��A�?}A��-A�ZA� �A�M�A���A�bNA�ƨA�ZA��
A��A��;A�&�A�S�A�ĜA�  A� �A��-A�bNA���A�?}A�-A���A��wA��A�`BA���A�oA�A��FA�%A�1'A��7A�^5A��yA�jA���A�/A���A���A�t�A�$�A���A�&�A�dZA�z�A���A�M�A}�A|�A{�7Ax�/Av�\AudZAtjAr��ApbAm�;Al��AkC�Aix�Af�Ae33AdM�AbA_�TA^Q�A]��A\��A\-A[oAZA�AY`BAYoAXAWhsAV{AT~�AS�#AR^5AP�/AO�-AN��AM�;AKAJ�AI"�AH�yAH��AH��AF�/ADȴAC�PAB��AA��A@�HA?/A>jA=&�A;��A;/A:r�A9�A9oA8ĜA8�\A8E�A7��A7�wA7��A7hsA7oA6v�A5�-A3ƨA2�jA2�\A2VA1�A17LA01A/�PA.�`A-�TA,�yA,$�A+|�A*��A*(�A)��A)�PA)t�A)`BA)/A)VA(��A(�A'�PA'S�A%hsA%C�A%VA%
=A%A$�/A$Q�A#�-A#p�A"�yA!��A!t�A!7LA (�A�AI�A��Al�A�9AE�A�TA33A�A��A5?A33AĜA5?A�
A��A+AdZA�!A��A��A�mA7LA��A�A+AAA	;dA�A��A�DAz�AE�A�wAn�AƨA`BA�`A��AbNA1A��A��A@��F@�
=@�v�@���@�b@�+@�p�@���@�@��-@���@�O�@���@��@��y@�O�@�\)@�+@���@��@�ff@��@���@��@���@�+@�p�@��@�%@�bN@޸R@ݡ�@�ƨ@�E�@�?}@�~�@�I�@�\)@җ�@�O�@ϕ�@�?}@�  @ȼj@ŉ7@��m@�ȴ@�=q@�hs@���@��w@�M�@�p�@��/@�r�@�9X@� �@�  @���@�K�@�
=@�^5@�%@�  @�z�@��@�$�@��^@���@�A�@�ƨ@�"�@�$�@��7@�%@��@�33@���@�V@��-@�O�@�&�@��@��D@���@�33@�=q@���@�X@���@�Q�@���@��y@��+@��#@�x�@�7L@��/@�Q�@���@�o@�@���@�ff@��7@���@���@��@��@���@�M�@�J@���@���@�&�@��@���@��9@��D@��@���@�A�@��F@�S�@��@��+@�V@�=q@�{@�@�?}@�Ĝ@��!@���@��j@���@��D@��j@��@�r�@� �@���@�l�@��@��y@���@�E�@�{@��T@�x�@���@��D@�j@�Z@�Z@�1@�b@�  @�l�@�+@�@��@��R@�ff@�M�@�=q@�=q@��@�@���@��@�%@��@��@��D@�Q�@���@�=q@�$�@�@��-@��7@��@�x�@�x�@�G�@�/@��u@��@��D@�Z@�1@��m@�33@��!@���@���@���@�M�@���@��h@��@��@��@�`B@��@���@���@��@�j@�b@~��@}V@}/@}�@|�@|1@{dZ@z�H@z~�@zn�@z=q@z=q@z-@z-@z�@y��@y��@yx�@yX@y7L@x��@xr�@xbN@xbN@xbN@x �@w��@v�@v��@v��@v��@v��@v{@t�/@t(�@s��@sƨ@sƨ@s�F@s�F@s��@sdZ@r�H@rn�@r-@q�#@q�7@qhs@qG�@q�@p��@pĜ@p��@p��@p�u@q��@q��@p�u@p �@o+@n��@n�R@n��@mp�@m�@m�T@n5?@n�+@n5?@m�-@m��@m?}@m�@m�h@mV@l��@lz�@l1@k�
@k�F@k��@k�F@k��@j�@j�\@j�@i��@i��@i��@h��@h1'@g�;@g|�@f��@d�@b��@bM�@a��@a&�@`Ĝ@`A�@`1'@_|�@_;d@^��@^�+@]�@]��@]`B@]�@\�j@\z�@\j@\Z@\9X@[�m@[S�@Z�@Z��@Z�!@Z�\@Z~�@Z^5@ZM�@Z^5@Z��@Z�\@Z�\@Z~�@Zn�@Z^5@ZM�@Z=q@Z=q@Z=q@Z=q@Z-@Z�@ZJ@ZJ@Y��@Y��@YX@XQ�@X1'@X1'@X1'@Xb@W��@W�P@W�@V�R@VE�@V@U`B@UV@T�@T9X@S��@S�m@S�
@S�
@Sƨ@S��@SC�@R��@R�\@Rn�@R=q@Q�@Q��@Q&�@P��@PQ�@Pb@O��@Ol�@OK�@O�@N��@Nȴ@NE�@Mp�@L��@L�@Lj@L1@Kt�@KS�@K"�@J�H@J��@J=q@I�#@IX@I%@HbN@Hb@G�w@G+@Fff@F{@E�T@E?}@D��@D�j@D��@D(�@C�
@C@C@C@B��@B��@B��@B��@B=q@BJ@A��@A�7@AG�@A&�@@Q�@?�;@?�P@?+@>ȴ@>��@>v�@>v�@>v�@>V@>E�@>$�@=�@=@=��@=/@<Z@;��@;S�@:�@:^5@:�@9�#@9��@9hs@9G�@9�@8�9@8�u@8A�@7�;@7+@6ȴ@65?@5�T@5@5�h@5p�@5?}@5/@5V@4��@4�@4�D@4Z@4Z@4(�@41@3ƨ@333@2�H@2~�@2�@2J@1�@1�^@1hs@1&�@0��@0�@0 �@0b@0  @/��@/��@/l�@/+@/+@/
=@.��@.�y@.��@.ff@.5?@.$�@.{@-��@-p�@,��@,��@,��@,9X@+ƨ@+t�@+S�@+C�@+@*��@*�!@*~�@*-@*J@)��@)�@)�^@)��@)x�@)G�@)�@)%@)%@)%@(�9@(�u@(bN@(A�@(b@'�;@'��@'l�@';d@';d@'
=@&�@&ȴ@&�R@&�+@&5?@%�@%�@%?}@%�@$��@$�/@$��@$j@$Z@$Z@$9X@#�
@#�@#dZ@#"�@"��@"^5@!�@!�#@!��@!�^@!��@!hs@!7L@!&�@!�@ Ĝ@ r�@ bN@ A�@ A�@ 1'@   @�;@\)@��@��@��@
=@�y@��@V@@��@p�@O�@O�@/@�@�j@��@Z@(�@�@1@��@�m@�
@�@S�@o@��@�!@�!@��@�\@~�@n�@-@�@��@hs@&�@%@��@�`@��@�@1'@1'@�;@�w@��@\)@+@��@��@@@�@O�@�/@z�@I�@(�@(�@��@�m@�
@ƨ@ƨ@�F@��@��@��@��@��@C�@��@�\@~�@n�@�@��@�^@�7@hs@X@G�@G�@7L@&�@��@Q�@�@��@�w@�P@;d@
=@��@��@E�@��@��@�@�@`B@?}@�@��@�/@�/@��@��@��@�D@I�@�m@ƨ@�F@�@@
��@
�!@
��@
�!@
��@
��@
�\@
~�@
M�@
=q@
�@	��@	��@	&�@��@��@��@��@��@�9@Q�@  @�;@�@�@��@�P@\)@
=@
=@
=@��@��@�y@�y@�y@�@ȴ@ȴ@�R@��@v�@ff@ff@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�bB�\B�JB�+B|�B� B�B�+B�1B�%B�+B�+B�+B�%B�B� B{�Bw�B�B� B�B{�B�B{�Bt�Bq�Bm�BhsBe`B^5B[#B[#BT�BS�BT�BXB\)BVBQ�BP�BR�BG�BM�BZB[#BW
BXBK�BG�BD�B@�B<jB:^B8RB5?B0!B,B&�B#�B�BDB�B�yB�BB�B��BB�B��B�uB�7B� Bs�Bm�Bu�B~�Bv�Bt�Bm�B_;BO�B>wB33B�BB�B�)B��B�B��B�B|�Bt�Bl�BffBcTB`BB[#BVB@�B6FB(�B �BVB��B�B�yB�BɺB��B�XB�B��B�bB�7B� Bw�BgmB^5BW
BL�BA�B8RB2-B/B-B,B'�B#�B �B�B�BuBDB+BB
��B
�B
�B
�B
�HB
�)B
��B
��B
��B
��B
��B
B
�dB
�XB
�?B
�3B
�B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
�oB
�hB
�\B
�VB
�PB
�DB
�7B
�B
�B
|�B
|�B
{�B
z�B
x�B
t�B
q�B
o�B
l�B
hsB
e`B
cTB
`BB
_;B
]/B
]/B
]/B
\)B
\)B
[#B
ZB
YB
W
B
T�B
N�B
M�B
M�B
M�B
L�B
L�B
J�B
H�B
G�B
F�B
D�B
A�B
?}B
>wB
:^B
8RB
6FB
5?B
49B
2-B
2-B
0!B
/B
.B
,B
+B
(�B
'�B
%�B
$�B
"�B
�B
�B
�B
uB
hB
VB
PB
DB

=B
%B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�yB	�fB	�fB	�fB	�`B	�`B	�ZB	�TB	�TB	�NB	�BB	�HB	�BB	�5B	�/B	�;B	�)B	�)B	�)B	�)B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�HB	�NB	�NB	�TB	�TB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
+B
+B
+B
+B
PB
VB
bB
oB
uB
uB
{B
�B
�B
�B
�B
�B
 �B
 �B
#�B
%�B
(�B
)�B
.B
/B
0!B
1'B
49B
6FB
:^B
:^B
:^B
<jB
@�B
B�B
B�B
J�B
L�B
N�B
P�B
Q�B
T�B
XB
ZB
YB
[#B
\)B
]/B
`BB
cTB
dZB
ffB
iyB
n�B
s�B
t�B
t�B
t�B
v�B
y�B
z�B
�B
�=B
�=B
�JB
�VB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�-B
�LB
�jB
�wB
�}B
�}B
ÖB
ĜB
ŢB
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�#B
�;B
�;B
�BB
�ZB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
��B
��BBB+B	7BDBVB\BhB{B�B�B�B�B�B�B�B �B#�B#�B$�B(�B,B-B.B.B.B/B0!B1'B0!B33B8RB:^B<jB=qB>wB?}B?}B?}B@�B@�B@�B@�BB�BC�BD�BF�BH�BJ�BK�BK�BL�BL�BM�BQ�BW
BXBXBXBXBYB^5BaHBbNBcTBffBgmBgmBgmBjBl�Bp�Br�Bs�Bt�Bt�Bu�Bv�Bx�B{�B~�B� B�B�B�%B�B�%B�B�B�1B�7B�7B�7B�JB�VB�bB�uB�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�-B�3B�3B�9B�9B�?B�LB�^B�^B�dB�dB�dB�jB�jB�qB�}B��B��BBBBÖBÖBÖBÖBÖBĜBĜBĜBĜBĜBĜBƨBǮBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�
B�B�B�B�#B�#B�)B�/B�5B�;B�;B�BB�BB�HB�HB�HB�NB�NB�TB�ZB�`B�`B�fB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBB%B%B+B+B1B1B1B	7B	7B	7B
=BJBJBPBPBPBPBVBVBVBVBVBVB\B\B\BbBbBbBhBoBoBuBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B!�B!�B!�B!�B"�B"�B#�B#�B#�B$�B$�B$�B$�B$�B%�B%�B%�B%�B%�B&�B&�B'�B(�B(�B(�B(�B(�B(�B(�B)�B)�B)�B+B+B,B,B,B-B-B-B-B.B.B.B.B/B/B/B0!B0!B0!B0!B0!B0!B1'B2-B2-B2-B2-B2-B2-B2-B33B33B49B49B49B49B49B5?B5?B5?B5?B5?B5?B6FB6FB6FB6FB7LB7LB8RB8RB8RB8RB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B;dB;dB;dB<jB<jB<jB<jB<jB=qB=qB>wB>wB>wB?}B?}B@�B@�BA�BA�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BE�BE�BE�BE�BE�BE�BE�BF�BE�BF�BF�BG�BG�BH�BH�BH�BH�BH�BI�BI�BJ�BJ�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BL�BM�BM�BM�BN�BO�BO�BO�BO�BO�BO�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BR�BR�BR�BR�BR�BR�BS�BS�BS�BT�BT�BT�BT�BT�BVBVBVBVBVBVBVBVBVBVBVBVBVBW
BW
BW
BW
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�bB�\B�JB�+B|�B� B�B�+B�1B�%B�+B�+B�+B�%B�B� B{�Bw�B�B� B�B{�B�B{�Bt�Bq�Bm�BhsBe`B^5B[#B[#BT�BS�BT�BXB\)BVBQ�BP�BR�BG�BM�BZB[#BW
BXBK�BG�BD�B@�B<jB:^B8RB5?B0!B,B&�B#�B�BDB�B�yB�BB�B��BB�B��B�uB�7B� Bs�Bm�Bu�B~�Bv�Bt�Bm�B_;BO�B>wB33B�BB�B�)B��B�B��B�B|�Bt�Bl�BffBcTB`BB[#BVB@�B6FB(�B �BVB��B�B�yB�BɺB��B�XB�B��B�bB�7B� Bw�BgmB^5BW
BL�BA�B8RB2-B/B-B,B'�B#�B �B�B�BuBDB+BB
��B
�B
�B
�B
�HB
�)B
��B
��B
��B
��B
��B
B
�dB
�XB
�?B
�3B
�B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
�oB
�hB
�\B
�VB
�PB
�DB
�7B
�B
�B
|�B
|�B
{�B
z�B
x�B
t�B
q�B
o�B
l�B
hsB
e`B
cTB
`BB
_;B
]/B
]/B
]/B
\)B
\)B
[#B
ZB
YB
W
B
T�B
N�B
M�B
M�B
M�B
L�B
L�B
J�B
H�B
G�B
F�B
D�B
A�B
?}B
>wB
:^B
8RB
6FB
5?B
49B
2-B
2-B
0!B
/B
.B
,B
+B
(�B
'�B
%�B
$�B
"�B
�B
�B
�B
uB
hB
VB
PB
DB

=B
%B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�yB	�fB	�fB	�fB	�`B	�`B	�ZB	�TB	�TB	�NB	�BB	�HB	�BB	�5B	�/B	�;B	�)B	�)B	�)B	�)B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�;B	�HB	�NB	�NB	�TB	�TB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
+B
+B
+B
+B
PB
VB
bB
oB
uB
uB
{B
�B
�B
�B
�B
�B
 �B
 �B
#�B
%�B
(�B
)�B
.B
/B
0!B
1'B
49B
6FB
:^B
:^B
:^B
<jB
@�B
B�B
B�B
J�B
L�B
N�B
P�B
Q�B
T�B
XB
ZB
YB
[#B
\)B
]/B
`BB
cTB
dZB
ffB
iyB
n�B
s�B
t�B
t�B
t�B
v�B
y�B
z�B
�B
�=B
�=B
�JB
�VB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�-B
�LB
�jB
�wB
�}B
�}B
ÖB
ĜB
ŢB
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�#B
�;B
�;B
�BB
�ZB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
��B
��BBB+B	7BDBVB\BhB{B�B�B�B�B�B�B�B �B#�B#�B$�B(�B,B-B.B.B.B/B0!B1'B0!B33B8RB:^B<jB=qB>wB?}B?}B?}B@�B@�B@�B@�BB�BC�BD�BF�BH�BJ�BK�BK�BL�BL�BM�BQ�BW
BXBXBXBXBYB^5BaHBbNBcTBffBgmBgmBgmBjBl�Bp�Br�Bs�Bt�Bt�Bu�Bv�Bx�B{�B~�B� B�B�B�%B�B�%B�B�B�1B�7B�7B�7B�JB�VB�bB�uB�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�-B�3B�3B�9B�9B�?B�LB�^B�^B�dB�dB�dB�jB�jB�qB�}B��B��BBBBÖBÖBÖBÖBÖBĜBĜBĜBĜBĜBĜBƨBǮBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�
B�B�B�B�#B�#B�)B�/B�5B�;B�;B�BB�BB�HB�HB�HB�NB�NB�TB�ZB�`B�`B�fB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBB%B%B+B+B1B1B1B	7B	7B	7B
=BJBJBPBPBPBPBVBVBVBVBVBVB\B\B\BbBbBbBhBoBoBuBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B!�B!�B!�B!�B!�B!�B"�B"�B#�B#�B#�B$�B$�B$�B$�B$�B%�B%�B%�B%�B%�B&�B&�B'�B(�B(�B(�B(�B(�B(�B(�B)�B)�B)�B+B+B,B,B,B-B-B-B-B.B.B.B.B/B/B/B0!B0!B0!B0!B0!B0!B1'B2-B2-B2-B2-B2-B2-B2-B33B33B49B49B49B49B49B5?B5?B5?B5?B5?B5?B6FB6FB6FB6FB7LB7LB8RB8RB8RB8RB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B;dB;dB;dB<jB<jB<jB<jB<jB=qB=qB>wB>wB>wB?}B?}B@�B@�BA�BA�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BE�BE�BE�BE�BE�BE�BE�BF�BE�BF�BF�BG�BG�BH�BH�BH�BH�BH�BI�BI�BJ�BJ�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BL�BM�BM�BM�BN�BO�BO�BO�BO�BO�BO�BO�BO�BP�BP�BP�BP�BP�BP�BQ�BR�BR�BR�BR�BR�BR�BS�BS�BS�BT�BT�BT�BT�BT�BVBVBVBVBVBVBVBVBVBVBVBVBVBW
BW
BW
BW
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210708090050                              AO  ARCAADJP                                                                    20210708090050    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210708090050  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210708090050  QCF$                G�O�G�O�G�O�8000            