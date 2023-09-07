CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-11-07T10:01:08Z creation      
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
resolution        =���   axis      Z        \  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  o4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ǈ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ڼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211107100108  20211107100108  5905729 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @٠�?%��1   @٠���@(�I�^5�d�333331   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�  @���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B  B��B��B��B'��B/��B7��B?��BG��BO��BXfgB_34Bg��Bo��Bw��B��B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQs4DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�De  Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D�  D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D�� D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�C�A�C�A�C�A�G�A�S�A�S�A�VA�S�A�S�A�VA�XA�ZA�VA�S�A�S�A�O�A�K�A�5?A�
=Aڛ�A�JA�K�A؟�A�Q�A�;dAϑhA�ĜA�A�5?A�/A��PA���A�p�A�ƨA��\A�\)A�x�A���A�ffA�JA�A�(�A�{A�$�A��9A��!A���A��\A�5?A���A��`A�\)A�K�A�VA��+A�{A�M�A��/A���A��A���A���A��A��Ay��AqC�AoVAm�FAl1Acl�A_�PA[33AWXAU%AT�AR�+AOl�AK
=AGG�AE��ADv�AF��AF��AE��AF$�AEp�AC�AAO�A@n�A@Q�A@$�A@  A?��A?t�A?K�A>�A=��A<ffA<jA;�A:I�A9;dA7%A4��A2M�A1�7A1�A0�/A0��A0r�A0E�A/�hA/oA.I�A-��A-&�A,��A-A-A,~�A,z�A,�+A,�+A,�+A,n�A+�A+\)A*�+A)�hA(^5A(JA'��A'��A'A&�A&�HA&�A&jA&A%�A$��A$�!A$VA#�A#��A#t�A#��A#�-A#x�A#?}A#
=A"�yA"ȴA"��A"v�A!��A!�A!33A!VA ��A �A �jA ~�A �A��AhsA%A�A{A  AS�A�A��A��A��A�A?}A�yA��A�\A�AAt�A;dAȴAZA��A�^A��A\)A��A=qA$�A �A��A��At�A�A��A��A�+AVA  A��A�A�AĜA��AJA|�AXAĜA�DAz�AM�A{A�
A�FA|�AhsA;dA��A�yA�RA�DA1'AA�A��Al�A7LA�A�9A�uA=qAJA
��A
E�A
A	�A	\)A	�AA�A��A�PAx�AdZA7LA�A��AE�A$�A�;A��A|�AC�A�RAv�A^5AE�A(�A�At�A�AA�A1A�^A;dAoA �A ��A ĜA ��A  �@���@��@���@���@���@� �@��;@���@��@���@���@��@��9@�9X@�dZ@��7@��@��9@�"�@�n�@���@�@�^@���@�A�@@��@�n�@��@�M�@�{@�X@�Q�@�"�@�R@�5?@��#@�p�@��@�A�@���@��@��y@��@�-@�j@�z�@�1@�o@�+@�n�@���@�@� �@���@ߍP@�o@�E�@���@�&�@�  @���@ى7@�7L@�V@��`@�r�@��@׶F@�+@���@֧�@�ff@�@�O�@�V@ԣ�@ӝ�@�S�@�;d@��@��H@҇+@��T@��`@Л�@�Z@�(�@��@�;d@Η�@�J@͑h@���@��/@�Ĝ@̃@�9X@˥�@�K�@�V@ɉ7@Ȭ@�Z@��m@���@ź^@��@ēu@�dZ@���@�~�@�^5@�J@��T@�`B@���@�Ĝ@��j@��9@��u@���@��H@���@��+@�^5@�=q@��-@�?}@��@���@�r�@� �@��@��y@���@�^5@��#@���@�%@��@� �@�b@��@���@���@���@���@��\@�5?@��T@��@��@�bN@��w@�
=@���@���@�E�@�{@��@��#@���@�x�@��@�x�@�&�@��`@�r�@��
@���@���@��@�C�@�@��R@�ff@�{@�x�@�&�@��9@�bN@��@��@�S�@��H@���@�~�@��-@�O�@�V@�Z@���@�l�@��@��y@�M�@���@�O�@���@���@�9X@��F@�\)@���@���@�V@�-@��T@�x�@���@�(�@��@�@�ȴ@��\@�V@��@��#@�`B@�r�@���@��@�33@���@�V@��@���@�X@�/@�V@���@���@��@�1'@���@�l�@���@��\@�M�@�@�X@��9@��F@�S�@�C�@�o@���@�^5@��#@�x�@���@�9X@��;@���@��@�S�@�C�@�o@��+@�M�@��@�7L@��@�%@���@�bN@��m@�ƨ@��F@���@��P@�K�@�;d@��y@�v�@�^5@�^5@�=q@�@���@�hs@�X@�7L@�7L@�&�@���@���@��@�Q�@�1@��m@��m@��;@��F@�t�@�\)@�
=@���@�=q@��@���@��^@��h@�hs@�?}@��@���@��9@��@��@�l�@�;d@�"�@�o@�@��H@�ȴ@�v�@�{@�@��T@��^@�`B@��@���@�Z@�I�@�1'@�b@�;@�P@l�@+@~�R@}��@|�j@{��@{�F@{��@{S�@{@z��@z��@z�!@z^5@y�@yX@xr�@xQ�@wl�@v�R@vV@vV@vV@v{@u�T@u��@u�@u�@t�/@t�@t9X@s��@sC�@r��@q��@qx�@q&�@p�u@o�P@o\)@oK�@n��@n5?@m��@l��@l1@kƨ@k�F@k��@kdZ@ko@j��@i��@i��@i&�@hA�@h �@g�@g��@gl�@g�@f��@f@e��@e@e�-@ep�@e�@d�@d9X@c��@cdZ@cC�@c33@c33@c"�@b=q@a�7@ahs@ahs@`��@_�;@_|�@_+@_
=@^ȴ@]�T@]/@\��@\�@\�@\�@\��@\��@\Z@[S�@Z��@Z�!@Z��@Zn�@Y��@Y��@Yx�@YX@Y7L@XĜ@X�@X �@X  @W\)@Vȴ@V��@U��@UO�@T��@T��@TZ@T1@S�F@SdZ@SS�@S33@RM�@Q�#@Q��@P�u@Ol�@N�y@N�y@N�y@N�y@O
=@N��@M?}@L�/@Lj@K��@J��@J-@I�@I��@I�7@I&�@H��@HQ�@G�@G
=@F��@F$�@Ep�@E�@D��@D(�@D�@C��@C�@Co@B��@B~�@B=q@A�^@A��@A%@@b@?�w@?l�@?�@>ȴ@>v�@>5?@=�@=@=�h@=�@<I�@<1@;�m@;�
@;�F@;�@;C�@:��@:^5@9��@9&�@8�@8r�@8bN@8bN@8Q�@81'@8  @7�;@7��@7��@7�@7�P@7l�@7\)@7K�@6�y@6��@6��@6V@5�@5�@5`B@5/@4��@4�j@4z�@4j@4I�@41@3��@3S�@333@3"�@2�@2��@2��@2�!@2��@2�\@2~�@2~�@2n�@2^5@1��@1��@1�7@1�7@1X@0��@0�9@0�@0b@0  @/�@/�w@/��@/K�@/+@.�@.��@.�+@.ff@.E�@.{@-@,��@,��@,j@,Z@,1@+ƨ@+�F@+��@+S�@*�H@*��@*�!@*�!@*=q@*J@)�@)�#@)��@)�^@)x�@)hs@)&�@)%@(�u@(bN@(Q�@(1'@'�@'�w@'��@'|�@'l�@'K�@'�@&ȴ@&��@&E�@&$�@%�@%�-@%`B@%�@$��@$�D@$j@$Z@$1@#�
@#ƨ@#��@#C�@#@"�\@"J@!�@!�#@!�#@!hs@ Ĝ@ �@ r�@ r�@ Q�@�@K�@�@��@V@@�T@��@�h@O�@��@�@z�@Z@1@��@�m@�
@��@dZ@o@��@��@��@�\@~�@-@��@�@�#@�#@��@�^@G�@Ĝ@bN@b@��@�P@��@�R@�+@ff@5?@$�@$�@@��@�h@�@��@�@�/@�j@�D@Z@Z@Z@Z@�@�m@ƨ@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�C�A�C�A�C�A�G�A�S�A�S�A�VA�S�A�S�A�VA�XA�ZA�VA�S�A�S�A�O�A�K�A�5?A�
=Aڛ�A�JA�K�A؟�A�Q�A�;dAϑhA�ĜA�A�5?A�/A��PA���A�p�A�ƨA��\A�\)A�x�A���A�ffA�JA�A�(�A�{A�$�A��9A��!A���A��\A�5?A���A��`A�\)A�K�A�VA��+A�{A�M�A��/A���A��A���A���A��A��Ay��AqC�AoVAm�FAl1Acl�A_�PA[33AWXAU%AT�AR�+AOl�AK
=AGG�AE��ADv�AF��AF��AE��AF$�AEp�AC�AAO�A@n�A@Q�A@$�A@  A?��A?t�A?K�A>�A=��A<ffA<jA;�A:I�A9;dA7%A4��A2M�A1�7A1�A0�/A0��A0r�A0E�A/�hA/oA.I�A-��A-&�A,��A-A-A,~�A,z�A,�+A,�+A,�+A,n�A+�A+\)A*�+A)�hA(^5A(JA'��A'��A'A&�A&�HA&�A&jA&A%�A$��A$�!A$VA#�A#��A#t�A#��A#�-A#x�A#?}A#
=A"�yA"ȴA"��A"v�A!��A!�A!33A!VA ��A �A �jA ~�A �A��AhsA%A�A{A  AS�A�A��A��A��A�A?}A�yA��A�\A�AAt�A;dAȴAZA��A�^A��A\)A��A=qA$�A �A��A��At�A�A��A��A�+AVA  A��A�A�AĜA��AJA|�AXAĜA�DAz�AM�A{A�
A�FA|�AhsA;dA��A�yA�RA�DA1'AA�A��Al�A7LA�A�9A�uA=qAJA
��A
E�A
A	�A	\)A	�AA�A��A�PAx�AdZA7LA�A��AE�A$�A�;A��A|�AC�A�RAv�A^5AE�A(�A�At�A�AA�A1A�^A;dAoA �A ��A ĜA ��A  �@���@��@���@���@���@� �@��;@���@��@���@���@��@��9@�9X@�dZ@��7@��@��9@�"�@�n�@���@�@�^@���@�A�@@��@�n�@��@�M�@�{@�X@�Q�@�"�@�R@�5?@��#@�p�@��@�A�@���@��@��y@��@�-@�j@�z�@�1@�o@�+@�n�@���@�@� �@���@ߍP@�o@�E�@���@�&�@�  @���@ى7@�7L@�V@��`@�r�@��@׶F@�+@���@֧�@�ff@�@�O�@�V@ԣ�@ӝ�@�S�@�;d@��@��H@҇+@��T@��`@Л�@�Z@�(�@��@�;d@Η�@�J@͑h@���@��/@�Ĝ@̃@�9X@˥�@�K�@�V@ɉ7@Ȭ@�Z@��m@���@ź^@��@ēu@�dZ@���@�~�@�^5@�J@��T@�`B@���@�Ĝ@��j@��9@��u@���@��H@���@��+@�^5@�=q@��-@�?}@��@���@�r�@� �@��@��y@���@�^5@��#@���@�%@��@� �@�b@��@���@���@���@���@��\@�5?@��T@��@��@�bN@��w@�
=@���@���@�E�@�{@��@��#@���@�x�@��@�x�@�&�@��`@�r�@��
@���@���@��@�C�@�@��R@�ff@�{@�x�@�&�@��9@�bN@��@��@�S�@��H@���@�~�@��-@�O�@�V@�Z@���@�l�@��@��y@�M�@���@�O�@���@���@�9X@��F@�\)@���@���@�V@�-@��T@�x�@���@�(�@��@�@�ȴ@��\@�V@��@��#@�`B@�r�@���@��@�33@���@�V@��@���@�X@�/@�V@���@���@��@�1'@���@�l�@���@��\@�M�@�@�X@��9@��F@�S�@�C�@�o@���@�^5@��#@�x�@���@�9X@��;@���@��@�S�@�C�@�o@��+@�M�@��@�7L@��@�%@���@�bN@��m@�ƨ@��F@���@��P@�K�@�;d@��y@�v�@�^5@�^5@�=q@�@���@�hs@�X@�7L@�7L@�&�@���@���@��@�Q�@�1@��m@��m@��;@��F@�t�@�\)@�
=@���@�=q@��@���@��^@��h@�hs@�?}@��@���@��9@��@��@�l�@�;d@�"�@�o@�@��H@�ȴ@�v�@�{@�@��T@��^@�`B@��@���@�Z@�I�@�1'@�b@�;@�P@l�@+@~�R@}��@|�j@{��@{�F@{��@{S�@{@z��@z��@z�!@z^5@y�@yX@xr�@xQ�@wl�@v�R@vV@vV@vV@v{@u�T@u��@u�@u�@t�/@t�@t9X@s��@sC�@r��@q��@qx�@q&�@p�u@o�P@o\)@oK�@n��@n5?@m��@l��@l1@kƨ@k�F@k��@kdZ@ko@j��@i��@i��@i&�@hA�@h �@g�@g��@gl�@g�@f��@f@e��@e@e�-@ep�@e�@d�@d9X@c��@cdZ@cC�@c33@c33@c"�@b=q@a�7@ahs@ahs@`��@_�;@_|�@_+@_
=@^ȴ@]�T@]/@\��@\�@\�@\�@\��@\��@\Z@[S�@Z��@Z�!@Z��@Zn�@Y��@Y��@Yx�@YX@Y7L@XĜ@X�@X �@X  @W\)@Vȴ@V��@U��@UO�@T��@T��@TZ@T1@S�F@SdZ@SS�@S33@RM�@Q�#@Q��@P�u@Ol�@N�y@N�y@N�y@N�y@O
=@N��@M?}@L�/@Lj@K��@J��@J-@I�@I��@I�7@I&�@H��@HQ�@G�@G
=@F��@F$�@Ep�@E�@D��@D(�@D�@C��@C�@Co@B��@B~�@B=q@A�^@A��@A%@@b@?�w@?l�@?�@>ȴ@>v�@>5?@=�@=@=�h@=�@<I�@<1@;�m@;�
@;�F@;�@;C�@:��@:^5@9��@9&�@8�@8r�@8bN@8bN@8Q�@81'@8  @7�;@7��@7��@7�@7�P@7l�@7\)@7K�@6�y@6��@6��@6V@5�@5�@5`B@5/@4��@4�j@4z�@4j@4I�@41@3��@3S�@333@3"�@2�@2��@2��@2�!@2��@2�\@2~�@2~�@2n�@2^5@1��@1��@1�7@1�7@1X@0��@0�9@0�@0b@0  @/�@/�w@/��@/K�@/+@.�@.��@.�+@.ff@.E�@.{@-@,��@,��@,j@,Z@,1@+ƨ@+�F@+��@+S�@*�H@*��@*�!@*�!@*=q@*J@)�@)�#@)��@)�^@)x�@)hs@)&�@)%@(�u@(bN@(Q�@(1'@'�@'�w@'��@'|�@'l�@'K�@'�@&ȴ@&��@&E�@&$�@%�@%�-@%`B@%�@$��@$�D@$j@$Z@$1@#�
@#ƨ@#��@#C�@#@"�\@"J@!�@!�#@!�#@!hs@ Ĝ@ �@ r�@ r�@ Q�@�@K�@�@��@V@@�T@��@�h@O�@��@�@z�@Z@1@��@�m@�
@��@dZ@o@��@��@��@�\@~�@-@��@�@�#@�#@��@�^@G�@Ĝ@bN@b@��@�P@��@�R@�+@ff@5?@$�@$�@@��@�h@�@��@�@�/@�j@�D@Z@Z@Z@Z@�@�m@ƨ@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�1B�+B�+B�1B�1B�7B�1B�7B�1B�7B�=B�=B�JB�PB�=B��B��B�!B��B	%B	��B
l�BoB-BPBB
�NB
��BPB
=B=qBM�Be`BffB�VB��B�B�B�B��B�}B�-B�B�FB�B��B�uB�JB�bB�1Bp�BcTBZBL�B.B�B1B
�TB
��B
XB
aHB
S�B
6FB	��B	ȴB	��B	��B	��B	�%B	R�B	J�B	F�B	C�B	M�B	S�B	K�B	:^B	?}B	cTB	�+B	��B	�B
�B
bB
hB
$�B
)�B
6FB
G�B
t�B
�=B
�hB
��B
��B
��B
��B
��B
�uB
��B
�VB
}�B
u�B
hsB
`BB
bNB
t�B
y�B
� B
�7B
�uB
�{B
��B
��B
��B
��B
��B
�B
�'B
�'B
�'B
�9B
�FB
�^B
�jB
��B
�}B
�wB
�^B
�XB
�?B
�LB
�^B
�^B
�LB
�XB
�^B
�RB
�FB
�9B
�'B
�!B
�-B
�'B
�'B
�3B
�LB
�XB
�wB
��B
��B
��B
��B
��B
�wB
�qB
�XB
�^B
�qB
�wB
�wB
�wB
�jB
�^B
�RB
�?B
�FB
�9B
�3B
�!B
�'B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�oB
�bB
�DB
�=B
�JB
�JB
�JB
�VB
�VB
�=B
�JB
�DB
�DB
�=B
�1B
�1B
�B
�B
�B
�B
}�B
z�B
z�B
x�B
w�B
x�B
v�B
u�B
t�B
u�B
t�B
v�B
v�B
u�B
u�B
t�B
r�B
p�B
p�B
p�B
n�B
l�B
k�B
jB
jB
iyB
ffB
cTB
\)B
ZB
\)B
\)B
\)B
[#B
VB
T�B
ZB
YB
XB
W
B
VB
S�B
O�B
R�B
Q�B
P�B
P�B
O�B
L�B
M�B
N�B
M�B
L�B
I�B
F�B
C�B
D�B
G�B
E�B
D�B
F�B
F�B
F�B
E�B
B�B
?}B
;dB
=qB
>wB
<jB
<jB
=qB
=qB
=qB
:^B
:^B
9XB
9XB
9XB
6FB
2-B
-B
/B
.B
(�B
)�B
+B
-B
.B
+B
-B
,B
-B
+B
,B
.B
/B
,B
+B
(�B
+B
-B
.B
-B
-B
,B
,B
,B
-B
,B
(�B
(�B
)�B
)�B
&�B
'�B
(�B
%�B
#�B
%�B
%�B
%�B
$�B
!�B
"�B
 �B
�B
�B
�B
!�B
"�B
"�B
!�B
!�B
!�B
 �B
"�B
!�B
!�B
 �B
�B
 �B
�B
�B
 �B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
 �B
!�B
 �B
!�B
!�B
 �B
�B
�B
�B
�B
!�B
!�B
!�B
 �B
�B
 �B
�B
 �B
�B
!�B
!�B
"�B
"�B
"�B
!�B
!�B
#�B
!�B
 �B
!�B
"�B
!�B
"�B
#�B
#�B
$�B
"�B
!�B
$�B
$�B
$�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
%�B
$�B
$�B
%�B
%�B
&�B
(�B
(�B
(�B
(�B
(�B
'�B
%�B
(�B
)�B
+B
)�B
,B
-B
,B
-B
.B
/B
.B
/B
/B
.B
-B
-B
.B
.B
/B
.B
-B
.B
.B
1'B
33B
2-B
1'B
1'B
0!B
1'B
1'B
0!B
2-B
49B
49B
49B
5?B
5?B
33B
5?B
49B
49B
6FB
7LB
6FB
5?B
6FB
8RB
9XB
9XB
9XB
8RB
9XB
8RB
8RB
:^B
;dB
:^B
:^B
9XB
;dB
<jB
<jB
=qB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
>wB
=qB
=qB
=qB
;dB
;dB
<jB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
>wB
<jB
<jB
=qB
?}B
@�B
A�B
A�B
A�B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
A�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
F�B
E�B
E�B
E�B
E�B
G�B
H�B
G�B
H�B
H�B
I�B
H�B
H�B
G�B
H�B
H�B
J�B
H�B
I�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
J�B
I�B
I�B
J�B
I�B
L�B
L�B
L�B
K�B
M�B
N�B
M�B
L�B
M�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
R�B
S�B
R�B
VB
VB
VB
VB
T�B
T�B
T�B
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
W
B
VB
W
B
YB
YB
YB
YB
XB
W
B
W
B
YB
YB
XB
W
B
YB
YB
YB
YB
XB
XB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
YB
ZB
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
\)B
]/B
\)B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
^5B
^5B
_;B
]/B
^5B
^5B
_;B
`BB
_;B
_;B
^5B
]/B
`BB
`BB
_;B
^5B
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
bNB
dZB
dZB
dZB
e`B
e`B
dZB
gmB
ffB
e`B
ffB
ffB
gmB
gmB
ffB
ffB
ffB
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
jB
k�B
k�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
v�B
v�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
v�B
x�B
x�B
w�B
x�B
x�B
x�B
x�B
w�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
z�B
z�B
y�B
z�B
y�B
y�B
y�B
z�B
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
{�B
|�B
|�B
|�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
}�B
}�B
}�B
� B
� B
~�B
}�B
}�B
� B
�B
�B
� B
~�B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�+B
�1B
�1B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�=B
�=B
�DB
�DB
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�1B�+B�+B�1B�1B�7B�1B�7B�1B�7B�=B�=B�JB�PB�=B��B��B�!B��B	%B	��B
l�BoB-BPBB
�NB
��BPB
=B=qBM�Be`BffB�VB��B�B�B�B��B�}B�-B�B�FB�B��B�uB�JB�bB�1Bp�BcTBZBL�B.B�B1B
�TB
��B
XB
aHB
S�B
6FB	��B	ȴB	��B	��B	��B	�%B	R�B	J�B	F�B	C�B	M�B	S�B	K�B	:^B	?}B	cTB	�+B	��B	�B
�B
bB
hB
$�B
)�B
6FB
G�B
t�B
�=B
�hB
��B
��B
��B
��B
��B
�uB
��B
�VB
}�B
u�B
hsB
`BB
bNB
t�B
y�B
� B
�7B
�uB
�{B
��B
��B
��B
��B
��B
�B
�'B
�'B
�'B
�9B
�FB
�^B
�jB
��B
�}B
�wB
�^B
�XB
�?B
�LB
�^B
�^B
�LB
�XB
�^B
�RB
�FB
�9B
�'B
�!B
�-B
�'B
�'B
�3B
�LB
�XB
�wB
��B
��B
��B
��B
��B
�wB
�qB
�XB
�^B
�qB
�wB
�wB
�wB
�jB
�^B
�RB
�?B
�FB
�9B
�3B
�!B
�'B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�oB
�bB
�DB
�=B
�JB
�JB
�JB
�VB
�VB
�=B
�JB
�DB
�DB
�=B
�1B
�1B
�B
�B
�B
�B
}�B
z�B
z�B
x�B
w�B
x�B
v�B
u�B
t�B
u�B
t�B
v�B
v�B
u�B
u�B
t�B
r�B
p�B
p�B
p�B
n�B
l�B
k�B
jB
jB
iyB
ffB
cTB
\)B
ZB
\)B
\)B
\)B
[#B
VB
T�B
ZB
YB
XB
W
B
VB
S�B
O�B
R�B
Q�B
P�B
P�B
O�B
L�B
M�B
N�B
M�B
L�B
I�B
F�B
C�B
D�B
G�B
E�B
D�B
F�B
F�B
F�B
E�B
B�B
?}B
;dB
=qB
>wB
<jB
<jB
=qB
=qB
=qB
:^B
:^B
9XB
9XB
9XB
6FB
2-B
-B
/B
.B
(�B
)�B
+B
-B
.B
+B
-B
,B
-B
+B
,B
.B
/B
,B
+B
(�B
+B
-B
.B
-B
-B
,B
,B
,B
-B
,B
(�B
(�B
)�B
)�B
&�B
'�B
(�B
%�B
#�B
%�B
%�B
%�B
$�B
!�B
"�B
 �B
�B
�B
�B
!�B
"�B
"�B
!�B
!�B
!�B
 �B
"�B
!�B
!�B
 �B
�B
 �B
�B
�B
 �B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
 �B
!�B
 �B
!�B
!�B
 �B
�B
�B
�B
�B
!�B
!�B
!�B
 �B
�B
 �B
�B
 �B
�B
!�B
!�B
"�B
"�B
"�B
!�B
!�B
#�B
!�B
 �B
!�B
"�B
!�B
"�B
#�B
#�B
$�B
"�B
!�B
$�B
$�B
$�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
%�B
$�B
$�B
%�B
%�B
&�B
(�B
(�B
(�B
(�B
(�B
'�B
%�B
(�B
)�B
+B
)�B
,B
-B
,B
-B
.B
/B
.B
/B
/B
.B
-B
-B
.B
.B
/B
.B
-B
.B
.B
1'B
33B
2-B
1'B
1'B
0!B
1'B
1'B
0!B
2-B
49B
49B
49B
5?B
5?B
33B
5?B
49B
49B
6FB
7LB
6FB
5?B
6FB
8RB
9XB
9XB
9XB
8RB
9XB
8RB
8RB
:^B
;dB
:^B
:^B
9XB
;dB
<jB
<jB
=qB
<jB
<jB
<jB
<jB
;dB
;dB
<jB
=qB
>wB
=qB
=qB
=qB
;dB
;dB
<jB
=qB
>wB
=qB
>wB
>wB
>wB
>wB
>wB
<jB
<jB
=qB
?}B
@�B
A�B
A�B
A�B
@�B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
A�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
F�B
E�B
E�B
E�B
E�B
G�B
H�B
G�B
H�B
H�B
I�B
H�B
H�B
G�B
H�B
H�B
J�B
H�B
I�B
J�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
J�B
I�B
I�B
J�B
I�B
L�B
L�B
L�B
K�B
M�B
N�B
M�B
L�B
M�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
R�B
S�B
R�B
VB
VB
VB
VB
T�B
T�B
T�B
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
W
B
VB
W
B
YB
YB
YB
YB
XB
W
B
W
B
YB
YB
XB
W
B
YB
YB
YB
YB
XB
XB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
YB
ZB
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
\)B
]/B
\)B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
^5B
^5B
_;B
]/B
^5B
^5B
_;B
`BB
_;B
_;B
^5B
]/B
`BB
`BB
_;B
^5B
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
bNB
dZB
dZB
dZB
e`B
e`B
dZB
gmB
ffB
e`B
ffB
ffB
gmB
gmB
ffB
ffB
ffB
e`B
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
jB
k�B
k�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
v�B
v�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
v�B
x�B
x�B
w�B
x�B
x�B
x�B
x�B
w�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
z�B
z�B
y�B
z�B
y�B
y�B
y�B
z�B
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
{�B
|�B
|�B
|�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
}�B
}�B
}�B
� B
� B
~�B
}�B
}�B
� B
�B
�B
� B
~�B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�+B
�1B
�1B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�=B
�=B
�DB
�DB
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211107100108                              AO  ARCAADJP                                                                    20211107100108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211107100108  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211107100108  QCF$                G�O�G�O�G�O�0               