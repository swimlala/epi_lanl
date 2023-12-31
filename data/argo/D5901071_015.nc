CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:55Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135548  20190522121825  1727_5046_015                   2C  D   APEX                            2143                            040306                          846 @�*��-?�1   @�*��@
@7�Ƨ�c��hr�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B!33B'33B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�fD�9�D�i�D���D��fD�,�D�S3D���D���D�#3D�S3D��fD��D�&fD�i�Dڣ3D��D�&fD�S3D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B ��B&��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCZ  C\  C^  C_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP�3DQs3DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW�3DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Do� Dp  Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy� D�6fD�ffD���D��3D�)�D�P D���D�ɚD�  D�P D��3D��fD�#3D�ffDڠ D��fD�#3D�P D��D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˓uAˍPA�~�Aˇ+A˕�A˙�A˙�A˙�A˙�A˛�A˝�A˟�A˟�A˟�A˟�A˟�A˟�A˙�Aˏ\A�1Aɰ!Aŗ�A�&�A�`BA�5?A��A���A���A��!A�^5A��PA�O�A��A���A�dZA��
A�jA���A�A�Q�A�"�A��^A�z�A�JA�  A���A���A�p�A���A��A��PA���A���A�I�A��PA��yA���A���A���A�&�A���A�r�A��A��A�  A���A��uA�z�A��
A���A���A���A�\)A���A���A��HA�ffA��A���A�ffA�1'A���A�\)A�1'A�z�A�I�A��A�S�A�M�A�r�A���A��mA��TA�+A�$�A��;A���A�7LA�n�A�+A�\)A�"�A�x�A�
=A�ȴA�A~{A|��A|n�A| �A{O�Azr�AzI�AzbNAzAw�TAv�DAw�AwG�Av�DAt�!As
=AqoAodZAn��AmK�AkƨAj�Ah�HAg��Ag/Ae��Ad^5Ab{A`�jA_O�A_33A\�!AY�mAX�uAW+AU�PAT�AT�AU\)ATZAS\)AQ/ALA�AI|�AHM�AG�AG+AEO�ADbACx�AB�\AA�hA@v�A>�A=�hA<1A:��A:1A9/A8(�A7XA6�\A5��A5�A4-A3S�A3;dA2M�A.��A,JA+\)A*�yA*��A*ZA)/A'x�A&n�A%�7A$�`A$�DA#�hA#�A"�yA"r�A!t�A �jA 1'A�A�yA1'A{A��A��A�wA��AE�A�-An�AVAbNA  A�A
=A9XA�/A�uA  A��A�HA{A��AJA��At�A�DAbA��A
�Av�A  AK�A��A�#A��AJA��A�/A�hA��A��Ar�A(�A�PA �`A E�@�l�@�{@�Ĝ@�+@��@���@�t�@�-@���@�$�@�7@@�j@�@�E�@�ƨ@�V@���@�1@�{@���@�
=@�l�@�z�@�"�@ޗ�@�J@�G�@�V@ܴ9@�j@�A�@�/@�@�dZ@�X@���@�+@�{@���@��H@�E�@���@���@ǶF@�p�@�1@���@ÍP@�o@��@���@�;d@��+@�5?@��T@�hs@��`@��D@���@�v�@�=q@�@���@�x�@�Z@�dZ@���@�$�@��@�ȴ@��@�"�@�v�@��@�@���@��h@��@��u@�S�@��#@�`B@��@�(�@��w@���@�{@�p�@���@� �@���@�|�@�;d@���@��!@���@�J@��@��D@��w@��+@�=q@��h@�r�@�1@��@���@�1'@�z�@�A�@�b@��
@�|�@��F@�  @���@�\)@�o@��\@�n�@�-@��h@�Ĝ@�  @���@�+@��+@�$�@�@�V@��@��;@�+@�
=@���@�=q@���@��@�hs@�/@���@���@��@�r�@�j@� �@��;@��@�;d@���@��@�@���@��7@�/@���@��@�bN@�\)@��@���@���@�5?@���@��@�p�@��h@�X@�x�@�p�@�x�@�x�@��@�V@���@��D@�t�@�~�@�V@�%@�Q�@��@��
@�|�@�33@��H@���@��+@�ff@�-@���@�?}@�V@��/@���@���@�bN@�;d@��y@��H@���@�~�@�$�@��@��h@�hs@��#@��^@�&�@��@��9@��@��D@�z�@��D@��u@�z�@�bN@�A�@� �@���@��;@���@��@�@�n�@�-@�$�@�J@��#@���@�p�@�O�@�?}@�V@��`@��9@�1'@�@��@;d@~v�@}�T@}��@}/@{�
@tj@j^5@b�H@\�D@V��@Q��@H  @C@=�-@7K�@/�w@,�/@( �@"��@��@�@S�@��@	�#@�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A˓uAˍPA�~�Aˇ+A˕�A˙�A˙�A˙�A˙�A˛�A˝�A˟�A˟�A˟�A˟�A˟�A˟�A˙�Aˏ\A�1Aɰ!Aŗ�A�&�A�`BA�5?A��A���A���A��!A�^5A��PA�O�A��A���A�dZA��
A�jA���A�A�Q�A�"�A��^A�z�A�JA�  A���A���A�p�A���A��A��PA���A���A�I�A��PA��yA���A���A���A�&�A���A�r�A��A��A�  A���A��uA�z�A��
A���A���A���A�\)A���A���A��HA�ffA��A���A�ffA�1'A���A�\)A�1'A�z�A�I�A��A�S�A�M�A�r�A���A��mA��TA�+A�$�A��;A���A�7LA�n�A�+A�\)A�"�A�x�A�
=A�ȴA�A~{A|��A|n�A| �A{O�Azr�AzI�AzbNAzAw�TAv�DAw�AwG�Av�DAt�!As
=AqoAodZAn��AmK�AkƨAj�Ah�HAg��Ag/Ae��Ad^5Ab{A`�jA_O�A_33A\�!AY�mAX�uAW+AU�PAT�AT�AU\)ATZAS\)AQ/ALA�AI|�AHM�AG�AG+AEO�ADbACx�AB�\AA�hA@v�A>�A=�hA<1A:��A:1A9/A8(�A7XA6�\A5��A5�A4-A3S�A3;dA2M�A.��A,JA+\)A*�yA*��A*ZA)/A'x�A&n�A%�7A$�`A$�DA#�hA#�A"�yA"r�A!t�A �jA 1'A�A�yA1'A{A��A��A�wA��AE�A�-An�AVAbNA  A�A
=A9XA�/A�uA  A��A�HA{A��AJA��At�A�DAbA��A
�Av�A  AK�A��A�#A��AJA��A�/A�hA��A��Ar�A(�A�PA �`A E�@�l�@�{@�Ĝ@�+@��@���@�t�@�-@���@�$�@�7@@�j@�@�E�@�ƨ@�V@���@�1@�{@���@�
=@�l�@�z�@�"�@ޗ�@�J@�G�@�V@ܴ9@�j@�A�@�/@�@�dZ@�X@���@�+@�{@���@��H@�E�@���@���@ǶF@�p�@�1@���@ÍP@�o@��@���@�;d@��+@�5?@��T@�hs@��`@��D@���@�v�@�=q@�@���@�x�@�Z@�dZ@���@�$�@��@�ȴ@��@�"�@�v�@��@�@���@��h@��@��u@�S�@��#@�`B@��@�(�@��w@���@�{@�p�@���@� �@���@�|�@�;d@���@��!@���@�J@��@��D@��w@��+@�=q@��h@�r�@�1@��@���@�1'@�z�@�A�@�b@��
@�|�@��F@�  @���@�\)@�o@��\@�n�@�-@��h@�Ĝ@�  @���@�+@��+@�$�@�@�V@��@��;@�+@�
=@���@�=q@���@��@�hs@�/@���@���@��@�r�@�j@� �@��;@��@�;d@���@��@�@���@��7@�/@���@��@�bN@�\)@��@���@���@�5?@���@��@�p�@��h@�X@�x�@�p�@�x�@�x�@��@�V@���@��D@�t�@�~�@�V@�%@�Q�@��@��
@�|�@�33@��H@���@��+@�ff@�-@���@�?}@�V@��/@���@���@�bN@�;d@��y@��H@���@�~�@�$�@��@��h@�hs@��#@��^@�&�@��@��9@��@��D@�z�@��D@��u@�z�@�bN@�A�@� �@���@��;@���@��@�@�n�@�-@�$�@�J@��#@���@�p�@�O�@�?}@�V@��`@��9@�1'@�@��@;d@~v�@}�T@}��@}/@{�
@tj@j^5@b�H@\�D@V��@Q��@H  @C@=�-@7K�@/�w@,�/@( �@"��@��@�@S�@��@	�#@�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bm�Bn�B�+B�^B�mB�BB�B�B�B�)B�/B�)B�/B�NB�ZB�ZB�`B�mB�B�B�mB�`B�HB�HB�;B�BB�;B�5B��BǮBƨBǮB�jB�^B�3B�'B�B��B��B��B��B��B�=B�B|�Bw�Br�Bp�Bl�BcTBYBQ�BH�B?}B7LB33B-B�B%B��B�B�mB�5B�B��B��B�LB��B�DB~�Bq�BdZBYBK�BA�B0!B�BDB  B
�sB
�/B
��B
�9B
�hB
�B
� B
�B
gmB
J�B
B�B
9XB
8RB
6FB
2-B
+B
.B
33B
;dB
.B
'�B
@�B
M�B
E�B
:^B
5?B
+B
"�B
�B
DB	��B	�mB	�5B	�B	�B	��B	��B	�jB	��B	��B	��B	�VB	dZB	VB	F�B	;dB	D�B	O�B	YB	P�B	E�B	,B��B�fB�5B�/B�B��B��B��BɺBŢBB�^B�?B�'B�-B�?B�9B�3B�-B�!B�B�B�B��B��B��B��B��B��B��B��B�{B�bB�VB�PB�DB�=B�7B�1B�1B�+B�%B�%B�%B�+B�%B�B�B�B� B~�B}�B{�Bz�Bx�Bv�Bt�Bs�Bq�Bo�Bm�BjBk�BiyBhsBhsBiyBo�Bm�BiyBl�Bk�Bl�Bk�Bk�BbNBYB\)BYBffBo�BffBaHBbNB\)BS�BP�BO�BO�BN�BN�BN�BM�BM�BK�BJ�BJ�BK�BK�BI�BG�BE�BF�BG�BD�BC�BA�B@�B>wB>wB=qB<jB=qB@�B>wBJ�B^5B`BB`BB^5B[#BZBXBW
BS�BI�BA�B<jB<jB:^B8RB6FB33B33B49B49B33B49B6FB8RB8RB7LB5?B6FB7LB7LB7LB7LB7LB7LB7LB7LB8RB<jB<jB<jB<jB<jB?}BB�BC�BB�BC�BD�BI�BN�BR�BT�BT�BW
BXBZB]/BbNBhsBk�Bm�Bq�Br�Bw�Bz�B|�B� B�B�B�%B�+B�1B�7B�7B�=B�bB�bB�uB��B��B��B��B��B��B��B�B�XB�dB�dB�dB�wBȴB��B��B��B��B�
B�
B�
B�B�)B�;B�ZB�`B�mB�mB�sB�B�B�B��B��B��B��B��B��B��B	B	B	%B	%B	+B	+B	1B	
=B	JB	VB	uB	�B	�B	�B	�B	�B	#�B	+B	C�B	XB	^5B	cTB	dZB	cTB	cTB	dZB	dZB	gmB	jB	o�B	p�B	q�B	u�B	w�B	}�B	�B	� B	|�B	{�B	z�B	{�B	}�B	}�B	}�B	~�B	� B	�B	�B	�B	�+B	�DB	�\B	�bB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�^B	�^B	�dB	�dB	�^B	�^B	�jB	�wB	�wB	�}B	��B	B	ÖB	ÖB	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	�HB	�B
B
DB
�B
$�B
,B
-B
7LB
?}B
I�B
Q�B
VB
YB
]/B
`BB
ffB
m�B
q�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bm�Bp�B�1B��B�B�fB�HB�)B�#B�/B�5B�5B�HB�TB�`B�`B�mB�B�B�B�B�B�fB�NB�BB�HB�TB�TB��BɺB��B��B�wB�}B�RB�3B�-B�B��B��B��B��B�JB�B�By�Bv�Br�Br�BiyB]/BYBN�BE�B9XB6FB5?B%�B	7BB��B�B�;B�B�B��B�^B��B�hB�Bv�BhsB]/BO�BF�B9XB�BhB%B
�B
�NB
��B
�wB
��B
�%B
�B
�DB
n�B
M�B
F�B
:^B
9XB
8RB
49B
+B
.B
49B
?}B
1'B
&�B
@�B
O�B
I�B
>wB
:^B
/B
$�B
�B
\B	��B	�B	�HB	�)B	�)B	�B	��B	��B	�B	��B	��B	��B	hsB	ZB	J�B	=qB	D�B	N�B	\)B	S�B	K�B	9XB	B�yB�BB�;B�;B�
B��B��B��BȴBǮB�qB�XB�?B�9B�RB�LB�FB�9B�-B�'B�'B�B�B�B��B��B��B��B��B��B��B�{B�hB�bB�PB�DB�JB�7B�7B�7B�7B�1B�1B�7B�7B�%B�B�B�B�B�B~�B|�B|�Bz�Bv�Bu�Bs�Bq�Bp�Bn�Bl�Bk�BjBk�Bl�Bs�Bp�BjBm�Bn�Bn�Bm�Bp�BgmB[#B^5BXBffBs�BiyBbNBe`BaHBVBQ�BP�BP�BP�BP�BP�BO�BO�BM�BM�BM�BL�BM�BK�BJ�BG�BG�BI�BG�BE�BC�BC�B@�B?}B?}B?}B?}BC�B>wBI�B`BBaHBaHB_;B\)B[#BYBXBYBO�BE�B@�B?}B;dB:^B8RB7LB49B5?B6FB6FB8RB9XB9XB9XB8RB9XB8RB8RB7LB8RB8RB8RB8RB8RB9XB;dB=qB=qB=qB=qB>wBA�BD�BD�BE�BG�BH�BL�BO�BS�BT�BVBXBYB[#B_;BdZBiyBl�Bn�Br�Bs�Bx�B{�B}�B�B�B�B�+B�1B�7B�7B�=B�JB�bB�hB��B��B��B��B��B��B��B��B�B�^B�jB�jB�jB�wBȴB��B��B�B�B�
B�B�B�B�5B�BB�`B�fB�sB�sB�B�B�B�B��B��B��B��B��B��B��B	B	B	%B	%B	+B	1B		7B	
=B	PB	\B	{B	�B	�B	�B	�B	�B	#�B	%�B	?}B	W
B	_;B	dZB	e`B	dZB	dZB	dZB	dZB	gmB	jB	o�B	p�B	q�B	v�B	w�B	~�B	�B	� B	|�B	|�B	|�B	|�B	~�B	}�B	~�B	� B	�B	�B	�B	�B	�1B	�DB	�bB	�bB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�^B	�^B	�dB	�dB	�dB	�dB	�qB	�wB	�wB	�}B	��B	B	ÖB	ÖB	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	�HB	�B
B
DB
�B
$�B
,B
-B
7LB
?}B
I�B
Q�B
VB
YB
^5B
`BB
gmB
m�B
q�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446392012010314463920120103144639  AO  ARGQ                                                                        20111130135548  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135548  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144639  IP                  G�O�G�O�G�O�                