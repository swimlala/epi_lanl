CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:03Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ,A   AO  20111130140049  20190522121825  1727_5046_044                   2C  D   APEX                            2143                            040306                          846 @�N�ɓ�1   @�N�m�/�@7k��Q��c��E��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��Dey�Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Dss3Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{��C}��C�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��D� D  Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC�3DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DM  DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd�3Des3De��Dfy�Df�3Dgy�Dg��Dhy�Dh��Diy�Di��Djs3Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do�3Dpy�Dp��Dqy�Dq��Dry�Dr��Dsl�Dy� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA��Aʺ^AʮAʣ�Aʟ�Aʝ�Aʛ�Aʗ�Aʗ�Aʕ�AʑhAʍPAʉ7Aʇ+AʅAʅAʅAʃAʃAʅAʁAʁA�~�A�|�A�|�A�z�A�z�A�x�A�x�A�x�A�z�A�x�A�x�A�v�A�v�A�v�A�t�A�l�A�XA�JAǧ�A��/A��;A��+A���A�9XA��mA���A���A�ƨA�JA�5?A���A�$�A��7A�1A��PA��A��A���A��uA��/A�E�A�hsA�^5A���A��A��A�bA�9XA��A�JA���A�bA�^5A���A�bA��DA���A�jA�t�A��9A��/A��\A���A�bNA��^A�z�A���A��/A�l�A�~�A��A��A�t�A��+A��/A��/A�;dA��`A���A�~�A��A���A���A�oA���A�A�A�^5A�n�A�A|�A{�Az��Ay/Awt�Au�At��As��AsO�Aq��Apz�Aox�An{Al�+AkVAi�
Ai+Ah9XAgp�Ag"�Af�Ae��Ac��Ac"�Ab^5Aa�hA`v�A_O�A^ȴA]K�AZȴAYt�AW�mAV �ATjASS�AQ�FAO�AJ�`AI;dAHbAF^5AE�AE"�ACS�AA/A@z�A?�
A>��A=�A<�`A;�FA:�A:Q�A:M�A:E�A9t�A8��A8^5A7�A5A4�9A2�`A21'A1��A0�yA/�A.�A-��A,��A,~�A*�uA)�TA)oA'"�A%�A%"�A$E�A#��A#/A"�9A!t�A �+A��A��A&�A�jAI�A�A��AhsA�/A�wA�/AbA�
A�A;dAAVAĜA5?A�TA�wA��A�9AQ�A��A?}A��A�wA�DA
=A�;A�PA
=A	�7A�A�+AhsA(�A��A7LA bN@��H@���@��j@�  @�bN@���@��@�`B@�=q@�/@��D@�P@�@�!@���@���@�P@�?}@�K�@�~�@�x�@�b@ٙ�@���@�1@��@�A�@���@�-@��@с@д9@�
=@�{@�`B@�Z@�
=@�V@��@�dZ@Ə\@��@���@�V@�A�@�C�@��@§�@�ff@���@��@�I�@� �@���@�\)@�@�~�@��@��^@��/@�33@��h@�j@���@�+@��+@�$�@�x�@��u@���@�ff@�hs@��D@�ƨ@�;d@�^5@��h@��D@��w@���@�V@�$�@��T@�V@�r�@���@�l�@��@��@�G�@���@�
=@�{@��-@�`B@��D@� �@�  @�ƨ@�l�@�E�@���@��h@�G�@��@��u@�bN@��@��m@�ƨ@��P@�K�@���@�v�@�M�@��@���@�O�@�%@���@�bN@�9X@��@��m@�l�@�t�@���@�ƨ@��@��m@��
@��w@���@�S�@�o@��@��@���@�{@��T@���@�&�@��`@��@�z�@�bN@�A�@�b@��w@��@�S�@��@���@�^5@�=q@��R@�"�@���@�n�@��@�M�@�E�@��@�J@��#@�&�@���@���@���@�Ĝ@�b@��@�r�@��@�bN@��@�1'@�Z@�Z@�I�@�(�@���@��@�ƨ@���@�S�@�"�@�@��@��y@�ȴ@���@�v�@�V@�-@�{@���@��#@��-@�O�@�&�@���@�9X@�  @�ƨ@��@�;d@�"�@��@���@�ff@�E�@��#@��h@�X@��/@��j@���@�Q�@��@���@��w@�t�@�;d@��H@��!@���@�~�@�^5@�E�@�5?@�$�@��@�J@��#@�x�@��`@���@��@�bN@�bN@�I�@��;@�dZ@���@��+@�n�@�V@�M�@�M�@�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA��Aʺ^AʮAʣ�Aʟ�Aʝ�Aʛ�Aʗ�Aʗ�Aʕ�AʑhAʍPAʉ7Aʇ+AʅAʅAʅAʃAʃAʅAʁAʁA�~�A�|�A�|�A�z�A�z�A�x�A�x�A�x�A�z�A�x�A�x�A�v�A�v�A�v�A�t�A�l�A�XA�JAǧ�A��/A��;A��+A���A�9XA��mA���A���A�ƨA�JA�5?A���A�$�A��7A�1A��PA��A��A���A��uA��/A�E�A�hsA�^5A���A��A��A�bA�9XA��A�JA���A�bA�^5A���A�bA��DA���A�jA�t�A��9A��/A��\A���A�bNA��^A�z�A���A��/A�l�A�~�A��A��A�t�A��+A��/A��/A�;dA��`A���A�~�A��A���A���A�oA���A�A�A�^5A�n�A�A|�A{�Az��Ay/Awt�Au�At��As��AsO�Aq��Apz�Aox�An{Al�+AkVAi�
Ai+Ah9XAgp�Ag"�Af�Ae��Ac��Ac"�Ab^5Aa�hA`v�A_O�A^ȴA]K�AZȴAYt�AW�mAV �ATjASS�AQ�FAO�AJ�`AI;dAHbAF^5AE�AE"�ACS�AA/A@z�A?�
A>��A=�A<�`A;�FA:�A:Q�A:M�A:E�A9t�A8��A8^5A7�A5A4�9A2�`A21'A1��A0�yA/�A.�A-��A,��A,~�A*�uA)�TA)oA'"�A%�A%"�A$E�A#��A#/A"�9A!t�A �+A��A��A&�A�jAI�A�A��AhsA�/A�wA�/AbA�
A�A;dAAVAĜA5?A�TA�wA��A�9AQ�A��A?}A��A�wA�DA
=A�;A�PA
=A	�7A�A�+AhsA(�A��A7LA bN@��H@���@��j@�  @�bN@���@��@�`B@�=q@�/@��D@�P@�@�!@���@���@�P@�?}@�K�@�~�@�x�@�b@ٙ�@���@�1@��@�A�@���@�-@��@с@д9@�
=@�{@�`B@�Z@�
=@�V@��@�dZ@Ə\@��@���@�V@�A�@�C�@��@§�@�ff@���@��@�I�@� �@���@�\)@�@�~�@��@��^@��/@�33@��h@�j@���@�+@��+@�$�@�x�@��u@���@�ff@�hs@��D@�ƨ@�;d@�^5@��h@��D@��w@���@�V@�$�@��T@�V@�r�@���@�l�@��@��@�G�@���@�
=@�{@��-@�`B@��D@� �@�  @�ƨ@�l�@�E�@���@��h@�G�@��@��u@�bN@��@��m@�ƨ@��P@�K�@���@�v�@�M�@��@���@�O�@�%@���@�bN@�9X@��@��m@�l�@�t�@���@�ƨ@��@��m@��
@��w@���@�S�@�o@��@��@���@�{@��T@���@�&�@��`@��@�z�@�bN@�A�@�b@��w@��@�S�@��@���@�^5@�=q@��R@�"�@���@�n�@��@�M�@�E�@��@�J@��#@�&�@���@���@���@�Ĝ@�b@��@�r�@��@�bN@��@�1'@�Z@�Z@�I�@�(�@���@��@�ƨ@���@�S�@�"�@�@��@��y@�ȴ@���@�v�@�V@�-@�{@���@��#@��-@�O�@�&�@���@�9X@�  @�ƨ@��@�;d@�"�@��@���@�ff@�E�@��#@��h@�X@��/@��j@���@�Q�@��@���@��w@�t�@�;d@��H@��!@���@�~�@�^5@�E�@�5?@�$�@��@�J@��#@�x�@��`@���@��@�bN@�bN@�I�@��;@�dZ@���@��+@�n�@�V@�M�@�M�@�w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�sB�yB�sB�yB�yB�yB�yB�B�B�5B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�`B�ZB�BB�5B�#B�B�B��B��B��BɺBǮBÖB�}B�qB�XB�LB�9B�'B�B��B�PBz�Bw�BgmBZBN�BJ�BJ�BF�B<jB,B(�B#�B�BPB%B��B��B�B�sB�TB�B��B��BŢBB�RB��B�7By�BbNBQ�B<jB�BB
�B
��B
ɺB
��B
�XB
�?B
�9B
�B
�B
��B
��B
�\B
�DB
~�B
s�B
k�B
^5B
XB
O�B
E�B
;dB
49B
/B
,B
)�B
#�B
�B
�B
VB
%B	��B	��B	�B	�B	�ZB	�NB	�5B	��B	B	ŢB	B	�qB	�LB	�B	�B	��B	�oB	�B	x�B	m�B	dZB	[#B	R�B	>wB	&�B	�B	�B	oB	hB	JB		7B��B��B��B�B�B�yB�B�sB�mB�mB�yB�B�yB�B�`B�5B�B��B��B��BǮBÖBÖB��B�^B�dB�9B�9B�B��B��B��B��B��B��B��B��B�uB�uB�\B�PB�JB�DB�=B�7B�+B�B�B�B� B~�B}�B}�B{�Bx�Bv�Bu�Bu�Bt�Br�Bq�Bo�Bn�Bl�BjBgmBdZB`BB_;B^5B[#BVBS�BO�BM�BL�BI�BH�BD�B?}B>wB=qB?}BH�BF�B@�B9XB6FB9XB8RB5?B33B49B33B2-B.B,B,B,B+B)�B)�B(�B'�B&�B(�B+B+B+B)�B)�B+B-B-B/B2-B33B33B2-B33B49B5?B5?B5?B8RB8RB9XB9XB;dB=qB>wB?}B>wB?}B@�BB�BB�BB�BA�B?}BB�BC�BE�BF�BJ�BJ�BK�BK�BK�BM�BO�BP�BQ�BR�BS�BT�BXBZB_;B`BBaHBaHBcTBo�Bz�B~�B�B�B�B�B�7B�DB�PB�VB�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�3B�?B�LB�dB�qB�}B��B��BÖB��B��B��B��B�
B�B�#B�/B�BB�TB�mB�B�B�B��B��B��B��B	B	B	B	%B	1B	DB	PB	VB	bB	oB	uB	�B	�B	�B	!�B	%�B	#�B	&�B	.B	33B	<jB	?}B	E�B	G�B	H�B	H�B	I�B	I�B	L�B	R�B	T�B	VB	XB	[#B	_;B	aHB	bNB	cTB	e`B	ffB	gmB	hsB	jB	k�B	m�B	n�B	o�B	o�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	x�B	z�B	~�B	� B	�B	�%B	�1B	�JB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�?B	�LB	�XB	�^B	�^B	�dB	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�yB�B�sB�yB�yB�yB�yB�B�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�ZB�TB�5B�/B�)B��B��B��B��B��BÖB��B�jB�^B�RB�9B�3B��B�oB|�B}�Bk�B]/BT�BN�BO�BJ�BA�B,B,B,B �BbB
=BB��B�B�B�mB�/B��B��BȴBƨB�}B�B�VB�BgmBZBH�B �B%B
��B
�B
��B
B
�^B
�FB
�FB
�!B
�'B
��B
��B
�hB
�\B
�B
v�B
q�B
aHB
[#B
S�B
J�B
@�B
7LB
1'B
.B
/B
'�B
 �B
�B
uB
DB
B	��B	��B	�B	�`B	�ZB	�NB	�B	ĜB	ǮB	ŢB	��B	�dB	�'B	�-B	�B	��B	�=B	}�B	r�B	gmB	`BB	ZB	L�B	,B	!�B	�B	{B	oB	bB	VB	B��B��B�B�B�B�B�yB�mB�mB�B�B�B�B�yB�HB�;B��B��B��B��BƨBƨBÖB�dB��B�FB�LB�?B�B��B��B��B��B��B��B��B��B��B�{B�\B�VB�JB�DB�=B�7B�1B�B�B�B� B~�B~�B~�B~�Bx�Bv�Bv�Bu�Bu�Bs�Bq�Bp�Bn�Bn�Bk�BiyBdZB`BB`BB_;B[#BW
BR�BP�BO�BN�BJ�BG�B@�B@�B>wB?}BK�BJ�BD�B=qB8RB:^B:^B8RB8RB9XB6FB6FB2-B/B.B.B.B.B+B+B)�B,B,B,B,B+B)�B,B,B.B/B1'B33B5?B6FB33B49B5?B6FB6FB7LB9XB9XB:^B:^B<jB>wB>wB@�B?}B@�BA�BC�BC�BD�BD�BB�BD�BD�BF�BG�BK�BK�BM�BL�BN�BO�BQ�BR�BR�BT�BVBW
BZB\)B`BBaHBbNBcTBdZBo�B}�B� B�B�+B�%B�B�=B�JB�VB�\B�oB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�9B�FB�RB�jB�wB�}B��B��BĜB��B��B��B��B�
B�B�#B�/B�HB�ZB�mB�B�B�B��B��B��B	  B	B	B	B	%B		7B	JB	VB	\B	hB	uB	{B	�B	�B	�B	!�B	'�B	$�B	&�B	.B	49B	<jB	@�B	F�B	G�B	H�B	H�B	J�B	J�B	K�B	R�B	T�B	VB	YB	[#B	_;B	aHB	bNB	cTB	e`B	ffB	gmB	hsB	k�B	k�B	m�B	n�B	o�B	o�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	x�B	{�B	~�B	�B	�B	�+B	�7B	�PB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�3B	�FB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	B	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�H1111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446492012010314464920120103144649  AO  ARGQ                                                                        20111130140049  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140049  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144649  IP                  G�O�G�O�G�O�                