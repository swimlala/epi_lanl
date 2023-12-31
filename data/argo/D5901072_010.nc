CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:42Z UW 3.1 conversion   
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               
A   AO  20111130143556  20190522121828  1728_5048_010                   2C  D   APEX                            2142                            040306                          846 @�CԴV_�1   @�C�W:�@3Ձ$�/�c�5?|�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZy�DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dy@ D�3D�6fD�� D���D�� D�,�D�|�D���D�3D�)�D�vfD�� D�ٚD� D�c3D�� D���D�#3D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�ffA�ffA�ffA�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC  C�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC@  CA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa��Cc�fCe�fCg�fCi�fCl  Cm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�3Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:�3D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DL� DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVs3DV��DWy�DW��DXy�DX��DYy�DY��DZs3DZ�3D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`�3Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dg� Dh  Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dt  Dty�Dt��Duy�Du��Dy9�D� D�33D���D���D���D�)�D�y�D��fD�  D�&fD�s3DǼ�D��fD��D�` D��D�ٚD�  D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԕ�Aԟ�Aԣ�Aԡ�Aԡ�Aԧ�Aԩ�Aԩ�Aԩ�Aԥ�Aԧ�Aԝ�Aԉ7A�~�A�l�A�=qA��A���A��TA���A�x�Aҙ�AэPAЁA�bA��;A̮A�jA�&�A˙�A�5?A�7LA��AȁA��#AǏ\A�`BA�S�A�7LAƇ+A�(�A�"�A�z�A�;dA��A���A��A�$�A���A��A���A�$�A�t�A�p�A��-A�v�A�XA�;dA��`A�$�A��#A��PA�I�A�p�A���A�9XA���A�G�A�33A��^A�I�A�33A��A��`A�ffA��7A�A�C�A���A��A�dZA��A��yA�ZA�l�A��+A�{A���A�
=A�(�A�&�A�A�A���A���A�1A�(�A�C�A�33A�1A��A��A�t�A� �A���A�%A�bNA��PA�z�A��A��hA�O�A��A�ƨA�"�A|�/Av�yAt$�Ao��Ak%AiO�AhE�Af�Ad�A`ĜA[�AXE�AW?}AV�`AV�jAVz�AU�7AO�7AMp�AL��AK��AJ �AI�7AI
=AH��AF�yAEx�AB�HABv�AA��A@  A?7LA>v�A=�hA<v�A<E�A;��A:��A9�mA97LA5`BA3�A3XA3oA1K�A.I�A-`BA,��A*�RA'�A&�A&A�A%��A$��A#XA"��A"�uA"{A �yAv�A��A?}AAM�A�#A�hA"�A�!A�AO�A~�A �A�Ap�AĜA�AhsA
=A��A�A��A+A�\AZA"�A	��AE�AI�A7LA�A��A9XA��A �@� �@���@��@��7@��@�V@�@���@�!@�-@��-@��`@�ƨ@�+@�-@�A�@旍@�p�@�/@�j@��@�=q@�%@��@�&�@�Ĝ@� �@�ƨ@�|�@�l�@�;d@�o@ڗ�@ٙ�@�Ĝ@�Z@��m@ו�@�V@�@�/@�Z@Ӯ@�ȴ@�^5@�@�7L@��/@Ѓ@�l�@�~�@��@�@�G�@̬@ˮ@˥�@˕�@�t�@��@��@�?}@�%@��/@ȴ9@�r�@��;@ǝ�@ǅ@�\)@�+@�ff@ēu@�|�@�o@�ff@���@���@�x�@��@���@���@���@�33@�=q@��#@���@���@���@��@�/@���@��`@���@�Ĝ@��j@��@��u@��@��@���@��u@�r�@� �@��
@��P@�"�@�@�ȴ@�v�@��@�J@�@��@���@���@�V@��u@�  @�S�@�
=@��H@�ȴ@�~�@��@���@��@�A�@��@��F@�|�@�;d@��@�n�@�J@���@�?}@�bN@�b@��P@�;d@�
=@��H@���@���@���@���@���@���@��\@�E�@��@�x�@�X@�G�@�7L@���@��@��D@�Z@��@�  @��m@��;@�ƨ@��@�t�@��H@�M�@��T@���@�x�@�?}@�%@���@�9X@�l�@���@�^5@�@�x�@��@��@�Ĝ@��@�r�@���@�+@��y@�n�@�@�p�@��@���@�(�@��@�dZ@�"�@�
=@���@�E�@��#@��7@�G�@��`@��@�bN@���@��@���@�t�@�\)@�33@���@�~�@�^5@�5?@�J@�hs@�Ĝ@��u@�r�@�bN@�1'@��;@�dZ@��y@���@�E�@��@��^@���@�p�@�/@���@�bN@�1'@�  @��
@�ƨ@���@�K�@�@��H@��!@��\@��\@�~�@�v�@�^5@�E�@��@���@���@�x�@�O�@�/@���@��j@���@��D@�r�@�Q�@��@���@�\)@�@���@�~�@�V@�5?@��@���@��T@�@��7@�/@�Ĝ@� �@��@��P@�l�@�S�@�p�@�9X@|�j@v��@p1'@l�D@e/@^$�@VV@O�;@HQ�@@bN@8Ĝ@4�/@-/@(1'@$(�@E�@M�@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aԕ�Aԟ�Aԣ�Aԡ�Aԡ�Aԧ�Aԩ�Aԩ�Aԩ�Aԥ�Aԧ�Aԝ�Aԉ7A�~�A�l�A�=qA��A���A��TA���A�x�Aҙ�AэPAЁA�bA��;A̮A�jA�&�A˙�A�5?A�7LA��AȁA��#AǏ\A�`BA�S�A�7LAƇ+A�(�A�"�A�z�A�;dA��A���A��A�$�A���A��A���A�$�A�t�A�p�A��-A�v�A�XA�;dA��`A�$�A��#A��PA�I�A�p�A���A�9XA���A�G�A�33A��^A�I�A�33A��A��`A�ffA��7A�A�C�A���A��A�dZA��A��yA�ZA�l�A��+A�{A���A�
=A�(�A�&�A�A�A���A���A�1A�(�A�C�A�33A�1A��A��A�t�A� �A���A�%A�bNA��PA�z�A��A��hA�O�A��A�ƨA�"�A|�/Av�yAt$�Ao��Ak%AiO�AhE�Af�Ad�A`ĜA[�AXE�AW?}AV�`AV�jAVz�AU�7AO�7AMp�AL��AK��AJ �AI�7AI
=AH��AF�yAEx�AB�HABv�AA��A@  A?7LA>v�A=�hA<v�A<E�A;��A:��A9�mA97LA5`BA3�A3XA3oA1K�A.I�A-`BA,��A*�RA'�A&�A&A�A%��A$��A#XA"��A"�uA"{A �yAv�A��A?}AAM�A�#A�hA"�A�!A�AO�A~�A �A�Ap�AĜA�AhsA
=A��A�A��A+A�\AZA"�A	��AE�AI�A7LA�A��A9XA��A �@� �@���@��@��7@��@�V@�@���@�!@�-@��-@��`@�ƨ@�+@�-@�A�@旍@�p�@�/@�j@��@�=q@�%@��@�&�@�Ĝ@� �@�ƨ@�|�@�l�@�;d@�o@ڗ�@ٙ�@�Ĝ@�Z@��m@ו�@�V@�@�/@�Z@Ӯ@�ȴ@�^5@�@�7L@��/@Ѓ@�l�@�~�@��@�@�G�@̬@ˮ@˥�@˕�@�t�@��@��@�?}@�%@��/@ȴ9@�r�@��;@ǝ�@ǅ@�\)@�+@�ff@ēu@�|�@�o@�ff@���@���@�x�@��@���@���@���@�33@�=q@��#@���@���@���@��@�/@���@��`@���@�Ĝ@��j@��@��u@��@��@���@��u@�r�@� �@��
@��P@�"�@�@�ȴ@�v�@��@�J@�@��@���@���@�V@��u@�  @�S�@�
=@��H@�ȴ@�~�@��@���@��@�A�@��@��F@�|�@�;d@��@�n�@�J@���@�?}@�bN@�b@��P@�;d@�
=@��H@���@���@���@���@���@���@��\@�E�@��@�x�@�X@�G�@�7L@���@��@��D@�Z@��@�  @��m@��;@�ƨ@��@�t�@��H@�M�@��T@���@�x�@�?}@�%@���@�9X@�l�@���@�^5@�@�x�@��@��@�Ĝ@��@�r�@���@�+@��y@�n�@�@�p�@��@���@�(�@��@�dZ@�"�@�
=@���@�E�@��#@��7@�G�@��`@��@�bN@���@��@���@�t�@�\)@�33@���@�~�@�^5@�5?@�J@�hs@�Ĝ@��u@�r�@�bN@�1'@��;@�dZ@��y@���@�E�@��@��^@���@�p�@�/@���@�bN@�1'@�  @��
@�ƨ@���@�K�@�@��H@��!@��\@��\@�~�@�v�@�^5@�E�@��@���@���@�x�@�O�@�/@���@��j@���@��D@�r�@�Q�@��@���@�\)@�@���@�~�@�V@�5?@��@���@��T@�@��7@�/@�Ĝ@� �@��@��P@�l�@�S�@�p�@�9X@|�j@v��@p1'@l�D@e/@^$�@VV@O�;@HQ�@@bN@8Ĝ@4�/@-/@(1'@$(�@E�@M�@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�\B�\B�VB�PB�JB�=B�=B�7B�7B�+B�Bz�Bt�BhsBjB�B�1B�7B�jB�B�B��BBB%BBBBBDBbB�B2-B}�B��B��B��BÖB��B��B�B�B��B�B�
B�
B��B��B�}B��B�hB��B��B��B��B�oB�1B�=B�Br�BhsBZBA�B.B6FBW
B:^B(�B�BJB  B��B�B�/BȴB��B�hB��B��BB��B�B��B�!B�XB�}BƨB��BÖB�9B|�BP�B�B
��B
��B
�'B
��B
y�B
l�B
[#B
VB
G�B
/B
oB	�B	ȴB	�?B	��B	~�B	y�B	r�B	k�B	\)B	C�B	)�B	�B	�B	�B	�B	�B	VB��B��B��B��B��B��B��B�B�mB�sB�mB�sB�BB�)B�#B�NB�HB�`B�B��B��B�
B��B��B�jBÖB�XB�-B�B��B��B��B��B��B��B��B�{B��B��B�{B��B�\B�\B�\B�VB�PB�JB�=B�7B�1B�+B�%B�B�B�B�B�B� B�%B�7B�DB�1B�=B�=B�\B�%B�oB�PB�JB�DB�JB�DB�DB�DB�=B�=B�PB�=B�1B�=B�JB�VB�bB�oB�bB�hB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B�-B�?B�?B�FB�FB�LB�RB�RB�RB�RB�^B�dB�dB�wB�}BĜBĜB��B�
B�)B�TB�fB�B�B�B�B�B�B�B�yB�B�B�B�B�B�B��B��B	  B	B	B	B	%B	+B		7B	
=B	JB	VB	\B	{B	�B	�B	&�B	)�B	)�B	-B	1'B	49B	5?B	9XB	;dB	?}B	C�B	C�B	C�B	C�B	D�B	E�B	G�B	G�B	H�B	H�B	H�B	I�B	J�B	K�B	L�B	M�B	N�B	P�B	P�B	P�B	Q�B	W
B	XB	YB	[#B	^5B	`BB	`BB	`BB	cTB	gmB	jB	k�B	m�B	o�B	o�B	o�B	o�B	q�B	v�B	|�B	}�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�7B	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�'B	�'B	�'B	�'B	�-B	�3B	�?B	�LB	�LB	�RB	�XB	�^B	�^B	�^B	�dB	�jB	�jB	�jB	�wB	�}B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
�B
�B
#�B
%�B
,B
2-B
8RB
:^B
D�B
J�B
Q�B
T�B
[#B
aHB
dZB
iyB
m�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�\B�\B�VB�PB�PB�DB�DB�=B�=B�7B�%B� Bx�Bp�Bn�B�B�7B�=B�wB�5B��BBBB1BBB%B+B\B�B�B7LB�B��B�!B��BŢB��B�B�B�#B�#B�)B�B�B��B��BB��B�oB��B��B��B��B��B�=B�\B�JBu�Br�B`BBG�B1'B<jBe`B=qB)�B�BPBB��B�B�HB��B�B�oB��B��BǮB�TB�B��B�B�RB�}BƨB��BȴB�}B�+BW
B �B
��B
�)B
�?B
�B
�1B
r�B
]/B
[#B
P�B
8RB
�B
B	��B	�wB	��B	�B	|�B	u�B	p�B	e`B	P�B	2-B	"�B	 �B	�B	�B	�B	�B	B��B��B��B��B��B��B�B�B�B�yB�B�ZB�5B�/B�ZB�ZB�fB�B�B��B�B�BĜB�wBĜB�wB�^B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�\B�bB�VB�DB�DB�=B�7B�7B�+B�B�B�%B�+B�+B�=B�=B�PB�=B�DB�JB�oB�oB��B�hB�hB�hB�\B�\B�bB�JB�JB�\B�{B�\B�DB�PB�\B�bB�oB��B�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�9B�FB�FB�LB�LB�LB�XB�XB�XB�^B�jB�jB�jB�}B��BŢBŢB��B�B�/B�ZB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	  B	B	B	%B	+B	1B		7B	
=B	JB	\B	oB	�B	�B	�B	'�B	)�B	+B	.B	1'B	5?B	6FB	9XB	=qB	@�B	C�B	C�B	C�B	C�B	E�B	F�B	G�B	G�B	H�B	H�B	H�B	I�B	J�B	K�B	L�B	M�B	N�B	Q�B	Q�B	Q�B	R�B	W
B	YB	ZB	\)B	^5B	`BB	`BB	`BB	dZB	hsB	k�B	l�B	n�B	p�B	o�B	o�B	o�B	r�B	w�B	}�B	~�B	�B	�B	�B	�B	�+B	�+B	�1B	�+B	�=B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�-B	�'B	�'B	�'B	�'B	�3B	�9B	�FB	�RB	�RB	�RB	�XB	�^B	�dB	�dB	�jB	�qB	�jB	�qB	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�B	�#B	�/B	�)B	�/B	�5B	�5B	�;B	�HB	�HB	�HB	�NB	�NB	�`B	�fB	�ZB	�ZB	�ZB	�ZB	�fB	�mB	�yB	�B	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
�B
�B
#�B
%�B
,B
2-B
8RB
:^B
D�B
J�B
Q�B
T�B
[#B
aHB
dZB
iyB
m�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<#�
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
<D��<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
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
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451562012011014515620120110145156  AO  ARGQ                                                                        20111130143556  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143556  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145156  IP                  G�O�G�O�G�O�                