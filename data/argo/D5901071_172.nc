CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:39Z UW 3.1 conversion   
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
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ex   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  o    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143043  20190522121827  1727_5046_172                   2C  D   APEX                            2143                            040306                          846 @���π 1   @���)���@5�XbM��c��/��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  @���A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BG��BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� DqfDq�fDr  Dr� Ds  DsffDz  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@  @�ff@�ffA��A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B)33B0��B8��B@��BHffBPffBXffB`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C0L�C2L�C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DfD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*3D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��DofDo��Dp�Dp��Dq3Dq�3Dr�Dr��Ds�Dss3Dz,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Aԙ�A�XA�G�A�=qA�33A�+A�$�A� �A��A��A�VA�
=A�1A���A���A��/A���AӰ!AӓuA�(�A��Aβ-A�+Aʲ-A��A�t�AžwA�K�A�G�A���A��/A���A��hA���A�(�A��A��wA�XA�oA���A��^A��!A�M�A�l�A�(�A�ĜA���A�G�A�JA��A��7A�I�A��TA�?}A��A���A�33A��PA���A�VA��RA�XA�VA�ȴA�\)A�oA�E�A�dZA�XA�VA�VA�VA�O�A�K�A�C�A�7LA�"�A�Q�A��A��FA�33A��A��FA��DA�p�A���A�-A�G�A��9A��RA�v�A��;A��
A�ffA��A���A���A�r�A�A�x�A�5?A���A�Q�A�  A��A���A�A�  A���A�A�+A�\)A�/A��jA�n�A�M�A�
=AƨA��A~9XA|��A{�mAq�Ak�;AkVAh(�Ag7LAf��Ad�jAd$�Ab��Aa%A_�^A^Q�A[�hAW�AW7LAV=qAU��AUK�AT1AR�AM?}AL�AKAI��AIdZAI"�AG�^AF=qAE&�ABVA<�9A:��A8ȴA6��A6ZA6  A5oA4 �A3;dA2�A2ȴA2��A2�A1ƨA/|�A.�jA.  A,I�A+��A+VA*r�A)��A)/A'�PA&�A&1A%�TA%&�A$�jA$��A$�DA$  A#�FA#`BA"��A"  A!`BA!?}A!
=A��A��AZA{A�A��A^5A��A�A5?A��A�A��AdZA��Az�A��AI�A��A�A�mAAv�A�PA?}A�A�A��A33A�DA�AĜA�
A�An�A�7@���@���@�n�@��#@�33@��`@��;@�X@�1'@�ƨ@�h@�A�@�|�@�R@�n�@��`@��@�F@�"�@�ȴ@�ff@�hs@�%@�bN@�E�@��@�+@�5?@��T@�Ĝ@�A�@ݩ�@�\)@���@���@�33@�I�@��m@�S�@�E�@�&�@�/@мj@��@�"�@Η�@���@�bN@ʸR@ʰ!@���@�
=@��@��@��@��y@ʧ�@��/@�X@�z�@�l�@���@��j@��
@��P@�(�@���@�M�@��^@�&�@���@��@�A�@���@��P@�S�@��R@�n�@�V@�=q@�{@��T@�7L@�(�@��
@�l�@�33@�o@�
=@�33@�33@���@��@��!@��\@���@���@���@��F@���@�|�@�33@��@���@�^5@�V@�J@��-@�O�@�7L@�/@���@��@�(�@�b@��;@���@�"�@��@��\@���@���@��+@�v�@�^5@�5?@���@�7L@�?}@�X@��@��@�dZ@���@�
=@�~�@��@�7L@���@�Q�@��@�\)@��y@�-@�$�@��@�@���@�X@�G�@�7L@�/@�%@��`@���@�z�@�Z@��@��;@��@��
@��@���@�S�@��@�5?@���@��T@���@��@��9@���@��P@�S�@���@���@���@���@� �@���@���@��@��#@���@�7L@��`@��@��u@�z�@�Z@�A�@��;@��@��H@�V@�=q@�J@��-@�V@���@��`@��`@�Ĝ@��9@���@�z�@�Z@� �@�1@��m@���@��F@��F@��@�J@��#@��T@��@���@���@�`B@�&�@�Ĝ@��9@��D@�Z@� �@���@�\)@�@���@�^5@��T@���@���@���@���@��-@���@��7@�p�@�X@�?}@��@��@�V@���@���@���@��D@�A�@�b@��w@�t�@�
=@��!@�V@�hs@�ƨ@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�Aԙ�A�XA�G�A�=qA�33A�+A�$�A� �A��A��A�VA�
=A�1A���A���A��/A���AӰ!AӓuA�(�A��Aβ-A�+Aʲ-A��A�t�AžwA�K�A�G�A���A��/A���A��hA���A�(�A��A��wA�XA�oA���A��^A��!A�M�A�l�A�(�A�ĜA���A�G�A�JA��A��7A�I�A��TA�?}A��A���A�33A��PA���A�VA��RA�XA�VA�ȴA�\)A�oA�E�A�dZA�XA�VA�VA�VA�O�A�K�A�C�A�7LA�"�A�Q�A��A��FA�33A��A��FA��DA�p�A���A�-A�G�A��9A��RA�v�A��;A��
A�ffA��A���A���A�r�A�A�x�A�5?A���A�Q�A�  A��A���A�A�  A���A�A�+A�\)A�/A��jA�n�A�M�A�
=AƨA��A~9XA|��A{�mAq�Ak�;AkVAh(�Ag7LAf��Ad�jAd$�Ab��Aa%A_�^A^Q�A[�hAW�AW7LAV=qAU��AUK�AT1AR�AM?}AL�AKAI��AIdZAI"�AG�^AF=qAE&�ABVA<�9A:��A8ȴA6��A6ZA6  A5oA4 �A3;dA2�A2ȴA2��A2�A1ƨA/|�A.�jA.  A,I�A+��A+VA*r�A)��A)/A'�PA&�A&1A%�TA%&�A$�jA$��A$�DA$  A#�FA#`BA"��A"  A!`BA!?}A!
=A��A��AZA{A�A��A^5A��A�A5?A��A�A��AdZA��Az�A��AI�A��A�A�mAAv�A�PA?}A�A�A��A33A�DA�AĜA�
A�An�A�7@���@���@�n�@��#@�33@��`@��;@�X@�1'@�ƨ@�h@�A�@�|�@�R@�n�@��`@��@�F@�"�@�ȴ@�ff@�hs@�%@�bN@�E�@��@�+@�5?@��T@�Ĝ@�A�@ݩ�@�\)@���@���@�33@�I�@��m@�S�@�E�@�&�@�/@мj@��@�"�@Η�@���@�bN@ʸR@ʰ!@���@�
=@��@��@��@��y@ʧ�@��/@�X@�z�@�l�@���@��j@��
@��P@�(�@���@�M�@��^@�&�@���@��@�A�@���@��P@�S�@��R@�n�@�V@�=q@�{@��T@�7L@�(�@��
@�l�@�33@�o@�
=@�33@�33@���@��@��!@��\@���@���@���@��F@���@�|�@�33@��@���@�^5@�V@�J@��-@�O�@�7L@�/@���@��@�(�@�b@��;@���@�"�@��@��\@���@���@��+@�v�@�^5@�5?@���@�7L@�?}@�X@��@��@�dZ@���@�
=@�~�@��@�7L@���@�Q�@��@�\)@��y@�-@�$�@��@�@���@�X@�G�@�7L@�/@�%@��`@���@�z�@�Z@��@��;@��@��
@��@���@�S�@��@�5?@���@��T@���@��@��9@���@��P@�S�@���@���@���@���@� �@���@���@��@��#@���@�7L@��`@��@��u@�z�@�Z@�A�@��;@��@��H@�V@�=q@�J@��-@�V@���@��`@��`@�Ĝ@��9@���@�z�@�Z@� �@�1@��m@���@��F@��F@��@�J@��#@��T@��@���@���@�`B@�&�@�Ĝ@��9@��D@�Z@� �@���@�\)@�@���@�^5@��T@���@���@���@���@��-@���@��7@�p�@�X@�?}@��@��@�V@���@���@���@��D@�A�@�b@��w@�t�@�
=@��!@�V@�hs@�ƨ@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�'B�!B�!B�'B�-B�-B�-B�-B�3B�9B�9B�9B�3B�-B�-B�-B�!B�B�B�B��B�{BhsBVBN�BO�Bm�BhsBhsBaHBW
B]/B^5B^5BffBl�Bl�Bn�Br�Bu�Bw�Bw�Bw�By�B�B�bB�uB�{B��B��B��B��B��B�?B�jB�jB�wB�wB��BÖBB��B��B��B��B��B�wB�}BĜBŢBǮBǮBǮBǮBǮBƨBĜBĜB�XB��B��B�hB�Bz�By�Bw�Bu�Bn�BffB`BBT�BA�B1'B�BB�TB��B{�Bp�B_;BF�BB�B:^B$�BB
�B
��B
�?B
�B
��B
��B
��B
�=B
}�B
aHB
N�B
K�B
H�B
E�B
B�B
7LB
,B
�B	�B	�?B	�'B	��B	��B	��B	�JB	�7B	�B	q�B	gmB	_;B	T�B	J�B	F�B	B�B	>wB	:^B	2-B	%�B	�B	{B	hB	VB	JB	
=B	+B	B	B�B��BǮB��B��B�qB�XB�FB�9B�?B�FB�?B�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�bB�bB�VB�PB�DB�=B�7B�+B�B~�B�B|�B{�B{�Bz�Bz�By�Bx�Bw�Bv�Bv�Bv�Bu�Bu�Bs�Bq�Bq�Bq�Bq�Bp�Bn�Bk�Bo�Bo�Bn�Bk�Bk�BhsBffBhsBiyBiyBgmBgmBe`BcTBdZBffBhsBgmBffBffBdZBe`BffBffBffBe`BffBffBe`BffBffBdZBe`BgmBjBm�Bk�Bk�BjBl�Br�Bu�Bx�B�B�B�B�PB�uB��B��B��B��B��B�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�LB�wBĜBȴB��B��B��B��B�B�
B�B�;B�BB�NB�fB�sB�yB�yB�B�B�B�yB�B�B�B�B�B��B��B��B��B��B��B	%B	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	)�B	+B	1'B	7LB	8RB	9XB	;dB	=qB	@�B	B�B	H�B	J�B	L�B	N�B	O�B	Q�B	VB	[#B	\)B	\)B	]/B	]/B	^5B	`BB	cTB	hsB	k�B	k�B	l�B	l�B	k�B	l�B	n�B	o�B	s�B	s�B	s�B	s�B	t�B	w�B	x�B	x�B	x�B	y�B	y�B	x�B	y�B	z�B	{�B	}�B	� B	�B	�+B	�1B	�=B	�=B	�JB	�PB	�\B	�\B	�VB	�\B	�bB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�'B	�3B	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�dB	�qB	��B	��B	B	B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�FB�'B�!B�'B�-B�-B�-B�-B�3B�9B�9B�9B�3B�-B�-B�-B�!B�B�B�B��B��Bl�BYBR�BVBs�Bn�Bq�BhsB\)B^5B_;BbNBjBl�Bn�Bp�Bt�Bv�Bx�Bw�Bz�B~�B�7B�oB�uB��B��B��B��B��B�B�RB�qB�wB��B��BĜBŢBŢBÖBÖBÖBÖBBBĜBĜBŢBǮBǮBǮBǮBǮBǮB��B��BŢB�'B��B��B�+B{�Bz�Bz�Bz�Bs�BiyBdZBZBG�B5?B$�BDB��B��B~�By�BffBH�BD�BB�B0!B%B
��B
�B
�RB
�B
��B
��B
��B
�bB
�B
gmB
O�B
L�B
I�B
E�B
E�B
:^B
/B
2-B	�`B	�RB	�XB	��B	��B	��B	�VB	�JB	�=B	u�B	k�B	hsB	`BB	L�B	I�B	D�B	?}B	>wB	9XB	5?B	�B	�B	{B	\B	PB	PB	
=B	B	+B��B�B��BƨBB�wB�jB�XB�LB�FB�LB�FB�FB�!B�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�hB�uB�bB�JB�DB�=B�1B�7B�=B�1B�B�B}�B{�B{�Bz�By�By�By�Bw�Bv�Bu�Bv�Bv�Bs�Br�Br�Br�Bq�Br�Br�Bq�Br�Bq�Bq�Bo�Bk�Bk�BjBiyBjBk�Bk�BgmBgmBffBgmBl�BiyBhsBgmBe`BhsBhsBgmBgmBffBgmBhsBffBhsBjBhsBgmBiyBk�Bo�Bl�Bp�Bn�Bm�Br�Bu�B}�B�=B�B�B�\B�uB��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�9B�RB�wBÖB��B��B��B��B��B�B�B�B�;B�BB�TB�fB�sB�yB�yB�B�B�B�yB�B�B�B�B�B��B��B��B��B��B��B	1B	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	)�B	+B	1'B	8RB	8RB	9XB	;dB	>wB	@�B	C�B	H�B	J�B	L�B	N�B	O�B	Q�B	W
B	\)B	\)B	\)B	^5B	_;B	_;B	aHB	bNB	iyB	l�B	l�B	m�B	m�B	l�B	m�B	o�B	p�B	s�B	s�B	s�B	s�B	u�B	w�B	x�B	x�B	x�B	y�B	y�B	x�B	y�B	z�B	{�B	}�B	� B	�B	�+B	�1B	�DB	�DB	�PB	�PB	�\B	�\B	�bB	�bB	�bB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�9B	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�wB	��B	��B	B	B	B	ĜB	ÖB	ŢB	ĜB	ĜB	ĜB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	��B	��B	��B	��B	��B	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�9X<#�
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
<�9X<u<#�
<#�
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
<u<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447342012010314473420120103144734  AO  ARGQ                                                                        20111130143043  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143043  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144734  IP                  G�O�G�O�G�O�                