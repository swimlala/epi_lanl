CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:38Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142938  20190522121827  1727_5046_167                   2C  D   APEX                            2143                            040306                          846 @��nW�1   @��/hP@6A�7Kƨ�cڏ\(��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C�fC�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-fD-�fD.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@L��@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffC 33C33C33C33C33C
33C33C�C�C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`L�Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D�fD�D��D�D��D�D��D�D��D�D�fD�D��D	�D	��D
�D
��D�D��D�D��D�D�3D3D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�fD$�D$��D%fD%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,�3D-3D-�3D.3D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6fD6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE�3DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De3De��Df�Df��Dg�Dg��Dh�Dh�fDi�Di��DjfDj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dy�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���Aқ�A҉7A�  AѓuA�hsA�XA�M�A�C�A�9XA�33A�(�A�&�A��A�
=A�A���A��yAϴ9Aɴ9A�t�A�|�A��#A�v�AĬA�1'A���A7A�7LA�oA���A�-A��
A�jA� �A��A��
A��A��A��^A�x�A��A��DA�ZA��7A�1A��A���A��A��A�`BA��hA�JA�{A���A�33A�  A��jA���A��
A���A��A�ȴA�C�A��wA��\A�ƨA��A�`BA�
=A��`A�XA�1'A��A�A��A��A��HA���A��DA�{A��-A�n�A���A��A��A���A�JA��^A�dZA���A�?}A���A���A���A��A��;A�G�A�I�A�VA��^A��A��wA��A�XA�7LA���A��DA�-A��A�1A�M�A���A�dZA���A���A��A|�uAv�RAs��Ap��Ak�FAjn�Ajr�AjZAi��Ah�Ae"�Ad��Ad�uAdn�Ac��Ac%Aa`BA_O�A]G�AY��AV�HAUS�AT��ASAR��AP5?AL$�AJ��AIƨAIXAH�AH-AG�^AE�AD�DAD�AC�AA��A>�HA<ZA;+A:�!A9��A9oA7�A6��A5A41'A25?A1K�A0��A0 �A/\)A.�DA.VA-/A*��A*A)��A(~�A%33A$ZA#��A#�FA#��A#�hA"Q�AS�A�uA�Al�Az�AJA�^A?}A��A�
A\)A �A�hAK�A(�A�jA�AdZA�A�-A33A�+A�TA�`A�mAl�A?}A�A��A�FAA
��A
VA	��A�Ar�A1'A��A\)AȴA�#A�A�AM�A
=A jA -@��@��+@��#@���@�Ĝ@�b@�l�@�;d@�~�@�C�@�33@�p�@�r�@@�33@�M�@��#@�h@�`B@�V@�bN@�@���@���@�ff@�(�@��T@���@���@�"�@��@���@�~�@�C�@�$�@��@�C�@թ�@Լj@��m@ӍP@�K�@��@�x�@Ͼw@ϕ�@��@͡�@�/@��m@�C�@�ȴ@�-@ɲ-@ɑh@�X@�x�@ɑh@Ɂ@�G�@�Ĝ@���@Ɵ�@��@�7L@ă@�|�@��;@�1'@�+@�n�@���@�z�@�1'@��@�o@���@���@���@���@��7@��h@���@��`@�b@�;d@��y@�@���@�ff@��@�bN@��@�o@��y@�~�@�{@��7@�V@��D@��m@��@��!@�^5@�5?@�J@��T@��-@��^@��T@���@���@��T@��T@��h@��@�(�@���@���@�dZ@�K�@�33@�"�@�o@�ȴ@�n�@�ff@�{@���@��T@�`B@��@�A�@��
@���@�S�@�ff@��-@�@�@�J@�`B@�O�@�X@�`B@�`B@�hs@�/@���@���@��u@�r�@��w@�S�@�o@��y@�ȴ@���@��+@�n�@�^5@�^5@�M�@��@��@��@��@� �@��
@���@�5?@��^@�O�@���@��j@��@��u@��D@�Q�@� �@�b@�b@�Z@��9@��j@���@�j@�A�@�  @��@��w@�ƨ@�9X@���@��@�Q�@��@�"�@�ȴ@�n�@�-@�@��#@��h@�O�@�7L@�7L@��@��/@��D@�A�@��
@�K�@�@�33@�
=@��!@�~�@�=q@�{@��@��^@���@��@�&�@��j@�r�@� �@��@��@��P@�l�@�33@��@��!@�$�@��@���@�7L@�V@��/@��@���@��D@��@�z�@�r�@�bN@�A�@���@�t�@�\)@�\)@�S�@��@���@�=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���Aқ�A҉7A�  AѓuA�hsA�XA�M�A�C�A�9XA�33A�(�A�&�A��A�
=A�A���A��yAϴ9Aɴ9A�t�A�|�A��#A�v�AĬA�1'A���A7A�7LA�oA���A�-A��
A�jA� �A��A��
A��A��A��^A�x�A��A��DA�ZA��7A�1A��A���A��A��A�`BA��hA�JA�{A���A�33A�  A��jA���A��
A���A��A�ȴA�C�A��wA��\A�ƨA��A�`BA�
=A��`A�XA�1'A��A�A��A��A��HA���A��DA�{A��-A�n�A���A��A��A���A�JA��^A�dZA���A�?}A���A���A���A��A��;A�G�A�I�A�VA��^A��A��wA��A�XA�7LA���A��DA�-A��A�1A�M�A���A�dZA���A���A��A|�uAv�RAs��Ap��Ak�FAjn�Ajr�AjZAi��Ah�Ae"�Ad��Ad�uAdn�Ac��Ac%Aa`BA_O�A]G�AY��AV�HAUS�AT��ASAR��AP5?AL$�AJ��AIƨAIXAH�AH-AG�^AE�AD�DAD�AC�AA��A>�HA<ZA;+A:�!A9��A9oA7�A6��A5A41'A25?A1K�A0��A0 �A/\)A.�DA.VA-/A*��A*A)��A(~�A%33A$ZA#��A#�FA#��A#�hA"Q�AS�A�uA�Al�Az�AJA�^A?}A��A�
A\)A �A�hAK�A(�A�jA�AdZA�A�-A33A�+A�TA�`A�mAl�A?}A�A��A�FAA
��A
VA	��A�Ar�A1'A��A\)AȴA�#A�A�AM�A
=A jA -@��@��+@��#@���@�Ĝ@�b@�l�@�;d@�~�@�C�@�33@�p�@�r�@@�33@�M�@��#@�h@�`B@�V@�bN@�@���@���@�ff@�(�@��T@���@���@�"�@��@���@�~�@�C�@�$�@��@�C�@թ�@Լj@��m@ӍP@�K�@��@�x�@Ͼw@ϕ�@��@͡�@�/@��m@�C�@�ȴ@�-@ɲ-@ɑh@�X@�x�@ɑh@Ɂ@�G�@�Ĝ@���@Ɵ�@��@�7L@ă@�|�@��;@�1'@�+@�n�@���@�z�@�1'@��@�o@���@���@���@���@��7@��h@���@��`@�b@�;d@��y@�@���@�ff@��@�bN@��@�o@��y@�~�@�{@��7@�V@��D@��m@��@��!@�^5@�5?@�J@��T@��-@��^@��T@���@���@��T@��T@��h@��@�(�@���@���@�dZ@�K�@�33@�"�@�o@�ȴ@�n�@�ff@�{@���@��T@�`B@��@�A�@��
@���@�S�@�ff@��-@�@�@�J@�`B@�O�@�X@�`B@�`B@�hs@�/@���@���@��u@�r�@��w@�S�@�o@��y@�ȴ@���@��+@�n�@�^5@�^5@�M�@��@��@��@��@� �@��
@���@�5?@��^@�O�@���@��j@��@��u@��D@�Q�@� �@�b@�b@�Z@��9@��j@���@�j@�A�@�  @��@��w@�ƨ@�9X@���@��@�Q�@��@�"�@�ȴ@�n�@�-@�@��#@��h@�O�@�7L@�7L@��@��/@��D@�A�@��
@�K�@�@�33@�
=@��!@�~�@�=q@�{@��@��^@���@��@�&�@��j@�r�@� �@��@��@��P@�l�@�33@��@��!@�$�@��@���@�7L@�V@��/@��@���@��D@��@�z�@�r�@�bN@�A�@���@�t�@�\)@�\)@�S�@��@���@�=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBl�Bl�Bl�Bk�BjBl�Bk�Bk�Bk�Bk�Bk�BjBjBiyBhsBgmBffBcTBT�B:^B;dB:^B7LB49B2-B6FB8RB:^B=qB@�BF�BW
BVBW
BjBq�Bu�B�%B�uB��B��B�{B��B��B��B��B�B�B�B�B�B�LB�jBŢBƨBƨBŢB��B�qB�XB�9B�B��B��B��B��B�{B�oB�bB�=B�B}�B}�B|�B{�Bz�Bz�By�Bx�Bu�Bo�BcTB`BB\)BW
BN�B;dB2-B)�B�BJB%B  B��B�HB�qB�B��B}�Bm�BbNBP�B?}B �B
��B
�;B
��B
ŢB
�jB
�B
��B
� B
k�B
aHB
VB
G�B
6FB
�B	��B	�BB	��B	�!B	�B	��B	��B	��B	��B	�1B	�%B	�%B	�B	�B	|�B	u�B	gmB	YB	G�B	;dB	49B	0!B	)�B	!�B	JB��B��B��B�B�B�B�sB�5B�B��B��BȴBŢB�}B�9B��B��B��B��B��B�oB�DB�+B�B�B�B� B� B}�Bz�Bz�By�Bv�Br�Br�Br�Br�Bq�Bp�Bn�BjBl�Bl�Bk�BjBjBjBjBiyBiyBjBk�Bl�Bk�BjBiyBjBjBk�BiyBk�Bk�BjBiyBiyBiyBiyBhsBgmBgmBffBffBffBdZBcTBcTBcTBbNBbNBaHB_;B\)B\)B\)B\)B^5B`BB`BB_;BaHBbNBdZBcTBdZBe`Be`BdZBiyBp�Br�Br�Bs�Bs�Bt�Bt�Bt�Bs�Bs�Bt�Bs�Bs�Bs�Bt�Bt�Bu�Bv�Bw�Bw�Bw�Bu�Bs�Bw�Bx�By�B}�B�B�B�7B�PB�\B�VB�uB��B��B��B��B�B�'B�-B�9B�LB�XB�XB�qB��BĜBɺB��B��B��BɺBȴBȴBȴBȴB��B�)B�#B�B�)B�;B�BB�;B�5B�/B�5B�BB�NB�TB�fB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	  B��B��B��B	  B	B	B	B	%B	1B		7B	JB	bB	bB	�B	�B	�B	!�B	$�B	(�B	0!B	33B	49B	5?B	5?B	7LB	8RB	8RB	9XB	:^B	:^B	;dB	;dB	:^B	<jB	?}B	@�B	@�B	?}B	D�B	J�B	Q�B	XB	\)B	]/B	^5B	^5B	^5B	^5B	^5B	bNB	iyB	l�B	m�B	m�B	m�B	l�B	o�B	p�B	q�B	s�B	t�B	u�B	v�B	w�B	x�B	z�B	|�B	~�B	�B	�VB	�bB	�bB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�!B	�-B	�9B	�?B	�?B	�FB	�LB	�LB	�RB	�RB	�RB	�LB	�LB	�RB	�^B	�jB	�qB	�wB	�wB	�}B	��B	��B	��B	B	B	ÖB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bl�Bl�Bo�Bm�Bk�Bl�Bk�Bk�Bk�Bk�Bk�BjBjBiyBhsBgmBffBffBdZBA�B?}B=qB9XB8RB9XB8RB9XB;dB>wBB�BL�BXBXB\)Bp�Bu�By�B�+B�{B��B��B��B��B��B��B��B�B�B�B�-B�-B�^B��BǮBȴB��B��BƨB��B�qB�RB�'B�B��B��B��B��B�uB�oB�bB�B~�B~�B|�B{�Bz�Bz�Bz�Bz�By�Bx�Be`BcTB_;B]/BVB>wB5?B2-B�BVB
=BB��B�BB�B��B�Bp�BgmBVBK�B.B
=B
�mB
��B
ȴB
ĜB
�?B
��B
�=B
p�B
gmB
\)B
Q�B
D�B
2-B
B	�B	�5B	�9B	�B	��B	�B	��B	��B	�7B	�%B	�%B	�%B	�B	�B	z�B	l�B	aHB	M�B	>wB	5?B	2-B	,B	&�B	�B	B��B��B��B�B�B�B�NB�B�B��B��B��BÖB�FB�B��B��B��B��B��B�hB�=B�+B�%B�B�B�B�B�B}�Bz�B{�B}�Bu�Bt�Br�Bq�Bq�Bs�Bs�Bn�Bn�Bm�Bm�Bk�Bk�Bl�Bk�Bl�Bl�Bn�Bm�Bl�Bl�Bl�Bk�Bl�Bl�Bl�Bl�Bm�Bl�Bl�Bl�BjBjBjBiyBgmBhsBgmBgmBffBe`BdZBdZBcTBdZBcTBbNBbNB_;B^5B_;B`BBaHBaHBaHBbNBcTBdZBdZBe`Be`BffBiyBo�Bs�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bt�Bt�Bv�Bu�Bw�By�By�By�Bw�Bx�Bx�Bw�Bx�Bw�Bz�Bz�B{�B}�B�B�B�+B�7B�VB�bB�hB��B��B��B��B�B�B�-B�3B�9B�LB�XB�XB�qB��BĜBɺB��B��B��B��BɺBɺBɺBȴB��B�)B�)B�#B�/B�;B�BB�BB�;B�/B�5B�BB�NB�TB�fB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	  B��B��B��B	  B	B	B	B	%B	1B	
=B	PB	bB	hB	�B	�B	�B	"�B	%�B	(�B	0!B	49B	5?B	6FB	5?B	7LB	8RB	9XB	9XB	:^B	:^B	;dB	;dB	:^B	=qB	?}B	@�B	@�B	@�B	E�B	J�B	Q�B	XB	\)B	]/B	^5B	^5B	^5B	^5B	^5B	bNB	iyB	m�B	m�B	m�B	m�B	n�B	p�B	q�B	q�B	s�B	t�B	u�B	v�B	w�B	x�B	z�B	|�B	}�B	�B	�VB	�bB	�bB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�!B	�-B	�9B	�FB	�?B	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�RB	�^B	�jB	�qB	�wB	�wB	�}B	��B	��B	B	B	B	ĜB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
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
<D��<e`B<T��<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<e`B<�j<D��<D��<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447322012010314473220120103144732  AO  ARGQ                                                                        20111130142938  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142938  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144732  IP                  G�O�G�O�G�O�                