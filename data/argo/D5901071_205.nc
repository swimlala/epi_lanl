CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:48Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143452  20190522121828  1727_5046_205                   2C  D   APEX                            2143                            040306                          846 @���io�1   @��{B`@7U?|�h�c��`A�71   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� DfD� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D   D � D!  D!� D"fD"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @L��@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BPffBX��B`��BhffBp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33CL�C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CVL�CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��D fD �fD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D3D��D3D��D�D��D�D��D�D��D�D��D�D��DfD�fD�D��D�D��D�D��D �D ��D!�D!��D"3D"�3D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.fD.��D/�D/��D0fD0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7�3D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DXfDX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db�3Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�3Dr�Dr��Ds�Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��^A��A���A���A�ZA�5?A�A��yA���A��+A�hsA�ZA�O�A�G�A�?}A�?}A�;dA�/A��A�  A��mA��`A��yA��HA���A���A���A���A���A���A��^A��!A��A��A�n�A�jA�jA�hsA�bNA�S�A�G�A�A�A�?}A�;dA�+A�(�A�$�A��A��A�oA�A��A��jA���A�p�A�Q�A�(�A��mA��wA�r�A�;dA�7LA�5?A�$�A��yA�x�A���A�n�A���A��A��
A��+A��A��A��RA��uA���A��mA�7LA�A�A�ĜA�oA�ĜA�JA��!A���A��/A��uA�  A�G�A�;dA�S�A��+A��A���A��#A���A���A�5?A�XA��`A��\A�ȴA�ZA��jA���A�z�A�  A�I�A�1A�A}��A|~�A|-A{��Ay?}Aw�7AvbNAu�Au��At�RAqƨAj��AiAf�/Ac�AbI�Aa�PAaK�Aa+A`�HA`9XA^��A]ƨA]�A\��AZE�AX��AW��AW�-AV��AUt�ATAS�AR=qAPVAN�uALffAK`BAJ�uAI;dAH��AG�AF�/AF~�AF1'AE�FAEO�AD-AB�9A@v�A?C�A=�A=�FA<�yA<M�A;��A:��A:{A8�HA7�#A7x�A6�DA4A1�7A/�A-t�A-A,��A,ffA+�A+XA*-A)x�A(�A'�mA'�A&�RA%��A%�A$�RA$VA#��A#
=A"bNA!"�A�FA�+Al�AM�A�A�`A  A��A�A�^A|�A��A��A��AA�A�AĜA-A;dA��AjA��A;dA�/A-AA7LA��A�+A �AO�A
r�A	t�A�A�`AXA�
AVA�\A��A �@��^@��w@���@�I�@��;@�5?@�z�@�\)@���@�@�$�@��@���@���@�/@��H@�p�@�j@畁@��@�;d@�ȴ@���@���@�Z@���@�;d@�n�@�V@�l�@�ff@�7L@�C�@��@�1'@�33@�^5@�-@�G�@�Q�@���@���@�$�@ͺ^@�p�@�?}@���@�C�@ɑh@��/@���@ř�@�1@�K�@�33@�o@°!@�@�v�@��@�{@�p�@��
@�`B@���@�S�@��\@���@�O�@��`@��@��
@�S�@�@�@��j@�bN@�b@�|�@�+@�o@�
=@��T@��@�j@�K�@�\)@��@�^5@���@�Ĝ@���@�
=@���@��R@��T@��7@��7@��@�M�@�Ĝ@�+@��!@�^5@�^5@��\@��7@�@��T@�O�@��T@��T@���@�x�@��@��j@���@�z�@� �@��
@���@��@�S�@�o@��@��R@��!@��R@���@��@��!@��@�p�@���@�z�@� �@��@��;@���@��m@���@���@���@�ȴ@���@��+@�{@��@��-@�O�@�Ĝ@���@�I�@��;@��@�;d@�o@�@���@��@���@�M�@���@�@�`B@�V@��9@��u@�j@�b@��w@�K�@�
=@��H@��!@�ff@�5?@��#@�@��7@�X@��@���@�Z@�I�@� �@�ƨ@���@�S�@��y@���@�v�@���@��#@��^@���@���@�@���@�x�@�O�@�?}@��@��D@�b@���@��P@�K�@�;d@�33@�+@��@��@�=q@��h@�`B@��@�Ĝ@�z�@�A�@�1'@� �@�1@���@��
@���@�l�@�+@�"�@��@�o@�
=@��@���@���@�^5@�@�x�@�`B@�O�@�?}@�/@��@��@��
@�S�@�
=@���@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��^A��A���A���A�ZA�5?A�A��yA���A��+A�hsA�ZA�O�A�G�A�?}A�?}A�;dA�/A��A�  A��mA��`A��yA��HA���A���A���A���A���A���A��^A��!A��A��A�n�A�jA�jA�hsA�bNA�S�A�G�A�A�A�?}A�;dA�+A�(�A�$�A��A��A�oA�A��A��jA���A�p�A�Q�A�(�A��mA��wA�r�A�;dA�7LA�5?A�$�A��yA�x�A���A�n�A���A��A��
A��+A��A��A��RA��uA���A��mA�7LA�A�A�ĜA�oA�ĜA�JA��!A���A��/A��uA�  A�G�A�;dA�S�A��+A��A���A��#A���A���A�5?A�XA��`A��\A�ȴA�ZA��jA���A�z�A�  A�I�A�1A�A}��A|~�A|-A{��Ay?}Aw�7AvbNAu�Au��At�RAqƨAj��AiAf�/Ac�AbI�Aa�PAaK�Aa+A`�HA`9XA^��A]ƨA]�A\��AZE�AX��AW��AW�-AV��AUt�ATAS�AR=qAPVAN�uALffAK`BAJ�uAI;dAH��AG�AF�/AF~�AF1'AE�FAEO�AD-AB�9A@v�A?C�A=�A=�FA<�yA<M�A;��A:��A:{A8�HA7�#A7x�A6�DA4A1�7A/�A-t�A-A,��A,ffA+�A+XA*-A)x�A(�A'�mA'�A&�RA%��A%�A$�RA$VA#��A#
=A"bNA!"�A�FA�+Al�AM�A�A�`A  A��A�A�^A|�A��A��A��AA�A�AĜA-A;dA��AjA��A;dA�/A-AA7LA��A�+A �AO�A
r�A	t�A�A�`AXA�
AVA�\A��A �@��^@��w@���@�I�@��;@�5?@�z�@�\)@���@�@�$�@��@���@���@�/@��H@�p�@�j@畁@��@�;d@�ȴ@���@���@�Z@���@�;d@�n�@�V@�l�@�ff@�7L@�C�@��@�1'@�33@�^5@�-@�G�@�Q�@���@���@�$�@ͺ^@�p�@�?}@���@�C�@ɑh@��/@���@ř�@�1@�K�@�33@�o@°!@�@�v�@��@�{@�p�@��
@�`B@���@�S�@��\@���@�O�@��`@��@��
@�S�@�@�@��j@�bN@�b@�|�@�+@�o@�
=@��T@��@�j@�K�@�\)@��@�^5@���@�Ĝ@���@�
=@���@��R@��T@��7@��7@��@�M�@�Ĝ@�+@��!@�^5@�^5@��\@��7@�@��T@�O�@��T@��T@���@�x�@��@��j@���@�z�@� �@��
@���@��@�S�@�o@��@��R@��!@��R@���@��@��!@��@�p�@���@�z�@� �@��@��;@���@��m@���@���@���@�ȴ@���@��+@�{@��@��-@�O�@�Ĝ@���@�I�@��;@��@�;d@�o@�@���@��@���@�M�@���@�@�`B@�V@��9@��u@�j@�b@��w@�K�@�
=@��H@��!@�ff@�5?@��#@�@��7@�X@��@���@�Z@�I�@� �@�ƨ@���@�S�@��y@���@�v�@���@��#@��^@���@���@�@���@�x�@�O�@�?}@��@��D@�b@���@��P@�K�@�;d@�33@�+@��@��@�=q@��h@�`B@��@�Ĝ@�z�@�A�@�1'@� �@�1@���@��
@���@�l�@�+@�"�@��@�o@�
=@��@���@���@�^5@�@�x�@�`B@�O�@�?}@�/@��@��@��
@�S�@�
=@���@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB!�B/BC�BN�BP�BQ�BR�BR�BR�BS�BS�BS�BS�BS�BR�BR�BR�BR�BQ�BO�BN�BK�BK�BL�BK�BK�BJ�BJ�BJ�BJ�BJ�BK�BK�BJ�BI�BH�BH�BH�BI�BI�BG�BG�BF�BF�BF�BF�BF�BF�BF�BF�BE�BE�BD�BD�BD�BD�BF�BF�BD�BC�BA�BB�BB�BB�BB�B@�B9XB0!B&�B$�B0!B2-B,B�B  B�ZBɺB��B��B�+Bt�BW
B8RBVBB��B�B�;B�B��B�?B��B�bB�Bo�B`BBG�BB�B33B�BVB	7B  B
��B
�)B
ǮB
��B
��B
�bB
�%B
s�B
\)B
N�B
H�B
E�B
A�B
2-B
(�B
 �B
�B
�B
hB	��B	��B	ȴB	�XB	��B	��B	��B	��B	��B	��B	�{B	�DB	�+B	�B	~�B	s�B	hsB	dZB	hsB	k�B	gmB	dZB	`BB	YB	N�B	@�B	8RB	33B	,B	#�B	�B	�B	�B	�B	oB	bB	PB	1B	B��B��B�B�B�B�B�mB�TB�BB�#B�B��B��BB�RB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�PB�=B�%B�B� B}�B|�B{�Bz�By�By�By�Bx�Bw�Bu�Bu�Bt�Bt�Bs�Bs�Br�Bs�Bs�Bs�Bv�Bt�Bs�Bs�Br�Br�Br�Bq�Bp�Bo�Bl�Bk�BjBgmBhsBhsBhsBe`Be`BaHBbNBaHBffBffBffBgmBiyBiyBjBjBjBjBjBiyBdZBZBW
BVBVBW
BXBXBYBZBZBZB[#B[#B]/B`BBbNBdZBffBo�Bo�Bo�Bp�Bp�Br�Bs�Bs�Bu�Bz�B|�B~�B� B�B�B�%B�+B�7B�DB�VB�uB��B��B��B��B��B��B��B��B��B�LB�^B�RB�XB�XB�qB�wB��BƨBɺBɺB��B�B�5B�/B�/B�)B�/B�5B�5B�HB�NB�NB�ZB�B�B�B��B	  B	+B	+B		7B	PB	\B	oB	hB	DB	%B	B	B		7B	JB	bB	oB	�B	�B	�B	#�B	&�B	(�B	)�B	,B	.B	/B	/B	1'B	33B	33B	33B	5?B	7LB	9XB	<jB	<jB	=qB	=qB	@�B	B�B	C�B	D�B	F�B	G�B	I�B	K�B	L�B	M�B	M�B	M�B	Q�B	S�B	S�B	T�B	T�B	YB	\)B	]/B	_;B	`BB	`BB	bNB	cTB	e`B	gmB	hsB	iyB	iyB	iyB	iyB	l�B	m�B	o�B	q�B	s�B	u�B	v�B	w�B	x�B	z�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�PB	�PB	�VB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�9B	�9B	�RB	�^B	�^B	�^B	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�}B	�}B	��B	��B	B	ĜB	ŢB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B1'BD�BO�BQ�BR�BS�BR�BS�BT�BT�BS�BS�BS�BR�BR�BR�BR�BQ�BO�BN�BK�BK�BL�BK�BK�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BI�BH�BH�BH�BI�BI�BG�BG�BF�BF�BF�BF�BF�BF�BF�BF�BE�BF�BE�BE�BE�BE�BG�BH�BE�BE�BB�BB�BB�BB�BD�BC�B<jB49B)�B#�B2-B49B.B$�BB�yB��B��B��B�DB{�B_;BC�BhBB  B�B�HB�)B�
B�}B��B��B�7Bw�Bk�BI�BG�B=qB �BhBDBB
��B
�TB
��B
��B
��B
�{B
�JB
}�B
bNB
Q�B
I�B
F�B
H�B
7LB
,B
!�B
�B
�B
�B
DB	�B	��B	B	�B	��B	��B	��B	��B	��B	��B	�VB	�1B	�+B	�%B	x�B	jB	e`B	k�B	p�B	l�B	ffB	dZB	_;B	T�B	G�B	;dB	6FB	1'B	%�B	"�B	�B	�B	�B	{B	oB	hB	VB	
=B	  B��B��B�B�B�B�yB�`B�TB�5B�B�B��BȴB�}B�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�bB�VB�DB�1B�B�B� B}�B}�B~�B|�Bz�By�Bz�Bx�Bw�Bv�Bv�Bu�Bu�Bt�Bt�Bt�Bu�Bw�Bu�Bu�Bt�Bt�Bt�Br�Br�Br�Br�Bo�Bo�Bn�Bl�Bm�Bk�BjBhsBiyBffBffBffBgmBgmBiyBjBk�BjBk�Bk�Bk�Bl�Bl�Bn�BhsB]/BYBXBVBZBYBZB[#B[#B[#B\)B]/B^5B`BBbNBdZBgmBiyBp�Bp�Bp�Bp�Bq�Bs�Bt�Bt�Bv�B{�B}�B~�B�B�B�B�+B�=B�DB�VB�\B�uB��B��B��B��B��B��B��B��B��B�XB�qB�XB�dB�^B�wB��BBǮB��BɺB��B�#B�;B�5B�5B�)B�/B�BB�BB�NB�ZB�NB�ZB�B�B�B��B	B	+B	+B	
=B	PB	\B	uB	{B	PB	1B	B	B		7B	JB	hB	hB	�B	�B	�B	#�B	&�B	(�B	)�B	-B	.B	/B	0!B	2-B	33B	33B	49B	5?B	7LB	9XB	<jB	<jB	=qB	=qB	A�B	C�B	D�B	E�B	G�B	H�B	I�B	K�B	L�B	M�B	N�B	N�B	Q�B	S�B	S�B	VB	VB	YB	]/B	^5B	`BB	`BB	aHB	cTB	dZB	ffB	gmB	hsB	iyB	iyB	iyB	jB	m�B	m�B	p�B	r�B	t�B	u�B	v�B	x�B	y�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�JB	�PB	�PB	�VB	�hB	�hB	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�9B	�?B	�XB	�^B	�dB	�dB	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	�wB	�wB	�wB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ŢB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447462012010314474620120103144746  AO  ARGQ                                                                        20111130143452  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143452  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144746  IP                  G�O�G�O�G�O�                