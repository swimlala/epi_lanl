CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:40Z UW 3.1 conversion   
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143125  20190522121827  1727_5046_176                   2C  D   APEX                            2143                            040306                          846 @�� �1 1   @��!� @5 A�7K��dl�C��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn�fDofDo�fDp  Dpy�Dp��Dqy�Dr  Dr� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB���B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffBș�B̙�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��DfD��D�D��D�D�fDfD�fD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�fD�D�3D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D93D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DWfDW��DX�DX��DY�DY��DZ�DZ��D[�D[�3D\�D\�3D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd�3De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl�3Dm�Dm��Dn�Dn�3Do3Do�3Dp�Dp�fDqfDq�fDr�Dr��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԸRAԸRAԸRAԺ^AԺ^A���A���A���A�A���A�A�A�A�A�A���A�ĜA�ȴA�ȴA�ȴA�ȴA�ĜAԾwA�AԸRAԴ9A԰!Aԕ�A�p�AЩ�Ả7A���A�M�A�bA��A�oA��#A�AŬA��
A��/A���A�VA��/A�x�A��A�Q�A�/A�-A�{A���A�S�A���A���A�ȴA���A��uA�x�A�z�A�1A�\)A���A�p�A�M�A��
A��wA�ffA���A��mA���A���A��jA�7LA�M�A�oA��wA��-A�VA�n�A��A��9A�ĜA��mA�r�A�oA��-A�?}A�VA�oA�1A�bA�ƨA���A�VA��`A�JA�  A�=qA�ĜA��TA���A�&�A�1'A�C�A���A�A+A~�A|�yA|n�A{�Az��Axz�AuVAr��Ap��Am|�Ai�hAfffAdI�Aa��A`bA_K�A^�AZ��AYXAXbNAW33AU��AS/APZAN��AK�AJQ�AH��AI��AIƨAIK�AHz�AH�AG��AF��AF$�AE\)AD�\AC�7AB-A?��A=��A=G�A<�+A;t�A:=qA9G�A8n�A5�
A2jA1�;A0VA.1A,$�A*�A(�!A'"�A&{A%+A$��A$n�A#A"��A"JA!�-A ffAp�A��A�AhsA�A�TA"�AVA��A�HAffAoAl�A(�A��A|�A�A��A��A&�AĜAt�A�A~�A
~�A	��Az�A��A;dA�+A��Ax�AQ�A�hA ��@�;d@��^@�Ĝ@���@�ȴ@��@��@�=q@�A�@���@�P@�33@�@�@�h@��D@��m@@�$�@�u@��@��@�X@�r�@�+@��@�Ĝ@��m@�
=@�I�@��H@݁@�?}@ܴ9@�I�@���@ۍP@ف@�b@�M�@Ձ@��/@�1@�dZ@��#@�dZ@�{@�O�@�%@���@̣�@�A�@��
@�\)@�o@ʸR@�J@ə�@�G�@���@ȴ9@�1'@��m@�"�@Ƈ+@�$�@��@���@���@�J@��u@�1'@�9X@��@��#@�%@�bN@��
@�dZ@�ȴ@�5?@��7@���@�9X@�;d@�^5@�@���@�O�@���@��D@� �@��m@���@��@�S�@�+@�o@��@���@�^5@�=q@��@��@��9@��@�/@�/@��@�V@���@�z�@���@��@��`@��u@�bN@�9X@� �@�1@���@���@�@�&�@�Z@�1'@�9X@�A�@� �@���@���@��\@�E�@�@�@���@�7L@��@��@�(�@�b@�b@��
@�\)@�o@��@��\@�~�@�ff@�$�@��T@��T@��#@���@���@�`B@�/@��@��`@��`@���@��u@�Q�@���@���@�ff@���@�@��7@�&�@�Z@�(�@�  @�|�@��y@�ȴ@���@���@�M�@�X@���@�Ĝ@��@��
@���@�K�@�K�@��@�@��@���@��@�7L@�%@�%@��@��@���@��@�Ĝ@���@�  @��@�K�@�
=@��y@��R@�=q@���@���@�X@�&�@�%@�Ĝ@��9@��u@�z�@�j@�1'@��
@��P@�t�@��@���@���@�dZ@�o@���@�ȴ@���@�@��@�@��h@�O�@�V@���@��`@��/@�r�@� �@�1@�1'@�1@��
@���@�|�@�dZ@�33@�
=@��H@�ȴ@��!@�n�@�-@��@���@��@�dZ@�E�@���@�p�@��7@��@���@��@���@��D@�bN@�Z@�z�@���@��u@��`@�X@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AԸRAԸRAԸRAԺ^AԺ^A���A���A���A�A���A�A�A�A�A�A���A�ĜA�ȴA�ȴA�ȴA�ȴA�ĜAԾwA�AԸRAԴ9A԰!Aԕ�A�p�AЩ�Ả7A���A�M�A�bA��A�oA��#A�AŬA��
A��/A���A�VA��/A�x�A��A�Q�A�/A�-A�{A���A�S�A���A���A�ȴA���A��uA�x�A�z�A�1A�\)A���A�p�A�M�A��
A��wA�ffA���A��mA���A���A��jA�7LA�M�A�oA��wA��-A�VA�n�A��A��9A�ĜA��mA�r�A�oA��-A�?}A�VA�oA�1A�bA�ƨA���A�VA��`A�JA�  A�=qA�ĜA��TA���A�&�A�1'A�C�A���A�A+A~�A|�yA|n�A{�Az��Axz�AuVAr��Ap��Am|�Ai�hAfffAdI�Aa��A`bA_K�A^�AZ��AYXAXbNAW33AU��AS/APZAN��AK�AJQ�AH��AI��AIƨAIK�AHz�AH�AG��AF��AF$�AE\)AD�\AC�7AB-A?��A=��A=G�A<�+A;t�A:=qA9G�A8n�A5�
A2jA1�;A0VA.1A,$�A*�A(�!A'"�A&{A%+A$��A$n�A#A"��A"JA!�-A ffAp�A��A�AhsA�A�TA"�AVA��A�HAffAoAl�A(�A��A|�A�A��A��A&�AĜAt�A�A~�A
~�A	��Az�A��A;dA�+A��Ax�AQ�A�hA ��@�;d@��^@�Ĝ@���@�ȴ@��@��@�=q@�A�@���@�P@�33@�@�@�h@��D@��m@@�$�@�u@��@��@�X@�r�@�+@��@�Ĝ@��m@�
=@�I�@��H@݁@�?}@ܴ9@�I�@���@ۍP@ف@�b@�M�@Ձ@��/@�1@�dZ@��#@�dZ@�{@�O�@�%@���@̣�@�A�@��
@�\)@�o@ʸR@�J@ə�@�G�@���@ȴ9@�1'@��m@�"�@Ƈ+@�$�@��@���@���@�J@��u@�1'@�9X@��@��#@�%@�bN@��
@�dZ@�ȴ@�5?@��7@���@�9X@�;d@�^5@�@���@�O�@���@��D@� �@��m@���@��@�S�@�+@�o@��@���@�^5@�=q@��@��@��9@��@�/@�/@��@�V@���@�z�@���@��@��`@��u@�bN@�9X@� �@�1@���@���@�@�&�@�Z@�1'@�9X@�A�@� �@���@���@��\@�E�@�@�@���@�7L@��@��@�(�@�b@�b@��
@�\)@�o@��@��\@�~�@�ff@�$�@��T@��T@��#@���@���@�`B@�/@��@��`@��`@���@��u@�Q�@���@���@�ff@���@�@��7@�&�@�Z@�(�@�  @�|�@��y@�ȴ@���@���@�M�@�X@���@�Ĝ@��@��
@���@�K�@�K�@��@�@��@���@��@�7L@�%@�%@��@��@���@��@�Ĝ@���@�  @��@�K�@�
=@��y@��R@�=q@���@���@�X@�&�@�%@�Ĝ@��9@��u@�z�@�j@�1'@��
@��P@�t�@��@���@���@�dZ@�o@���@�ȴ@���@�@��@�@��h@�O�@�V@���@��`@��/@�r�@� �@�1@�1'@�1@��
@���@�|�@�dZ@�33@�
=@��H@�ȴ@��!@�n�@�-@��@���@��@�dZ@�E�@���@�p�@��7@��@���@��@���@��D@�bN@�Z@�z�@���@��u@��`@�X@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBXBXBXBXBYBYBYBYBYBYBYBYBYBYBYBYBYBYBYBYBZB[#B[#B^5B_;B_;Bo�B� Br�B�7B�VB�VB�PB�DB�+B�B�Bx�Bt�Bu�By�Br�Bl�By�B�%B�JB�VB�\B�\B�bB�1Bl�BW
B9XB�BB�sB��B��B��B��B�B%B�B$�B-B�BJB�B/BE�BT�BR�BR�BK�B<jB&�B�BuB��B�yB�/B��BƨB��B�XB��B�bB�Bs�Bl�B\)BK�BE�B7LB$�BJB
�B
�B
�3B
��B
�hB
�B
_;B
Q�B
L�B
D�B
<jB
8RB
49B
)�B
�B
  B	�B	�BB	ĜB	�B	��B	�=B	y�B	s�B	o�B	ffB	XB	K�B	D�B	9XB	-B	�B	DB	B��B�B��B	\B	�B	�B	�B	�B	�B	{B	bB	DB	%B��B��B�mB�5B�/B�#B�B��B��BȴB�wB�FB�3B�!B�B��B��B��B��B��B��B��B��B�uB�hB�bB�VB�DB�DB�=B�+B�%B�B�B�B�B�B� B{�B~�Bx�Bx�Bx�Bx�Bw�Bv�Bv�Bu�Bs�Bq�Bn�Bk�Bk�Bk�Bk�Bk�BiyBgmBdZBbNBdZBdZBbNB`BB_;B\)B[#BZB\)B]/B\)B]/B^5B^5B^5B^5B^5B^5B_;B_;BaHBbNBgmBjBm�Bq�Bw�B|�B}�B�B�B�B�B�B�%B�VB�\B�VB�VB�JB�B�B�B�B�+B�1B�1B�=B�\B�JB�DB�DB�JB�JB�VB�hB��B��B��B��B��B��B�B�!B�FB�XB�dB�qB�}B�}B��B��BǮB��B��B��B�B�/B�/B�TB�ZB�ZB�ZB�yB�yB�yB�B�B��B��B��B��B��B��B	B	DB	\B	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	.B	/B	0!B	0!B	0!B	1'B	33B	6FB	<jB	=qB	=qB	?}B	@�B	@�B	B�B	E�B	M�B	O�B	S�B	VB	W
B	XB	XB	ZB	[#B	[#B	[#B	[#B	\)B	]/B	^5B	bNB	dZB	gmB	gmB	gmB	hsB	k�B	n�B	t�B	w�B	x�B	y�B	}�B	� B	� B	� B	� B	� B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�7B	�7B	�DB	�PB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�B	�B	�'B	�3B	�?B	�FB	�FB	�RB	�dB	�dB	�dB	�qB	�wB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�)B	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�)B	�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BXBXBXBXBXBYBYBYBYBYBYBYBYBYBYBYBYBYBYBYBYBZB[#B[#B^5B_;B`BBt�B�PB�+B�oB�hB�\B�VB�\B�JB�1B�1B{�Bx�B�B|�Bt�Bv�B}�B�1B�PB�VB�\B�\B�{B�JBp�BZB=qB�BB�B�#B��B��B�B�B1B�B&�B1'B�BPB$�B0!BH�B[#BZBZBQ�BH�B)�B"�B�B��B�B�ZB��BȴBÖB�wB�3B��B�%Bu�Br�BcTBM�BI�B=qB/B{B
��B
�B
�dB
��B
��B
�uB
dZB
S�B
N�B
G�B
=qB
9XB
7LB
/B
�B
B	��B	�mB	��B	�3B	��B	�bB	~�B	u�B	s�B	o�B	\)B	N�B	H�B	>wB	5?B	$�B	hB	JB��B��B��B	\B	!�B	!�B	�B	�B	�B	�B	uB	VB	
=B	B��B�B�BB�;B�5B�#B��B��B��BƨB�RB�LB�FB�-B�B��B��B��B��B��B��B��B��B�uB�hB�oB�VB�PB�JB�=B�1B�1B�B�B�B�B�B�B�B}�B}�B|�Bz�Bx�By�Bx�Bw�Bw�Bu�Bu�Br�Bn�Bp�Bn�Bm�Bl�Bm�BiyBffBgmBgmBe`BbNB`BB^5B\)B^5B]/B^5B_;B^5B^5B_;B^5B_;B`BB`BB`BBaHBbNBe`BjBl�Bn�Bs�By�B~�B� B�B�+B�%B�B�B�%B�\B�bB�VB�\B�\B�+B�B�%B�%B�1B�7B�=B�VB�hB�PB�DB�DB�JB�PB�\B�oB��B��B��B��B��B��B�B�'B�LB�^B�jB�wB�}B��BÖBĜBɺB��B��B��B�)B�5B�5B�ZB�ZB�ZB�ZB�B�B�B�B��B��B��B��B��B��B	  B	B	DB	\B	hB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	&�B	.B	/B	0!B	0!B	1'B	2-B	5?B	7LB	<jB	=qB	=qB	?}B	@�B	A�B	C�B	G�B	N�B	P�B	S�B	VB	W
B	XB	YB	[#B	\)B	[#B	[#B	[#B	\)B	^5B	_;B	bNB	e`B	gmB	gmB	gmB	iyB	l�B	n�B	u�B	w�B	x�B	y�B	}�B	� B	� B	� B	� B	� B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�DB	�JB	�PB	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�!B	�'B	�3B	�?B	�FB	�FB	�RB	�dB	�dB	�jB	�wB	�}B	�wB	�}B	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ŢB	ŢB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�/B	�/B	�5B	�)B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�#B	�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<��
<#�
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
<49X<���<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447362012010314473620120103144736  AO  ARGQ                                                                        20111130143125  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143125  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144736  IP                  G�O�G�O�G�O�                