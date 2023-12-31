CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:43Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143329  20190522121827  1727_5046_187                   2C  D   APEX                            2143                            040306                          846 @�a�1   @�aβ@ @5���Q��c�     1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Df��Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(ffB0��B8��B@��BH��BP��BX��B`��Bi33Bq33Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33CL�C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33CbL�Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��DfD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2�fD3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW�fDX�DX��DYfDY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`fD`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��DffDf�fDgfDg��Dh�Dh��Di3Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm�3Dn�Dn��DofDo�fDp�Dp��Dq�Dq��Dr�Dr��Ds�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�A��;Aʏ\A�ZA�9XA�
=A��A��`A��/A��/A��HA��TA��`A��TA��TA��TA��`A��mA��`A��yA��A��yA��mA��A��A��A��A��mA���A�A�AȺ^A�G�Aǥ�A���A���A�=qA�S�A���A�1'A���A�?}A�1A�"�A�n�A��7A���A���A���A��`A�ffA��A��A���A�z�A��TA��A�(�A���A��A�l�A�;dA�-A�JA��A���A���A�r�A��TA��A�7LA��yA�r�A�+A���A��wA�$�A��A�A��A�E�A�E�A�ƨA�hsA��FA��A��+A���A�p�A��A��/A�^5A�I�A��
A�"�A�+A�I�A��A�x�A�JA�Q�A�p�A�O�A���A��A� �A�jA�G�A�A�A���A���A��^A�JA�M�A}�A{��Ay�wAv�uAtbAs��AsXAr(�ApȴAo�Ao�wAop�Ao7LAo
=An�yAn�RAm�
Al��Ak��Ak+Ak%Aj�DAidZAi�Ah�DAg�wAf�AdbNAb^5A\bAZ-AX-AS�PAO�FAM�AL9XAK�hAJQ�AH-AE�FAD��AD�DAD�AA��A<�A8bNA7�^A7"�A6��A6 �A5x�A4~�A3��A1�wA0�DA/��A.{A,�DA)��A(��A(E�A({A'A&�A&bNA%�A%C�A$~�A#��A#&�A"��A"�+A"M�A!�
A �`A�jA�A�mAC�A��A��A��Al�A��A$�AM�A�A1A%A+A	�mAr�AS�A�A�A+A1A&�A
=A�`A1'A�^AO�A ��A �@���@�\)@�?}@���@�~�@��j@�dZ@�S�@�l�@��@���@��@��D@�Q�@�A�@�b@�l�@��@�Q�@�"�@�~�@�=q@���@�-@��`@�9X@�@�M�@�$�@��/@�o@���@���@���@�9X@���@ާ�@ݡ�@�1@���@��@�p�@�%@ؓu@��@�S�@��@�o@�
=@��@��@Լj@�A�@�(�@��@�K�@���@�$�@���@ϥ�@���@�1@ˍP@�~�@�1'@�(�@��@�G�@��@���@ļj@�1'@�^5@��^@�?}@���@�1@��R@���@��/@���@�bN@�1@��@��F@��-@��`@�r�@� �@�C�@��T@�t�@���@���@��#@�hs@��@��/@��j@��@� �@��F@�t�@�+@��@�@��H@���@�-@��@�@�hs@�7L@�%@�1'@�t�@�V@��@��@��@��
@���@�+@��@�-@��#@���@�G�@��@���@��9@�r�@���@��F@�|�@��@�
=@��@�V@�J@��T@���@��^@��h@��@�O�@�%@��j@�A�@��F@�\)@�ȴ@�V@���@���@��#@��#@��T@��@��@��T@���@��#@���@��#@���@��^@��-@���@�hs@�O�@�?}@�%@��9@�z�@�j@�bN@�Z@��m@�S�@�@���@���@���@�~�@�n�@�^5@�-@�@��@���@�X@�V@���@��/@���@��j@��@���@���@��D@�r�@�A�@� �@��m@���@�l�@�K�@�+@���@�M�@�@���@�O�@� �@��@��@��w@��@�/@���@�A�@��
@�t�@�K�@�S�@��
@��@�j@��@��-@��T@���@���@���@�x�@�/@���@��D@���@���@�-@�@��@��T@�`B@���@��u@�A�@���@��F@���@�t�@�+@���@�n�@�V@��@�hs@�%@��/@�j@�1'@��
@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�A��;Aʏ\A�ZA�9XA�
=A��A��`A��/A��/A��HA��TA��`A��TA��TA��TA��`A��mA��`A��yA��A��yA��mA��A��A��A��A��mA���A�A�AȺ^A�G�Aǥ�A���A���A�=qA�S�A���A�1'A���A�?}A�1A�"�A�n�A��7A���A���A���A��`A�ffA��A��A���A�z�A��TA��A�(�A���A��A�l�A�;dA�-A�JA��A���A���A�r�A��TA��A�7LA��yA�r�A�+A���A��wA�$�A��A�A��A�E�A�E�A�ƨA�hsA��FA��A��+A���A�p�A��A��/A�^5A�I�A��
A�"�A�+A�I�A��A�x�A�JA�Q�A�p�A�O�A���A��A� �A�jA�G�A�A�A���A���A��^A�JA�M�A}�A{��Ay�wAv�uAtbAs��AsXAr(�ApȴAo�Ao�wAop�Ao7LAo
=An�yAn�RAm�
Al��Ak��Ak+Ak%Aj�DAidZAi�Ah�DAg�wAf�AdbNAb^5A\bAZ-AX-AS�PAO�FAM�AL9XAK�hAJQ�AH-AE�FAD��AD�DAD�AA��A<�A8bNA7�^A7"�A6��A6 �A5x�A4~�A3��A1�wA0�DA/��A.{A,�DA)��A(��A(E�A({A'A&�A&bNA%�A%C�A$~�A#��A#&�A"��A"�+A"M�A!�
A �`A�jA�A�mAC�A��A��A��Al�A��A$�AM�A�A1A%A+A	�mAr�AS�A�A�A+A1A&�A
=A�`A1'A�^AO�A ��A �@���@�\)@�?}@���@�~�@��j@�dZ@�S�@�l�@��@���@��@��D@�Q�@�A�@�b@�l�@��@�Q�@�"�@�~�@�=q@���@�-@��`@�9X@�@�M�@�$�@��/@�o@���@���@���@�9X@���@ާ�@ݡ�@�1@���@��@�p�@�%@ؓu@��@�S�@��@�o@�
=@��@��@Լj@�A�@�(�@��@�K�@���@�$�@���@ϥ�@���@�1@ˍP@�~�@�1'@�(�@��@�G�@��@���@ļj@�1'@�^5@��^@�?}@���@�1@��R@���@��/@���@�bN@�1@��@��F@��-@��`@�r�@� �@�C�@��T@�t�@���@���@��#@�hs@��@��/@��j@��@� �@��F@�t�@�+@��@�@��H@���@�-@��@�@�hs@�7L@�%@�1'@�t�@�V@��@��@��@��
@���@�+@��@�-@��#@���@�G�@��@���@��9@�r�@���@��F@�|�@��@�
=@��@�V@�J@��T@���@��^@��h@��@�O�@�%@��j@�A�@��F@�\)@�ȴ@�V@���@���@��#@��#@��T@��@��@��T@���@��#@���@��#@���@��^@��-@���@�hs@�O�@�?}@�%@��9@�z�@�j@�bN@�Z@��m@�S�@�@���@���@���@�~�@�n�@�^5@�-@�@��@���@�X@�V@���@��/@���@��j@��@���@���@��D@�r�@�A�@� �@��m@���@�l�@�K�@�+@���@�M�@�@���@�O�@� �@��@��@��w@��@�/@���@�A�@��
@�t�@�K�@�S�@��
@��@�j@��@��-@��T@���@���@���@�x�@�/@���@��D@���@���@�-@�@��@��T@�`B@���@��u@�A�@���@��F@���@�t�@�+@���@�n�@�V@��@�hs@�%@��/@�j@�1'@��
@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB9XB9XB:^B>wBD�BE�BE�BB�B?}B?}B@�BA�BA�BC�BC�BD�BE�BF�BG�BH�BH�BJ�BL�BM�BO�BR�BT�BW
BZB\)BaHBp�B�B�PB�{B��B�'B��B�BB�/B�TB�ZB�ZB�NB�HB�`B�B�B�mB�mB�TB�HB�BB�;B�;B�;B�#B�B��B�}B�FB�'B��B��B��B��B��B��B�oB�Bu�Bq�Bl�BhsBe`BcTB`BBXBH�B?}B33B&�B�BoBJBB��B�B�NB�/B�B��BǮB�B��B�JBz�Bn�Be`BaHBYBM�BB�B33B%�B�BPB
��B
�/B
�?B
��B
�oB
�%B
{�B
cTB
O�B
>wB
0!B
�B
+B
B
  B	��B	�B	�yB	�mB	�`B	�ZB	�TB	�NB	�BB	�B	��B	��B	��B	ɺB	ƨB	��B	�}B	�jB	�FB	�B	��B	�oB	q�B	aHB	N�B	6FB	'�B	#�B	 �B	�B	�B	oB	+B	%B	B	B��B�B�#B�B��B��B��B��BȴBŢB�wB�dB�FB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�DB�=B�1B�B�B�B�B~�B{�Bz�Bx�Bw�Bu�Bq�Bn�Bl�Bk�Bk�Bk�BiyBhsBiyBiyBhsBiyBiyBiyBiyBhsBgmBhsBhsBhsBhsBgmBgmBk�Bp�Bs�Br�Bq�Bq�Bq�Bs�Bt�Bt�Bu�Bx�B{�B{�B{�B|�B}�B~�B}�B|�B~�B�B�B�B�B�%B�JB�VB�PB�VB�bB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�LB�qB�qB�}BȴB��B��B�B�#B�#B�#B�#B�ZB�ZB�fB�mB�yB�B��B��B��B	  B	B	B	B	+B		7B	JB	VB	\B	PB		7B	B	B	B	B	B	B	B	B	+B	
=B	JB	\B	\B	bB	hB	uB	�B	�B	�B	�B	�B	#�B	(�B	+B	0!B	6FB	8RB	;dB	A�B	B�B	E�B	H�B	N�B	Q�B	R�B	T�B	XB	XB	YB	ZB	\)B	^5B	_;B	bNB	bNB	bNB	e`B	gmB	gmB	hsB	hsB	iyB	iyB	jB	jB	k�B	n�B	r�B	t�B	x�B	|�B	�B	�B	�1B	�1B	�7B	�7B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�\B	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�FB	�LB	�XB	�^B	�dB	�jB	�jB	�qB	�qB	�}B	��B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	ȴB	ǮB	ǮB	ƨB	ŢB	ÖB	��B	��B	B	ÖB	ĜB	ŢB	��B	��B	�B	�B	�/B	�HB	�TB	�ZB	�`B	�`B	�`B	�ZB	�TB	�HB	�5B	�/B	�5B	�;B	�;B	�5B	�/B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�TB	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B9XB9XB;dB?}BE�BF�BF�BB�B?}B?}B@�BA�BA�BC�BC�BD�BE�BF�BG�BH�BH�BJ�BL�BM�BO�BR�BT�BW
BZB\)BbNBq�B�B�\B��B��B�9B�B�mB�HB�`B�`B�`B�`B�mB�B�B�B�B�B�fB�TB�HB�BB�HB�NB�;B�)B�BɺB�dB�RB�B��B��B��B��B��B��B�1Bv�Br�Bn�BiyBffBdZBcTB_;BK�BB�B7LB,B�B{BbBB��B�B�TB�;B�B�
B��B�3B��B�oB� Br�BffBdZB^5BS�BI�B8RB+B �BoBVB
�B
�wB
��B
��B
�7B
�B
jB
T�B
B�B
8RB
�B
1B
%B
B	��B	�B	�B	�sB	�fB	�ZB	�TB	�TB	�TB	�/B	��B	��B	��B	��B	ɺB	��B	��B	�wB	�dB	�9B	��B	��B	v�B	gmB	[#B	@�B	/B	%�B	"�B	 �B	�B	�B		7B	+B	B	%B	B��B�/B�B��B��B��B��B��B��B��B�wB�^B�LB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�JB�DB�DB�7B�B�B�B�B~�B|�B{�B|�B}�Bs�Bp�Bo�Bm�Bl�Bl�BjBiyBjBjBjBjBk�Bk�BjBl�Bk�BjBjBk�BiyBgmBk�Br�Bu�Br�Bq�Bq�Bq�Bt�Bu�Bw�Bx�Bz�B|�B{�B{�B|�B~�B� B~�B~�B~�B�%B�B�B�B�+B�PB�\B�\B�bB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�LB�^B�wB�}BÖBȴB��B��B�B�#B�#B�)B�5B�`B�`B�mB�sB�B�B��B��B��B	B	B	B	B	1B	
=B	JB	\B	bB	bB	DB		7B	B	B	B	B	B	B	%B	1B	
=B	JB	\B	\B	bB	oB	{B	�B	�B	�B	�B	�B	$�B	)�B	-B	1'B	7LB	9XB	<jB	A�B	C�B	F�B	I�B	O�B	Q�B	S�B	VB	XB	XB	ZB	[#B	\)B	^5B	`BB	bNB	bNB	cTB	ffB	gmB	gmB	hsB	hsB	iyB	iyB	k�B	k�B	l�B	o�B	s�B	u�B	y�B	}�B	�B	�B	�1B	�1B	�7B	�7B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�\B	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�FB	�LB	�XB	�^B	�dB	�jB	�jB	�qB	�qB	�}B	��B	��B	ĜB	ƨB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	ǮB	ǮB	ƨB	ƨB	ƨB	B	B	ÖB	ĜB	ĜB	ŢB	ɺB	��B	��B	�
B	�)B	�HB	�TB	�ZB	�`B	�fB	�`B	�ZB	�ZB	�HB	�5B	�/B	�5B	�;B	�;B	�5B	�5B	�#B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�BB	�;B	�;B	�;B	�HB	�HB	�NB	�TB	�ZB	�TB	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447392012010314473920120103144739  AO  ARGQ                                                                        20111130143329  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143329  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144740  IP                  G�O�G�O�G�O�                