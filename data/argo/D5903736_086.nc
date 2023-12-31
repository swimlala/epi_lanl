CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-08-21T04:20:08Z AOML 3.0 creation; 2016-05-31T19:14:38Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140821042008  20160531121439  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               VA   AO  4051_7090_086                   2C  D   APEX                            5368                            041511                          846 @��{B?�1   @����@3CS����d��"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    VA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BQ��BV  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��3D�C3D���D��3D�	�D�L�D�i�D��3D�fD�FfD���D��3D�3D�6fD�i�D���D� D�I�D�s3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BQfgBU��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��B��fB��3B��fB��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DI3DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dtp Dy�gD��D�A�D�� D�љD� D�K3D�h D���D��D�D�D��3D�љD��D�4�D�h D��3D�fD�H D�q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��\A��A�v�A�t�A�n�A�n�A�n�A�l�A�l�A�p�A�z�A��A���A�-A�^A�jA�^A�jA�RA�FA�^A��mA�^5A�ƨA�VA��#A�33A��#A�-A؅A��HA�^5A�ffA�ZA�x�A�VA̝�A��`A�&�A��A�;dAğ�A�p�A��+A�A�A���A���A��DA��A��A�E�A���A���A��A�z�A��A��A�z�A�z�A�  A�E�A��;A��wA�1'A���A�JA��A�S�A�A��/A���A��A��HA���A���A��A�/A�{A��jA�C�A���A�9XA�ZA��
A�jA� �A���A��HA���A�A�A�G�A���A��mA�5?A�5?A�A�?}A��;A�A�x�A��TA�l�A���A���A��
A���A�O�A���A��-A��A�A��A��A�A��wA�VA���A|�Av�HAt1'Arr�ApbNAm
=Ai�Af^5Ae�Ae33Ac�Ab�!Ab�Aa�
A`�HA^�/A^A]A]%A[G�AY�AV��AT�DAS|�APjAN�HANv�AN �AM\)AL�\AKG�AIhsAG7LAE&�AB�AAA?7LA=O�A;�A:(�A8~�A7�7A7�A6n�A5O�A3�FA2ȴA1dZA.��A.JA-S�A,Q�A*�yA)�A(jA'G�A&~�A%K�A$1'A#�A"��A"(�A!p�A�!AbA��A�RA  A�Ar�A��AO�A�A�A\)Ap�A-A�;A�AG�A��A�AĜA�TA�A�A
�yA	x�A�uA�Ap�A�DA��A�!A�AO�A �A�FA ȴ@�dZ@�x�@�A�@�+@��-@�x�@��@���@� �@��
@�dZ@�~�@�bN@�+@���@�o@�v�@�1@�R@��@�  @�\@�+@�~�@�!@���@�R@�~�@��@��m@�ȴ@�$�@ܣ�@ە�@ۅ@�{@ؓu@؛�@�1'@�1'@� �@�(�@��@և+@�{@Ցh@�/@ա�@�Ĝ@ӶF@�@�(�@�?}@ɑh@�Z@�n�@��@�7L@ă@�C�@ÍP@�I�@Ĭ@���@��@��@��`@���@��@� �@��T@�9X@���@��@�I�@�ƨ@�t�@��@�V@�@�&�@���@�G�@���@���@��@�/@�x�@�;d@��@���@��T@��@�$�@�@�%@�A�@��/@��@��@�V@���@�o@�V@�V@�n�@�Q�@���@��@���@��@��9@���@���@��D@�z�@���@���@�&�@��j@���@���@�Ĝ@�V@�/@���@�x�@��D@�ƨ@�
=@�ȴ@�E�@��T@��-@���@�$�@��
@�1'@���@�\)@�+@�;d@���@���@��j@�v�@�%@��+@�?}@���@�  @��@��@���@��@�K�@�S�@�dZ@��@���@���@���@�S�@�
=@��T@��@��@��j@�z�@��9@��`@��9@�A�@��@���@�l�@�K�@�@�;d@�;d@��H@�n�@�=q@�{@���@��T@��@�`B@�`B@�`B@�`B@�X@�O�@��@��9@��u@��D@�z�@�Q�@�A�@� �@��m@�ƨ@���@�t�@�S�@�33@��@�n�@�x�@��@��7@��h@��h@��7@�?}@���@�A�@��
@�ƨ@��w@��@���@�"�@��\@�n�@�E�@�/@��/@��@�z�@�A�@�  @��;@��F@��@��@���@��P@�K�@�+@��@���@�ff@�M�@��^@��u@��@��;@��@���@�@�X@���@��`@��/@��u@�1@���@�dZ@�+@��@��@��!@��+@��+@�@���@�(�@���@��P@���@�Z@xbN@o\)@i%@aG�@Zn�@R��@Ix�@B��@:~�@3dZ@.V@)&�@#��@��@@5?@�@��@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��\A��A�v�A�t�A�n�A�n�A�n�A�l�A�l�A�p�A�z�A��A���A�-A�^A�jA�^A�jA�RA�FA�^A��mA�^5A�ƨA�VA��#A�33A��#A�-A؅A��HA�^5A�ffA�ZA�x�A�VA̝�A��`A�&�A��A�;dAğ�A�p�A��+A�A�A���A���A��DA��A��A�E�A���A���A��A�z�A��A��A�z�A�z�A�  A�E�A��;A��wA�1'A���A�JA��A�S�A�A��/A���A��A��HA���A���A��A�/A�{A��jA�C�A���A�9XA�ZA��
A�jA� �A���A��HA���A�A�A�G�A���A��mA�5?A�5?A�A�?}A��;A�A�x�A��TA�l�A���A���A��
A���A�O�A���A��-A��A�A��A��A�A��wA�VA���A|�Av�HAt1'Arr�ApbNAm
=Ai�Af^5Ae�Ae33Ac�Ab�!Ab�Aa�
A`�HA^�/A^A]A]%A[G�AY�AV��AT�DAS|�APjAN�HANv�AN �AM\)AL�\AKG�AIhsAG7LAE&�AB�AAA?7LA=O�A;�A:(�A8~�A7�7A7�A6n�A5O�A3�FA2ȴA1dZA.��A.JA-S�A,Q�A*�yA)�A(jA'G�A&~�A%K�A$1'A#�A"��A"(�A!p�A�!AbA��A�RA  A�Ar�A��AO�A�A�A\)Ap�A-A�;A�AG�A��A�AĜA�TA�A�A
�yA	x�A�uA�Ap�A�DA��A�!A�AO�A �A�FA ȴ@�dZ@�x�@�A�@�+@��-@�x�@��@���@� �@��
@�dZ@�~�@�bN@�+@���@�o@�v�@�1@�R@��@�  @�\@�+@�~�@�!@���@�R@�~�@��@��m@�ȴ@�$�@ܣ�@ە�@ۅ@�{@ؓu@؛�@�1'@�1'@� �@�(�@��@և+@�{@Ցh@�/@ա�@�Ĝ@ӶF@�@�(�@�?}@ɑh@�Z@�n�@��@�7L@ă@�C�@ÍP@�I�@Ĭ@���@��@��@��`@���@��@� �@��T@�9X@���@��@�I�@�ƨ@�t�@��@�V@�@�&�@���@�G�@���@���@��@�/@�x�@�;d@��@���@��T@��@�$�@�@�%@�A�@��/@��@��@�V@���@�o@�V@�V@�n�@�Q�@���@��@���@��@��9@���@���@��D@�z�@���@���@�&�@��j@���@���@�Ĝ@�V@�/@���@�x�@��D@�ƨ@�
=@�ȴ@�E�@��T@��-@���@�$�@��
@�1'@���@�\)@�+@�;d@���@���@��j@�v�@�%@��+@�?}@���@�  @��@��@���@��@�K�@�S�@�dZ@��@���@���@���@�S�@�
=@��T@��@��@��j@�z�@��9@��`@��9@�A�@��@���@�l�@�K�@�@�;d@�;d@��H@�n�@�=q@�{@���@��T@��@�`B@�`B@�`B@�`B@�X@�O�@��@��9@��u@��D@�z�@�Q�@�A�@� �@��m@�ƨ@���@�t�@�S�@�33@��@�n�@�x�@��@��7@��h@��h@��7@�?}@���@�A�@��
@�ƨ@��w@��@���@�"�@��\@�n�@�E�@�/@��/@��@�z�@�A�@�  @��;@��F@��@��@���@��P@�K�@�+@��@���@�ff@�M�@��^@��u@��@��;@��@���@�@�X@���@��`@��/@��u@�1@���@�dZ@�+@��@��@��!@��+@��+@�@���@�(�@���@��P@���@�Z@xbN@o\)@i%@aG�@Zn�@R��@Ix�@B��@:~�@3dZ@.V@)&�@#��@��@@5?@�@��@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB8RB8RB8RB8RB7LB7LB7LB8RB8RB9XB;dB?}BD�BL�BR�BW
BXBYBZB[#B^5Bm�B�%B�B�BÖB\B#�BE�BS�B^5BcTBu�B�hB��B��B��B��B��B��B��B��B��B�oB��B��B��B��B��B��B��B��B�uB�+Bz�Bm�Be`B_;BN�BO�BP�BQ�BVB`BBgmBffB]/BF�B;dB;dBA�BA�B8RB#�B�BDB��B�mB��B��BƨB�}B�qB�?B�!B�B�B��B�oBhsBM�B'�B  B�B�NB��B��B�'B�JBm�BcTBXBO�BA�B8RB49B0!B(�B�BJB
�B
�NB
��B
�jB
�'B
��B
l�B
O�B
+B
	7B	��B	�B	�BB	��B	�}B	�RB	�^B	�RB	�9B	�B	��B	��B	��B	��B	��B	��B	�bB	�1B	~�B	s�B	iyB	bNB	XB	Q�B	O�B	M�B	I�B	E�B	?}B	6FB	-B	"�B	�B	hB	B��B�B�B�B�sB�fB�ZB�BB�)B�B��B��BɺBǮBÖB�}B�qB�dB�dB�XB�LB�LB�?B�3B�-B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B�'B�-B�B�!B�9B�B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�^B��BĜBȴB��B��B��B��B��B�wB�wBBĜB�qB�dBǮBɺB��B��B��B�
B�B�#B�/B�/B�`B�`B�NB�BB�B��BĜB��B�wB��B�wB�qB�}BǮB��B��B�B�
B�
B�
B�B�/B�5B�5B�#B�B�B�;B�NB�`B�yB�B�B�B��B��B	  B	B	B	B	JB	 �B	-B	(�B	)�B	&�B	#�B	$�B	)�B	0!B	8RB	C�B	E�B	E�B	B�B	@�B	F�B	H�B	J�B	YB	ZB	YB	XB	\)B	\)B	^5B	ffB	hsB	m�B	t�B	w�B	x�B	w�B	|�B	}�B	�B	�B	�B	�DB	�bB	�VB	�PB	�DB	�=B	�7B	�DB	�oB	��B	��B	��B	�B	�!B	�-B	�3B	�9B	�FB	�LB	�?B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�3B	�3B	�3B	�LB	��B	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�TB	�ZB	�ZB	�`B	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B
uB
�B
%�B
,B
0!B
6FB
9XB
?}B
C�B
H�B
O�B
VB
[#B
`BB
e`B
jB
m�B
q�B
u�B
z�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B8TB8TB8WB8RB7NB7PB7NB8WB8TB9\B;gB?�BD�BL�BR�BWBXBYBZ#B[(B^7Bm�B�,B�B�BÙB`B#�BE�BS�B^=BcZBu�B�pB��B��B��B��B��B��B��B��B��B�zB��B��B��B��B��B��B��B��B��B�7Bz�Bm�BekB_@BN�BO�BP�BQ�BV
B`KBgrBfoB]8BF�B;lB;jBA�BA�B8[B#�B�BKB��B�vB��B��BƫB��B�tB�DB�&B�B�B��B�xBhzBM�B'�B B�B�QB�B��B�)B�MBm�BcYBXBO�BA�B8WB4?B0)B(�B�BRB
�B
�UB
��B
�sB
�/B
��B
l�B
O�B
+B
	DB	��B	�B	�QB	��B	��B	�aB	�oB	�_B	�IB	�"B	�
B	� B	��B	��B	��B	��B	�rB	�CB	B	s�B	i�B	b`B	X!B	RB	O�B	M�B	I�B	E�B	?�B	6ZB	-!B	"�B	�B	}B	6B��B��B��B�B�B�|B�sB�WB�DB�7B�B��B��B��BðB��B��B�B�}B�oB�gB�eB�YB�LB�HB�5B�5B�4B�-B�&B�#B�"B�$B�$B�'B�(B�!B�B�%B�5B�'B�"B�!B�B�B�B�'B�@B�GB�5B�;B�UB�B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�-B�&B�%B�5B�wB��BĵB��B��B��B��B��B��B��B��B¨BķB��B�}B��B��B��B��B��B�#B�B�9B�EB�EB�uB�uB�fB�XB�/B��BĴB��B��B��B��B��B��B��B��B�B�B�"B�#B�"B�4B�GB�NB�NB�;B�B�,B�SB�eB�wB�B�B��B��B��B�B	 B	!B	&B	2B	_B	 �B	-!B	)	B	*B	&�B	#�B	$�B	*B	05B	8fB	C�B	E�B	E�B	B�B	@�B	F�B	H�B	J�B	Y,B	Z2B	Y+B	X!B	\=B	\<B	^GB	fwB	h�B	m�B	t�B	w�B	x�B	w�B	|�B	~B	�B	�)B	�0B	�UB	�uB	�iB	�bB	�QB	�OB	�FB	�UB	�}B	��B	��B	�B	�%B	�0B	�<B	�BB	�HB	�TB	�ZB	�JB	�*B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�1B	�6B	�5B	�7B	�5B	�<B	�CB	�DB	�EB	�]B	��B	æB	ůB	ƷB	ǿB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�$B	�%B	�&B	�7B	�DB	�IB	�PB	�bB	�hB	�kB	�oB	�uB	�}B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B
 B
B
 B
'B
;B
�B
�B
%�B
,B
0/B
6RB
9cB
?�B
C�B
H�B
O�B
VB
[.B
`LB
eiB
j�B
m�B
q�B
u�B
z�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214392016053112143920160531121439  AO  ARCAADJP                                                                    20140821042008    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140821042008  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140821042008  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121439  IP                  G�O�G�O�G�O�                