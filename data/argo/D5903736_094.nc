CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-11-10T06:01:40Z AOML 3.0 creation; 2016-05-31T19:14:40Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20141110060140  20160531121440  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ^A   AO  4051_7090_094                   2C  D   APEX                            5368                            041511                          846 @�"9/��1   @�"9�� @4��t��dlr� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ^A   A   A   @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  BhffBo��Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD��D�S3D��3D�ٚD���D�S3D�vfD�s3D��D�C3D���D��3D���D�)�Dڀ D���D��D�6fD�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�ff@�ffA ��A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B@33BG��BO��BW��B_��Bh33BofgBw��BfgB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D�3D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��DtvgDy�3D�3D�Q�D���D�� D�� D�Q�D�t�D�q�D�3D�A�D�� D�љD��3D�( D�~fD��3D� D�4�D�k3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�&�A�$�A�&�A�(�A�(�A�(�A�(�A�(�A�&�A�(�A�&�A�(�A�(�A�+A�+A�-A�+A�$�Aݟ�A։7A�=qA���A͡�A��;AǁAř�AÓuA¬A�A�jA���A��;A���A�+A��mA�z�A�1A�hsA��A�A�jA�?}A� �A��A�I�A���A�bA�I�A�1'A�A��PA��#A�XA�E�A���A���A�E�A�G�A�=qA�M�A��hA��hA��A�C�A�O�A�{A��+A�$�A���A�ffA�z�A���A�S�A�"�A��A�K�A��A�JA��A��;A��A�;dA���A�C�A��A�1'A�/A���A�VA��#A�(�A�  A�dZA�z�A�K�A���A�M�A�hsA��hA�1'A|�A{33Ay
=Au��At��Aq\)An�+Al�yAk��Aj��Ai��Ag"�Ac��AbA�A`E�A_&�A]XA[��AZ�AY�TAX��AW+AU�AT��ASAPn�AN-AL1AI�AG��AF�DAEVAC�ABn�AA�hA@�yA>�HA=�A<��A;�A:^5A9�A7l�A5oA3��A0�A.�jA-+A+��A)�;A(9XA&$�A#��A!�7A n�A�uA��A�A�AXAn�AA7LAAI�A�AȴA�A��At�A�A��A=qA
=A9XA��A�yA{A
�A	�AbNAK�A��AȴA�RA��A��A�A�hA��A�A�DA��AdZA ��@���@��7@��h@���@�
=@�7L@��@��D@�dZ@��@�b@�+@�A�@�n�@���@���@�`B@��@�7L@�Z@�1'@���@�ƨ@㕁@�@�`B@�r�@�|�@��@ݩ�@��m@�o@ڸR@�X@؛�@؃@ץ�@ղ-@���@�E�@���@У�@�r�@��@��;@�
=@�5?@́@��
@��@�M�@��/@Ȭ@�9X@�;d@��@��H@�V@��@�x�@��@ě�@��@�l�@�+@��H@�=q@���@��^@�@��h@�?}@��u@�bN@�ƨ@��!@���@��/@�Q�@��P@���@��#@�p�@�O�@�%@�j@��w@�t�@�@�ȴ@���@�ff@�@�@���@��F@��@���@��#@��h@�X@�?}@�&�@��@��@���@�Ĝ@��u@�(�@�dZ@��@�hs@�`B@�/@���@�1@�S�@�"�@�o@�
=@�@�o@�+@�K�@�S�@�@�C�@��\@��#@���@�%@�  @���@�1@���@�1'@��D@��@��`@�Q�@��@���@�I�@�|�@��@�^5@��@�bN@��u@��D@��m@�  @�1@�(�@���@�Z@�  @��@��@���@�1@��F@���@���@��@�|�@�\)@�33@��H@�ff@�-@�@��@��^@��7@�x�@�`B@�G�@�&�@�Ĝ@��@�|�@�\)@��@���@��\@�5?@��@���@�O�@�7L@�/@���@�I�@�b@���@��@��H@���@��!@��\@��@�@��h@�hs@�`B@�O�@�?}@�/@�V@���@��9@���@��@�Q�@�(�@�  @��m@���@�S�@�o@��H@���@��+@�v�@�V@�E�@�=q@�=q@�-@�$�@�J@��@�@��-@���@���@�p�@�O�@�7L@���@��`@��D@�j@�bN@�Z@�9X@�  @��
@��F@���@�\)@�K�@�o@��!@���@�ff@�J@��@���@�@��7@�/@��@��@��D@�bN@�A�@���@�;d@��@���@��R@���@��\@�$�@�J@�J@�J@�@�x�@�X@�&�@�ƨ@��@{dZ@s�F@k�@cC�@\z�@SC�@KC�@B^5@>v�@6V@0A�@.V@)&�@#��@ �9@�
@�y@X@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$�A�&�A�&�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�&�A�$�A�&�A�(�A�(�A�(�A�(�A�(�A�&�A�(�A�&�A�(�A�(�A�+A�+A�-A�+A�$�Aݟ�A։7A�=qA���A͡�A��;AǁAř�AÓuA¬A�A�jA���A��;A���A�+A��mA�z�A�1A�hsA��A�A�jA�?}A� �A��A�I�A���A�bA�I�A�1'A�A��PA��#A�XA�E�A���A���A�E�A�G�A�=qA�M�A��hA��hA��A�C�A�O�A�{A��+A�$�A���A�ffA�z�A���A�S�A�"�A��A�K�A��A�JA��A��;A��A�;dA���A�C�A��A�1'A�/A���A�VA��#A�(�A�  A�dZA�z�A�K�A���A�M�A�hsA��hA�1'A|�A{33Ay
=Au��At��Aq\)An�+Al�yAk��Aj��Ai��Ag"�Ac��AbA�A`E�A_&�A]XA[��AZ�AY�TAX��AW+AU�AT��ASAPn�AN-AL1AI�AG��AF�DAEVAC�ABn�AA�hA@�yA>�HA=�A<��A;�A:^5A9�A7l�A5oA3��A0�A.�jA-+A+��A)�;A(9XA&$�A#��A!�7A n�A�uA��A�A�AXAn�AA7LAAI�A�AȴA�A��At�A�A��A=qA
=A9XA��A�yA{A
�A	�AbNAK�A��AȴA�RA��A��A�A�hA��A�A�DA��AdZA ��@���@��7@��h@���@�
=@�7L@��@��D@�dZ@��@�b@�+@�A�@�n�@���@���@�`B@��@�7L@�Z@�1'@���@�ƨ@㕁@�@�`B@�r�@�|�@��@ݩ�@��m@�o@ڸR@�X@؛�@؃@ץ�@ղ-@���@�E�@���@У�@�r�@��@��;@�
=@�5?@́@��
@��@�M�@��/@Ȭ@�9X@�;d@��@��H@�V@��@�x�@��@ě�@��@�l�@�+@��H@�=q@���@��^@�@��h@�?}@��u@�bN@�ƨ@��!@���@��/@�Q�@��P@���@��#@�p�@�O�@�%@�j@��w@�t�@�@�ȴ@���@�ff@�@�@���@��F@��@���@��#@��h@�X@�?}@�&�@��@��@���@�Ĝ@��u@�(�@�dZ@��@�hs@�`B@�/@���@�1@�S�@�"�@�o@�
=@�@�o@�+@�K�@�S�@�@�C�@��\@��#@���@�%@�  @���@�1@���@�1'@��D@��@��`@�Q�@��@���@�I�@�|�@��@�^5@��@�bN@��u@��D@��m@�  @�1@�(�@���@�Z@�  @��@��@���@�1@��F@���@���@��@�|�@�\)@�33@��H@�ff@�-@�@��@��^@��7@�x�@�`B@�G�@�&�@�Ĝ@��@�|�@�\)@��@���@��\@�5?@��@���@�O�@�7L@�/@���@�I�@�b@���@��@��H@���@��!@��\@��@�@��h@�hs@�`B@�O�@�?}@�/@�V@���@��9@���@��@�Q�@�(�@�  @��m@���@�S�@�o@��H@���@��+@�v�@�V@�E�@�=q@�=q@�-@�$�@�J@��@�@��-@���@���@�p�@�O�@�7L@���@��`@��D@�j@�bN@�Z@�9X@�  @��
@��F@���@�\)@�K�@�o@��!@���@�ff@�J@��@���@�@��7@�/@��@��@��D@�bN@�A�@���@�;d@��@���@��R@���@��\@�$�@�J@�J@�J@�@�x�@�X@�&�@�ƨ@��@{dZ@s�F@k�@cC�@\z�@SC�@KC�@B^5@>v�@6V@0A�@.V@)&�@#��@ �9@�
@�y@X@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BT�BS�BS�BS�BS�BS�BS�BT�B^5B�9B��B��B��B��B�!B�RB�?B�-B�B�B��B��B��B��B�VB�Bv�Bm�BiyBhsBe`BcTBaHB^5BW
BH�B;dB5?B33B1'B+B!�BoBB��B�/B��B�^B��B�\B�Bt�Bk�B]/BD�B>wB5?B#�B�BDB��B�B�B�fB�NB�B��B��B��B��B�dB��B�VBx�Bk�BcTBVBM�BC�B:^B+B�B
��B
�sB
�TB
��B
�-B
��B
��B
}�B
e`B
T�B
C�B
)�B
�B
  B	�B	�HB	�B	��B	ǮB	�XB	��B	��B	�JB	~�B	p�B	dZB	]/B	[#B	T�B	K�B	D�B	?}B	9XB	.B	&�B	�B	bB	1B	B	B��B��B�B�B�sB�TB�BB�HB�ZB�TB�5B��BƨB��B�^B�FB�9B�B��B��B��B��B��B��B��B�uB�oB�\B�PB�DB�7B�B�B~�B}�B}�B|�B}�B|�B|�B{�Bz�By�Bx�Bw�Bv�Bv�Bu�Bu�Bu�Bv�Bv�Bv�Bu�Bt�Bs�Br�Br�Bq�Bp�Bp�Bp�Bo�Bn�Bm�Bn�Bn�Bl�BjBiyBhsBiyBgmBgmBhsBcTBffBffBe`BgmBhsBn�Br�Bs�Bv�By�B�B�B�1B�1B�DB�=B�7B�bB�uB�{B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�?B�XB�jBÖBŢBɺB��B��B��B��B��B�B�
B�B�#B�/B�5B�5B�HB�HB�NB�ZB�`B�mB�B�B�B�B�B��B��B��B��B	  B	B	B	B	%B	DB	VB	hB	oB	uB	�B	�B	�B	�B	!�B	&�B	,B	-B	.B	0!B	0!B	1'B	1'B	1'B	2-B	33B	49B	8RB	<jB	B�B	K�B	N�B	P�B	Q�B	S�B	W
B	XB	ZB	ZB	\)B	^5B	aHB	e`B	gmB	gmB	l�B	q�B	t�B	t�B	t�B	x�B	y�B	{�B	}�B	�B	�+B	�JB	�\B	�PB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�?B	�?B	�FB	�^B	�jB	�wB	�}B	��B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�BB	�BB	�BB	�HB	�HB	�HB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B

=B
JB
DB
JB
PB
VB
VB
\B
\B
hB
{B
�B
�B
&�B
-B
33B
6FB
>wB
C�B
I�B
M�B
R�B
XB
XB
]/B
aHB
e`B
jB
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BTBTBUBS�BTBTBTBTBTBUB^?B�CB� B��B��B��B�*B�ZB�IB�9B� B�B�B��B��B��B�`B�Bv�Bm�Bi�BhzBejBc]BaNB^>BWBH�B;jB5GB39B1/B+B!�BxB$B��B�5B��B�dB��B�aB�Bt�Bk�B]6BD�B>~B5GB#�B�BGB��B�B�B�kB�SB�B��B��B��B��B�hB��B�YBx�Bk�Bc\BV
BM�BC�B:eB+B�B
��B
�zB
�YB
� B
�6B
��B
��B
}�B
ekB
U
B
C�B
*B
�B
 B	�B	�VB	�B	��B	��B	�hB	��B	��B	�ZB	B	p�B	dlB	]BB	[6B	UB	K�B	D�B	?�B	9kB	.*B	&�B	�B	yB	HB	0B	B��B��B��B�B�B�mB�YB�`B�sB�nB�OB��B��B��B�zB�`B�RB�/B�B��B��B��B��B��B��B��B��B�zB�lB�bB�RB�9B�'BB~B~B}	B~B}B}
B|Bz�By�Bx�Bw�Bv�Bv�Bu�Bu�Bu�Bv�Bv�Bv�Bu�Bt�Bs�Br�Br�Bq�Bp�Bp�Bp�Bo�Bn�Bm�Bn�Bn�Bl�Bj�Bi�Bh�Bi�Bg�Bg�Bh�BcqBf�Bf�Be}Bg�Bh�Bn�Br�Bs�Bv�By�B�"B�3B�LB�KB�_B�XB�PB�|B��B��B��B��B��B��B��B��B�B�!B�B�B�#B�!B�.B�:B�<B�ZB�pB��BðBźB��B��B��B��B�B�B�B�"B�/B�;B�EB�LB�KB�aB�^B�fB�rB�uB�B�B�B��B��B��B��B��B��B�B	 B	"B	'B	0B	9B	XB	lB	|B	�B	�B	�B	�B	�B	�B	!�B	&�B	,B	-"B	.)B	04B	05B	19B	19B	1;B	2@B	3EB	4MB	8eB	<~B	B�B	K�B	N�B	P�B	Q�B	TB	WB	X"B	Z/B	Z.B	\<B	^IB	a\B	epB	g}B	g�B	l�B	q�B	t�B	t�B	t�B	x�B	y�B	{�B	~B	�B	�;B	�YB	�mB	�`B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�0B	�7B	�<B	�CB	�HB	�IB	�LB	�MB	�UB	�mB	�yB	��B	��B	��B	äB	īB	ƶB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�#B	�+B	�+B	�9B	�>B	�EB	�EB	�QB	�QB	�QB	�TB	�XB	�XB	�bB	�cB	�cB	�jB	�gB	�nB	�mB	�pB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B
 B
 B
B
B
B
B
B
B
B
'B
(B
*B
*B
,B
'B
%B
)B
?B
	BB

HB
WB
QB
YB
^B
bB
aB
gB
iB
tB
�B
�B
�B
&�B
-B
3>B
6QB
>�B
C�B
I�B
M�B
R�B
XB
XB
]7B
aSB
ejB
j�B
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214402016053112144020160531121440  AO  ARCAADJP                                                                    20141110060140    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141110060140  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141110060140  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121440  IP                  G�O�G�O�G�O�                