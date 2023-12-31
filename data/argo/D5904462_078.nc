CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-29T02:15:48Z AOML 3.0 creation; 2016-08-07T21:51:22Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150929021548  20160807145122  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               NA   AO  5287_9017_078                   2C  D   APEX                            6529                            072314                          846 @�r��=1   @�r�)�	�@/�E�����d�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    NA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD��D�C3D��3D���D�fD�S3D�s3D���D� D�VfD�s3D��3D�fD�C3DږfD�� D� D�,�D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BBQ�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C�{Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy�D�(�D�R�D���D��)D�%�D�b�D���D��)D�\D�e�D���D��D�%�D�R�Dڥ�D��\D�\D�<)D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�JA�JA�VA��A��A� �A��A�$�A�$�A�"�A��A��A�
=A��A�/A�^5A�dZA�r�A�%A�|�A�VA�uA�FA�?}A�{AߓuA��mA��A�oA�%A�z�A�VA�(�A�~�A�A�~�AԃA�=qA��#A�G�A�$�A�A��#AҬA��Aя\A���Aϡ�A���A̰!A�K�A��`A�t�A�A�1A��A�VA��A�C�A��Ať�A���Aĺ^A�-A�n�A��mA�A��A�bNA���A���A�G�A��`A��A�ffA�A��9A���A�5?A�ȴA���A�A�K�A��A��wA�r�A�ffA��PA���A�O�A��;A��
A�^5A�9XA��FA�&�A���A��;A��jA�XA�M�A�r�A���A��A��
A�5?A�x�A���A�%A��\A�oA{`BAx�\Avv�ArA�Aq�
AqVAl{Aj�+Ad�`A_7LA\jA\�AZAV��AU��AQƨAM��AL��AL1'AJQ�AGoAD5?AAC�A>  A<��A;p�A:{A81'A6ȴA5�A4��A4A�A3p�A1��A0^5A/A-�mA,�jA+��A*��A)S�A(1'A&�RA%�TA%
=A$�A$9XA!S�AA{A;dA�A��AG�A�A|�A�RAO�A�
AO�A�!AE�A�mA��A�A\)A%A=qA%A��A�mA�TAoA{A�yA��A��A1Al�A
$�A	O�A1'A�-AbAĜAAbA|�AVA �+@���@��@��9@�1@�\)@�v�@��-@���@�Z@���@���@�1@�+@�V@��@�9X@���@���@�Q�@���@�dZ@���@�(�@�K�@�"�@�A�@�X@�$�@��@�C�@�^5@�O�@��H@�E�@���@땁@�w@ꗍ@�t�@�h@�33@�
=@�%@���@��m@�\@�-@�I�@�E�@��/@�j@�j@�%@��@��@�ff@�~�@�V@ڸR@�=q@ܛ�@�ƨ@ۥ�@�C�@�E�@��@��@��#@ى7@�X@�V@���@�\)@��@�r�@�C�@�n�@��@мj@�1@�  @�l�@Ώ\@��@���@�b@�(�@�I�@�Q�@�b@��m@�t�@ʏ\@Ɂ@ɑh@�-@���@ɉ7@��/@ȣ�@�j@�(�@�(�@�1'@��
@Ǯ@ǅ@Ƨ�@�M�@ř�@��`@�A�@�1'@���@�ƨ@�S�@�-@���@��@��@�E�@�p�@�?}@�V@��`@��/@���@���@�Ĝ@�Z@���@���@��H@�@��#@�@��#@�M�@�$�@��@��-@��h@�%@��/@���@�A�@��@�ff@��@�@�&�@��j@�Q�@�1'@�  @��@���@�n�@�{@��@���@��@�1'@���@�ƨ@���@�t�@���@��@��@��\@�n�@���@�ȴ@��\@���@���@��#@�M�@��@�  @��@��@��R@��+@�M�@�$�@��@���@���@�?}@��/@�%@��9@�r�@�I�@�1'@��@� �@�b@���@���@�5?@�J@��h@��@�`B@�?}@��/@��j@��u@�I�@�A�@��@�C�@���@���@�n�@�E�@��@�{@�@��T@���@�V@�j@��w@��P@�;d@�
=@���@��+@�{@��-@�x�@�G�@���@��9@��u@�Z@�1'@�  @��P@�33@��@���@�M�@�5?@���@��@�X@��@��/@��j@��j@��9@��D@�9X@�  @��F@��@�\)@�;d@��@�ȴ@�ff@�-@��@���@���@���@�7L@�bN@�  @�ƨ@���@�33@�@���@�ff@�J@���@�X@��@���@�{@���@�+@~E�@r�\@gl�@`�9@Y�@O�@F5?@?\)@6v�@1�#@,��@&��@ Q�@1@  @ƨ@bN@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�A�A�JA�JA�VA��A��A� �A��A�$�A�$�A�"�A��A��A�
=A��A�/A�^5A�dZA�r�A�%A�|�A�VA�uA�FA�?}A�{AߓuA��mA��A�oA�%A�z�A�VA�(�A�~�A�A�~�AԃA�=qA��#A�G�A�$�A�A��#AҬA��Aя\A���Aϡ�A���A̰!A�K�A��`A�t�A�A�1A��A�VA��A�C�A��Ať�A���Aĺ^A�-A�n�A��mA�A��A�bNA���A���A�G�A��`A��A�ffA�A��9A���A�5?A�ȴA���A�A�K�A��A��wA�r�A�ffA��PA���A�O�A��;A��
A�^5A�9XA��FA�&�A���A��;A��jA�XA�M�A�r�A���A��A��
A�5?A�x�A���A�%A��\A�oA{`BAx�\Avv�ArA�Aq�
AqVAl{Aj�+Ad�`A_7LA\jA\�AZAV��AU��AQƨAM��AL��AL1'AJQ�AGoAD5?AAC�A>  A<��A;p�A:{A81'A6ȴA5�A4��A4A�A3p�A1��A0^5A/A-�mA,�jA+��A*��A)S�A(1'A&�RA%�TA%
=A$�A$9XA!S�AA{A;dA�A��AG�A�A|�A�RAO�A�
AO�A�!AE�A�mA��A�A\)A%A=qA%A��A�mA�TAoA{A�yA��A��A1Al�A
$�A	O�A1'A�-AbAĜAAbA|�AVA �+@���@��@��9@�1@�\)@�v�@��-@���@�Z@���@���@�1@�+@�V@��@�9X@���@���@�Q�@���@�dZ@���@�(�@�K�@�"�@�A�@�X@�$�@��@�C�@�^5@�O�@��H@�E�@���@땁@�w@ꗍ@�t�@�h@�33@�
=@�%@���@��m@�\@�-@�I�@�E�@��/@�j@�j@�%@��@��@�ff@�~�@�V@ڸR@�=q@ܛ�@�ƨ@ۥ�@�C�@�E�@��@��@��#@ى7@�X@�V@���@�\)@��@�r�@�C�@�n�@��@мj@�1@�  @�l�@Ώ\@��@���@�b@�(�@�I�@�Q�@�b@��m@�t�@ʏ\@Ɂ@ɑh@�-@���@ɉ7@��/@ȣ�@�j@�(�@�(�@�1'@��
@Ǯ@ǅ@Ƨ�@�M�@ř�@��`@�A�@�1'@���@�ƨ@�S�@�-@���@��@��@�E�@�p�@�?}@�V@��`@��/@���@���@�Ĝ@�Z@���@���@��H@�@��#@�@��#@�M�@�$�@��@��-@��h@�%@��/@���@�A�@��@�ff@��@�@�&�@��j@�Q�@�1'@�  @��@���@�n�@�{@��@���@��@�1'@���@�ƨ@���@�t�@���@��@��@��\@�n�@���@�ȴ@��\@���@���@��#@�M�@��@�  @��@��@��R@��+@�M�@�$�@��@���@���@�?}@��/@�%@��9@�r�@�I�@�1'@��@� �@�b@���@���@�5?@�J@��h@��@�`B@�?}@��/@��j@��u@�I�@�A�@��@�C�@���@���@�n�@�E�@��@�{@�@��T@���@�V@�j@��w@��P@�;d@�
=@���@��+@�{@��-@�x�@�G�@���@��9@��u@�Z@�1'@�  @��P@�33@��@���@�M�@�5?@���@��@�X@��@��/@��j@��j@��9@��D@�9X@�  @��F@��@�\)@�;d@��@�ȴ@�ff@�-@��@���@���@���@�7L@�bN@�  @�ƨ@���@�33@�@���@�ff@�J@���@�X@��G�O�@�{@���@�+@~E�@r�\@gl�@`�9@Y�@O�@F5?@?\)@6v�@1�#@,��@&��@ Q�@1@  @ƨ@bN@/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
DB
DB
DB
DB
DB

=B

=B

=B

=B

=B

=B

=B
DB

=B
DB
\B
uB
�B
)�B
9XB
5?B
49B
5?B
49B
-B
)�B
&�B
uB
B
B
DB
hB
�B
�B
�B
#�B
.B
R�B
gmB
k�B
iyB
r�B
y�B
� B
�B
�oB
��B
ɺB
�B �BW
By�B�B�hB��B�!B��B�yB%B�B)�B/B1'B>wBC�BL�BW
B]/BiyBt�B�B�7B�DB�JB�JB�+B�%B�7B�DB�PB�BiyBO�B:^B-B�B�B�BJB��B�B�sB�HB�
B�XB��B��B�bB�BiyB=qB�B
��B
�`B
�
B
�FB
��B
�DB
u�B
\)B
R�B
G�B
%�B
hB
B	�B	�;B	�)B	��B	�9B	��B	|�B	W
B	I�B	E�B	:^B	-B	#�B	uB	%B	B��B�B�mB�fB�HB�#B�B��B��BɺBƨBÖB��B�wB�qB�RB�?B�FB�9B�B�B��B��B��B��B�B�B�-B�3B��B�B�B��B��B��B�B��B��B��B�XB��B��B��B�B�B�)B�/B�/B�#B�/B�mB�B�B��B��B��B�B�B�B�B��B��B��B��B��B��B��B�B�B�sB�fB�ZB�B��B��B��B��B��B	B	B	B	B	B	B	%B	VB	VB	\B	�B	�B	 �B	-B	/B	-B	-B	1'B	7LB	J�B	YB	dZB	� B	�B	}�B	z�B	u�B	t�B	y�B	�B	�B	�B	w�B	q�B	jB	k�B	}�B	�B	�B	�B	�B	{�B	t�B	t�B	v�B	v�B	�B	�B	�%B	�oB	��B	�\B	�7B	�1B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�FB	�LB	�?B	�9B	�FB	�^B	�dB	�wB	�}B	��B	B	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ƨB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	�B	�B	�
B	�B	�
B	�B	�B	�B	��B	��B	��B	�
B	�B	�B	�5B	�BB	�;B	�BB	�BB	�BB	�BB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
	7B
	7B

=B
DB
JB
JB
JB
JB
PB
VB
bB
bB
\B
\B
\B
bB
hB
hB
oB
uB
uB
uB
{B
{B
{B
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
+B
33B
?}B
C�B
H�B
N�B
T�B
\)B
_;B
dZB
hsB
m�B
q�B
u�B
y�B
{�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
)B
+B
*B
+B
+B

"B

"B

"B

 B

 B

 B

"B
)B

 B
*B
@B
]B
�B
)�B
9:B
5$B
4B
5B
4B
,�B
)�B
&�B
XB
�B
�B
&B
KB
fB
vB
�B
#�B
-�B
R�B
gKB
keB
iYB
r�B
y�B
�B
��B
�KB
��B
ɖB
�B �BV�By�B��B�>B��B��BαB�RB�BtB)�B.�B0�B>RBCmBL�BV�B]BiQBt�B��B�B�B�B� B�B��B�
B�B�*B��BiMBO�B:3B,�B�BvBfBB��B�B�GB�B��B�,B��B�zB�5B��BiJB=FBnB
��B
�6B
��B
�B
��B
�B
u�B
[�B
R�B
G�B
%�B
=B
�B	�B	�B	� B	��B	�B	��B	|�B	V�B	I�B	E�B	::B	,�B	#�B	QB	B	 �B��B��B�KB�FB�%B��B��B��BεBɘBƃB�tB�aB�TB�NB�.B�B�$B�B��B��B��B��B��B��B��B��B�
B�B��B��B��B��B��B��B��B��B��B��B�1B��B��B��B��B��B�B�B�B��B�B�EB�WB�B��B��B��B�B�gB�vB�B��B��B��B��B��B��B��B�vB�VB�IB�;B�1B�YB��B��B��B��B��B	�B	�B	�B	�B	�B	�B	�B	)B	+B	1B	XB	�B	 �B	,�B	.�B	,�B	,�B	0�B	7B	J�B	X�B	d*B	�B	��B	}�B	z�B	u�B	t�B	y�B	��B	��B	��B	w�B	qzB	jQB	kUB	}�B	��B	��B	��B	��B	{�B	t�B	t�B	v�B	v�B	��B	��B	��B	�=B	�OB	�)B	�B	��B	�lB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�zB	�sB	�mB	�`B	�sB	�yB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�*B	�2B	�CB	�GB	�PB	�YB	�oB	�rB	�zB	˓B	͠B	΢B	ΣB	͝B	̘B	ʍB	ʋB	ˑB	͠B	͜B	̘B	˒B	ɆB	�rB	�kB	�zB	�yB	ɅB	˓B	˓B	̔B	̖B	˓B	˓B	ˏB	̕B	ˑB	ʊB	ɄB	ɅB	ʌB	ΧB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�$B	�%B	�%B	�$B	�&B	�1B	�1B	�5B	�=B	�HB	�SB	�UB	�TB	�YB	�sB	��B	�B	�hB	�RB	�SB	�SB	�VB	�SB	�XB	�`B	�aB	�aB	�hB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B

B
B
B
B
B
B
B
)B
*B
&B
$B
$B
*B
0B
1B
5B
=B
>B
<B
BB
BB
CB
>B
<B
<B
@B
CB
BB
IB
IB
JB
HG�O�B
HB
fB
~B
 �B
*�B
2�B
??B
C\B
HyB
N�B
T�B
[�B
_B
dB
h9B
mXB
qpB
u�B
y�B
{�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451222016080714512220160807145122  AO  ARCAADJP                                                                    20150929021548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150929021548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150929021548  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145122  IP                  G�O�G�O�G�O�                