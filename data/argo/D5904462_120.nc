CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-07T19:18:15Z AOML 3.0 creation; 2016-08-07T21:51:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160507191815  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               xA   AO  5287_9017_120                   2C  D   APEX                            6529                            072314                          846 @תUA��1   @תU��)-@0̋C���dڗ�O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    xA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�P D���D�� D�#3D�VfD�s3D��fD� D�L�D�vfD��fD�fD�FfDچfD��fD�3D�I�D� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B�(�B���B���B�B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D%D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�,)D�_\D���D��\D�2�D�e�D���D���D�\D�\)D���D���D��D�U�Dڕ�D���D��D�X�D�\D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AՁAՋDAՏ\AՏ\AՋDA�x�A�`BA�
=A�ƨA���A�=qAӰ!A�33AҮAҗ�A�x�A�I�A��A���A�ƨAѮAя\Aћ�AѬAћ�Aщ7AхA�|�A�v�A�n�A�K�A�/A��A���A��`A�ĜAд9AЧ�AН�AЉ7A�n�A�p�A�ffA�Q�A�C�A��A�XA���A�E�A�E�A�$�A��TA�A�hsA�1'A��A�$�A�&�AǮA���A�VAœuA�O�A�z�Aô9A�G�A©�A�%A���A�oA��9A�5?A��/A��mA��hA�ƨA�+A��PA��mA�9XA�^5A�VA��A��\A���A��TA���A���A���A��hA�~�A�1A���A��A���A��A�G�A���A�~�A�S�A��A�p�A���A���A��PA�x�A�ZA�E�A��+A��A`BAz�Az{Asp�An�HAk�wAg��AdVAb �A`5?A]��A\AZ5?AWVAV�+AT�/ASXAP-AN��AN^5ALn�AH��AFAB�DA?��A=�;A=
=A<VA;|�A9K�A7�TA6jA3�mA2�yA1�#A/�mA. �A-p�A,�A,I�A+`BA*�DA)�A)��A(M�A%A%G�A%�A$n�A"�A   A��AbNA(�AƨAC�A�AbNA��A��A;dA�9An�A�AjA�A��A�yA$�A�mA�TAA��A��A{AC�Az�A�A�PA
��A=qA|�A�HAn�AbA\)A�Av�A$�A�A?}A��A$�Al�A\)AbNA~�An�A�A�A �!@��@��@��@��@���@��/@�V@���@�;d@���@�t�@�V@��!@�J@�E�@�5?@��-@�`B@���@�l�@�ȴ@��@��@�A�@��@��@�j@�dZ@��@�R@�M�@陚@�Z@��m@���@�|�@��@��@�Ĝ@�1@�E�@�x�@�p�@�hs@�`B@�G�@��@��m@ߕ�@�@���@�V@ܣ�@ܓu@�1'@�K�@�-@��@��#@ّh@�X@��@�1@��@֗�@�=q@���@Ցh@�x�@�hs@�`B@�?}@�&�@���@ԃ@���@�t�@�"�@�@��y@ҟ�@�@�x�@��`@��;@��y@�@́@�%@�bN@�  @��@��T@�&�@���@Ȭ@Ǯ@��@Ƨ�@Ə\@�M�@�J@�p�@�A�@�t�@�33@°!@�J@��#@���@�`B@�O�@�7L@��@��`@��D@��@��@�;d@���@�@��-@��h@��@��9@�(�@��m@���@��w@�|�@��@�~�@�$�@�@�O�@���@�I�@��@��w@���@�K�@��@��H@���@���@���@�`B@���@�Ĝ@�r�@�(�@���@��@���@�E�@�$�@��@���@���@��^@��-@�x�@�/@��D@�b@��;@���@�l�@�dZ@�+@��@�V@�J@���@�`B@�G�@�&�@�j@�b@��;@�K�@��y@�n�@��T@���@��`@�Z@���@�C�@�33@��R@�v�@�-@��#@��@��@���@�Z@�9X@��m@��@���@�dZ@�
=@��H@���@�n�@�5?@�@�@�hs@��`@��D@�r�@�Z@�A�@�b@�|�@�+@�@���@�-@��@���@�p�@�?}@���@�z�@���@�ƨ@��@�l�@�K�@�+@�
=@��y@���@�-@���@���@��@�?}@�V@���@�A�@��@�ƨ@��@���@��@�C�@�ȴ@�-@��7@�7L@�V@���@��D@�A�@�(�@�b@��m@���@�;d@��@��@�ȴ@���@�M�@��@��h@�X@�/@�Ĝ@�A�@�  @���@�hs@��y@��\@K�@t�@g��@`  @UO�@M`B@I7L@E�-@=�@6$�@-V@'K�@#@�D@��@��@�H@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AՁAՋDAՏ\AՏ\AՋDA�x�A�`BA�
=A�ƨA���A�=qAӰ!A�33AҮAҗ�A�x�A�I�A��A���A�ƨAѮAя\Aћ�AѬAћ�Aщ7AхA�|�A�v�A�n�A�K�A�/A��A���A��`A�ĜAд9AЧ�AН�AЉ7A�n�A�p�A�ffA�Q�A�C�A��A�XA���A�E�A�E�A�$�A��TA�A�hsA�1'A��A�$�A�&�AǮA���A�VAœuA�O�A�z�Aô9A�G�A©�A�%A���A�oA��9A�5?A��/A��mA��hA�ƨA�+A��PA��mA�9XA�^5A�VA��A��\A���A��TA���A���A���A��hA�~�A�1A���A��A���A��A�G�A���A�~�A�S�A��A�p�A���A���A��PA�x�A�ZA�E�A��+A��A`BAz�Az{Asp�An�HAk�wAg��AdVAb �A`5?A]��A\AZ5?AWVAV�+AT�/ASXAP-AN��AN^5ALn�AH��AFAB�DA?��A=�;A=
=A<VA;|�A9K�A7�TA6jA3�mA2�yA1�#A/�mA. �A-p�A,�A,I�A+`BA*�DA)�A)��A(M�A%A%G�A%�A$n�A"�A   A��AbNA(�AƨAC�A�AbNA��A��A;dA�9An�A�AjA�A��A�yA$�A�mA�TAA��A��A{AC�Az�A�A�PA
��A=qA|�A�HAn�AbA\)A�Av�A$�A�A?}A��A$�Al�A\)AbNA~�An�A�A�A �!@��@��@��@��@���@��/@�V@���@�;d@���@�t�@�V@��!@�J@�E�@�5?@��-@�`B@���@�l�@�ȴ@��@��@�A�@��@��@�j@�dZ@��@�R@�M�@陚@�Z@��m@���@�|�@��@��@�Ĝ@�1@�E�@�x�@�p�@�hs@�`B@�G�@��@��m@ߕ�@�@���@�V@ܣ�@ܓu@�1'@�K�@�-@��@��#@ّh@�X@��@�1@��@֗�@�=q@���@Ցh@�x�@�hs@�`B@�?}@�&�@���@ԃ@���@�t�@�"�@�@��y@ҟ�@�@�x�@��`@��;@��y@�@́@�%@�bN@�  @��@��T@�&�@���@Ȭ@Ǯ@��@Ƨ�@Ə\@�M�@�J@�p�@�A�@�t�@�33@°!@�J@��#@���@�`B@�O�@�7L@��@��`@��D@��@��@�;d@���@�@��-@��h@��@��9@�(�@��m@���@��w@�|�@��@�~�@�$�@�@�O�@���@�I�@��@��w@���@�K�@��@��H@���@���@���@�`B@���@�Ĝ@�r�@�(�@���@��@���@�E�@�$�@��@���@���@��^@��-@�x�@�/@��D@�b@��;@���@�l�@�dZ@�+@��@�V@�J@���@�`B@�G�@�&�@�j@�b@��;@�K�@��y@�n�@��T@���@��`@�Z@���@�C�@�33@��R@�v�@�-@��#@��@��@���@�Z@�9X@��m@��@���@�dZ@�
=@��H@���@�n�@�5?@�@�@�hs@��`@��D@�r�@�Z@�A�@�b@�|�@�+@�@���@�-@��@���@�p�@�?}@���@�z�@���@�ƨ@��@�l�@�K�@�+@�
=@��y@���@�-@���@���@��@�?}@�V@���@�A�@��@�ƨ@��@���@��@�C�@�ȴ@�-@��7@�7L@�V@���@��D@�A�@�(�@�b@��m@���@�;d@��@��@�ȴ@���@�M�@��@��h@�X@�/@�Ĝ@�A�@�  G�O�@�hs@��y@��\@K�@t�@g��@`  @UO�@M`B@I7L@E�-@=�@6$�@-V@'K�@#@�D@��@��@�H@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
v�B
{�B
�B
�}B
�5BB{B{B{B{BuB�B�B!�B%�B.B6FBC�BF�BF�BG�BH�BJ�BM�BO�BM�BM�BL�BK�BJ�BJ�BJ�BJ�BH�BK�BL�BL�BJ�BJ�BW
B]/B_;B\)BG�BA�B?}BG�BS�B{�B��B��B�sB+B �BB�BI�Be`B�B�uB��BŢB�5B�`B�fB�yB�yB�B�B�sB�ZB��B{B\BB��B��B��Bw�BS�B �B�B  B�B�yBȴB�JB�B�B�bB��B{�BcTB$�B+B
��B
�yB
ȴB
�9B
��B
�B
dZB
N�B
G�B
5?B
�B
VB	�yB	ȴB	�?B	��B	�DB	�=B	�=B	w�B	hsB	\)B	N�B	L�B	D�B	H�B	F�B	>wB	8RB	(�B	{B	+B��B�B�sB�`B�TB�;B�#B�
B��B��B��B��B��B��B��B��BɺB��B��B��B��B��B�B�B�B�sB�`B�/B�B�B�)B�#B�)B�NB�mB�B�B�B��B��B	B	1B		7B	1B	+B	B	B	%B	+B��B�B�B�sB�fB�ZB�ZB�TB�#B�/B�BB�HB�HB�BB�NB��B��B��B��B�B�B�B��B	1B	�B	�B	�B	�B	!�B	 �B	 �B	"�B	"�B	!�B	+B	33B	7LB	;dB	<jB	A�B	G�B	B�B	D�B	F�B	G�B	G�B	J�B	J�B	H�B	H�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	L�B	M�B	N�B	O�B	P�B	P�B	Q�B	VB	VB	T�B	S�B	YB	^5B	_;B	_;B	`BB	aHB	bNB	e`B	e`B	e`B	gmB	hsB	jB	jB	k�B	l�B	p�B	p�B	p�B	q�B	r�B	s�B	v�B	x�B	z�B	{�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�JB	�VB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�-B	�3B	�9B	�9B	�9B	�?B	�FB	�LB	�XB	�^B	�dB	�qB	�wB	�wB	�}B	��B	��B	B	B	B	ÖB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�NB	�TB	�TB	�fB	�fB	�`B	�fB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
\B
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
#�B
&�B
.B
5?B
;dB
C�B
I�B
M�B
Q�B
W
B
^5B
dZB
iyB
m�B
q�B
s�B
v�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
v�B
{�B
��B
�WB
�B �BTBVBUBUBPBaBB!�B%�B-�B6!BCqBF�BF�BG�BH�BJ�BM�BO�BM�BM�BL�BK�BJ�BJ�BJ�BJ�BH�BK�BL�BL�BJ�BJ�BV�B]	B_B\BG�BAdB?YBG�BS�B{�B��B˝B�KBB �BBgBI�Be:B��B�NB��B�}B�B�:B�?B�RB�RB�\B�[B�LB�0B��BPB5B�B��B��B��Bw�BS�B �BeB��B�B�MBȈB� B��B��B�4B�WB{�Bc'B$�B�B
��B
�MB
ȇB
�B
��B
��B
d-B
N�B
G�B
5B
rB
-B	�QB	ȊB	�B	�sB	�B	�B	�B	w�B	hLB	\B	N�B	L�B	DxB	H�B	F�B	>OB	8.B	(�B	WB	B��B�tB�OB�<B�1B�B��B��B��BϼBͮBˣBʞBˡBʟBʟBɖBʞBʞBˣBʞBͭB��B��B��B�KB�:B�	B��B��B�B��B�B�'B�GB�eB�cB�wB��B��B	�B		B		B	B	B	�B	�B	�B	B��B�B�mB�MB�=B�1B�3B�+B��B�B�B� B�B�B�$B��B��B��B��B�uB�VB�eB��B	B	TB	dB	�B	�B	!�B	 �B	 �B	"�B	"�B	!�B	*�B	3B	7B	;7B	<<B	A\B	GB	B`B	DpB	FzB	G�B	G}B	J�B	J�B	H�B	H�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	L�B	M�B	N�B	O�B	P�B	P�B	Q�B	U�B	U�B	T�B	S�B	X�B	^B	_B	_B	`B	aB	bB	e-B	e0B	e/B	g<B	hBB	jPB	jLB	kUB	lYB	psB	ptB	pqB	qxB	r~B	s�B	v�B	x�B	z�B	{�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�0B	�>B	�;B	�EB	�HB	�KB	�YB	�lB	�{B	�yB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�
B	�B	�B	�#B	�'B	�1B	�;B	�DB	�DB	�IB	�PB	�SB	�[B	�[B	�\B	�`B	�nB	�sB	�tB	�wB	�~B	ʌB	ʌB	ˑB	̖B	̘B	͠B	ΥB	͜B	΢B	ЯB	ЮB	аB	ҾB	һB	��B	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�.B	�.B	�)B	�0B	�.B	�<B	�<B	�IB	�FB	�FB	�PB	�OB	�UB	�SB	�cB	�aB	�aB	�fB	�eB	�rB	�tB	�wB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B
�B

B

B

B
B
B
B
B
B
B
B
B
B
B
B
 B
%B
*B
(B
)B
*B
)B
*B
,B
+B
/B
.B
1B
0B
.B
.B
3B
7B
=B
>B
=B
<B
DB
BG�O�B
PB
�B
#�B
&�B
-�B
5B
;)B
C[B
I�B
M�B
Q�B
V�B
]�B
d B
i=B
mVB
qsB
s{B
v�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451292016080714512920160807145129  AO  ARCAADJP                                                                    20160507191815    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160507191815  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160507191815  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145129  IP                  G�O�G�O�G�O�                