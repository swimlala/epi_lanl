CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-27T19:16:42Z AOML 3.0 creation; 2016-06-01T00:08:25Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150627191642  20160531170825  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               wA   AO  4055_7112_119                   2C  D   APEX                            5374                            041511                          846 @�[�� 1   @�[���
@;r� Ĝ�dGI�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    wA   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD�3D�L�D��3D��3D���D�I�D��fD�� D�  D�P D��3D��3D�	�D�)�Dډ�D��3D�	�D�@ D�y�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��HAp�A%p�AEp�Aep�A��A��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)ByB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�z�B�B�B��B��B��C W
CW
CW
CW
CW
C
W
CW
CW
CW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CHW
CJp�CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D�]D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�]D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�)Dy�)D�D�W�D��D��D�{D�T{D��GD���D�
�D�Z�D��D��D�{D�4{Dڔ{D��D�{D�J�D�{D�'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AÃA�K�A�-A�VA��;A�ƨA§�A� �A��9A��PA��A�(�A���A�\)A�A���A�\)A�33A�A���A��\A���A�hsA�  A���A�-A��
A�K�A��A���A�Q�A��A��wA��RA��A� �A�G�A�r�A���A��hA�/A���A�(�A��TA��HA��hA�v�A�  A��A�E�A�
=A�JA�/A�"�A��-A�E�A�ffA���A�7LA��wA�bNA�-A�oA��mA��mA���A���A�5?A�"�A�K�A���A�(�A�O�A�ȴA�ffA���A��\A��A�(�A�?}A��!A��uA�|�A�bNA�A�A�"�A���A��^A�XA��`A���A�A�A�bA��hA�  A��FA��DA�l�A�"�A��9A�ȴA�ZA��A��A��DA�%A��FA��DA�$�A��#A���A��HA�Q�A�A��TA���A��9A�O�A���A� �A��PA�A��^A�"�A~��A}G�A|E�Az��Ay��AxffAwO�AvQ�Au|�At �AsK�Ar�Aq
=An��AnZAl~�AkS�Aj�Ai��Ai�AhȴAg��Af��Aex�AdJAc;dAb�DAbA�Ab{AaƨAa��Aat�A`ffA]/A[�AZĜAY�;AY��AY`BAY&�AXĜAX��AXz�AXVAX�AW�hAW�AVn�ATbNAPQ�AOS�AN�+ANA�AN �AM�AMƨAM��ALjAK�PAK�AKp�AK&�AHQ�AE�ADE�ABA�AA`BA@��A@jA?�A>�A<�`A<z�A;�A:ZA9��A9A8$�A6��A6v�A5�;A4(�A2bA/�-A.A�A-�FA-�A+��A*A�A'��A&��A&�DA&�A&z�A&r�A&bNA&{A%x�A%"�A$�9A$z�A$1A"�RA!XA �9A �A�TA;dA�!A�7Ar�A  A\)AA$�A�hA�AffA`BA  A�;A�A��A�PAĜA�!A��AQ�A�wAoAVAE�A�A"�A	XAM�A(�A�A��A�-A�hA`BAĜA��A��A�AhsA�RAE�A��A r�A 9X@��@�E�@��9@�C�@��j@�ȴ@�`B@���@��@��@���@�I�@�b@�|�@��@�hs@�S�@��@��/@�P@���@�M�@�9@��m@��@�|�@�~�@���@��/@� �@�C�@�r�@�n�@���@˕�@ʇ+@ɉ7@ț�@�ƨ@�~�@Ĭ@�;d@��H@\@�-@��T@��/@��@�t�@��@�$�@�J@��@��7@�G�@�&�@��@��@��F@��H@��@�j@�bN@�Z@�I�@�A�@��m@�|�@��H@�=q@���@�/@�z�@��
@��P@��y@�hs@�9X@���@��w@���@�@�%@���@�
=@��R@��@�x�@���@��@�j@��
@�dZ@�C�@�;d@�;d@�33@�"�@���@���@�b@�+@�n�@�$�@�Ĝ@�9X@��m@�|�@��!@�ff@�@���@���@�1@�ƨ@��@���@�X@�&�@��@��w@�t�@�;d@��!@��@��D@�z�@�bN@�9X@��@��@�dZ@�l�@�dZ@�K�@�33@��@���@�5?@���@�%@��9@��@�l�@�S�@�C�@�+@��@�@�@�
=@�
=@���@��y@��@���@�J@���@��@�X@�O�@�G�@��@��9@�r�@�  @�
=@��H@���@�~�@�v�@�ff@�E�@�{@��7@���@���@��@�j@�Z@�9X@��m@�ƨ@��@�l�@��@���@��R@���@��+@�V@�{@���@�X@��j@�1'@l�@~��@~E�@}�@}@}�h@}V@{t�@z�\@y��@y��@yx�@y%@xb@w
=@v�+@v$�@r�H@hĜ@_�P@Y7L@Q�@O
=@H�9@B�!@;S�@7|�@2=q@,I�@&ff@!%@��@�@G�@�@S�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AÃA�K�A�-A�VA��;A�ƨA§�A� �A��9A��PA��A�(�A���A�\)A�A���A�\)A�33A�A���A��\A���A�hsA�  A���A�-A��
A�K�A��A���A�Q�A��A��wA��RA��A� �A�G�A�r�A���A��hA�/A���A�(�A��TA��HA��hA�v�A�  A��A�E�A�
=A�JA�/A�"�A��-A�E�A�ffA���A�7LA��wA�bNA�-A�oA��mA��mA���A���A�5?A�"�A�K�A���A�(�A�O�A�ȴA�ffA���A��\A��A�(�A�?}A��!A��uA�|�A�bNA�A�A�"�A���A��^A�XA��`A���A�A�A�bA��hA�  A��FA��DA�l�A�"�A��9A�ȴA�ZA��A��A��DA�%A��FA��DA�$�A��#A���A��HA�Q�A�A��TA���A��9A�O�A���A� �A��PA�A��^A�"�A~��A}G�A|E�Az��Ay��AxffAwO�AvQ�Au|�At �AsK�Ar�Aq
=An��AnZAl~�AkS�Aj�Ai��Ai�AhȴAg��Af��Aex�AdJAc;dAb�DAbA�Ab{AaƨAa��Aat�A`ffA]/A[�AZĜAY�;AY��AY`BAY&�AXĜAX��AXz�AXVAX�AW�hAW�AVn�ATbNAPQ�AOS�AN�+ANA�AN �AM�AMƨAM��ALjAK�PAK�AKp�AK&�AHQ�AE�ADE�ABA�AA`BA@��A@jA?�A>�A<�`A<z�A;�A:ZA9��A9A8$�A6��A6v�A5�;A4(�A2bA/�-A.A�A-�FA-�A+��A*A�A'��A&��A&�DA&�A&z�A&r�A&bNA&{A%x�A%"�A$�9A$z�A$1A"�RA!XA �9A �A�TA;dA�!A�7Ar�A  A\)AA$�A�hA�AffA`BA  A�;A�A��A�PAĜA�!A��AQ�A�wAoAVAE�A�A"�A	XAM�A(�A�A��A�-A�hA`BAĜA��A��A�AhsA�RAE�A��A r�A 9X@��@�E�@��9@�C�@��j@�ȴ@�`B@���@��@��@���@�I�@�b@�|�@��@�hs@�S�@��@��/@�P@���@�M�@�9@��m@��@�|�@�~�@���@��/@� �@�C�@�r�@�n�@���@˕�@ʇ+@ɉ7@ț�@�ƨ@�~�@Ĭ@�;d@��H@\@�-@��T@��/@��@�t�@��@�$�@�J@��@��7@�G�@�&�@��@��@��F@��H@��@�j@�bN@�Z@�I�@�A�@��m@�|�@��H@�=q@���@�/@�z�@��
@��P@��y@�hs@�9X@���@��w@���@�@�%@���@�
=@��R@��@�x�@���@��@�j@��
@�dZ@�C�@�;d@�;d@�33@�"�@���@���@�b@�+@�n�@�$�@�Ĝ@�9X@��m@�|�@��!@�ff@�@���@���@�1@�ƨ@��@���@�X@�&�@��@��w@�t�@�;d@��!@��@��D@�z�@�bN@�9X@��@��@�dZ@�l�@�dZ@�K�@�33@��@���@�5?@���@�%@��9@��@�l�@�S�@�C�@�+@��@�@�@�
=@�
=@���@��y@��@���@�J@���@��@�X@�O�@�G�@��@��9@�r�@�  @�
=@��H@���@�~�@�v�@�ff@�E�@�{@��7@���@���@��@�j@�Z@�9X@��m@�ƨ@��@�l�@��@���@��R@���@��+@�V@�{@���@�X@��j@�1'@l�@~��@~E�@}�@}@}�h@}V@{t�@z�\@y��@y��@yx�@y%@xb@w
=@v�+@v$�@r�H@hĜ@_�P@Y7L@Q�@O
=@H�9@B�!@;S�@7|�@2=q@,I�@&ff@!%@��@�@G�@�@S�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�/B�)B�)B�)B�)B�)B�B��B��B��B�wB�B��B��B��B�oB�VB�B� B~�B{�Bt�Bq�Bp�Bp�Bo�Bm�BiyBdZBbNB`BB_;B\)BXBVBXBT�BN�BO�BQ�BP�BL�BG�BG�BH�BF�B=qB6FB0!B(�B(�B33B"�BoB\B1B��B�B�ZB�B��B�qB��B��B��B��B��B��B�Bq�BhsB[#BJ�B?}B6FB+B&�B$�B�BVBBBB  B��B��B��B��B�B�`B�;B�B��B��B�}B�qB�dB�^B�9B�B��B�PB�B}�By�Bo�BhsBdZB\)BVBO�BA�B7LB+B�B{BuBJBB
��B
�B
�;B
�B
ǮB
�FB
��B
��B
�hB
�B
x�B
m�B
dZB
\)B
O�B
G�B
?}B
0!B
 �B
�B
JB
B	��B	��B	��B	�B	�sB	�HB	�B	��B	ɺB	ŢB	B	��B	�}B	�qB	�^B	�!B	��B	�{B	�PB	�1B	�%B	�B	�B	�B	�B	� B	}�B	{�B	y�B	v�B	p�B	gmB	\)B	W
B	S�B	R�B	Q�B	P�B	O�B	N�B	L�B	M�B	M�B	L�B	H�B	:^B	.B	(�B	!�B	 �B	�B	�B	�B	\B	1B	B��B��B�B�B�yB�TB�BB�#B��BɺBB�}B�qB�XB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�\B�PB�JB�=B�%B�B�B� B}�Bz�Bv�Bo�Bm�BiyBffBe`BbNB_;B[#BZBYBYBW
BVBS�BR�BP�BN�BH�BB�BA�BA�B@�B@�B?}B?}B=qB;dB9XB7LB5?B49B2-B1'B/B.B-B+B(�B'�B%�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B$�B$�B$�B$�B&�B'�B(�B)�B+B+B+B,B,B,B,B+B-B.B33B5?B5?B5?B5?B5?B5?B6FB7LB8RB:^B<jB=qB>wB=qB>wBB�BD�BM�BVB[#B]/B_;BdZBffBgmBjBl�Bn�Bp�Bq�Bs�Bu�Bu�Bv�Bv�Bu�Bu�Bu�B|�B�B�B�7B�7B�oB�{B��B��B��B��B��B��B��B��B��B�B�-B�9B�9B�FB�dB�qB�qB�}BǮB��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�BB�ZB�fB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	%B	JB	PB	\B	bB	bB	bB	hB	oB	�B	�B	�B	�B	�B	�B	 �B	$�B	&�B	'�B	+B	.B	/B	1'B	2-B	33B	49B	5?B	7LB	:^B	=qB	@�B	C�B	D�B	E�B	F�B	F�B	G�B	H�B	L�B	O�B	S�B	VB	VB	VB	ZB	aHB	dZB	gmB	l�B	��B	�B	��B	�;B	�mB	��B
%B
�B
%�B
7LB
?}B
G�B
N�B
VB
]/B
`BB
dZB
hsB
l�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B��B��BʪB�`B�B��B��B��B�VB�=B��B�B~�B{�Bt�Bq�Bp�Bp�Bo�BmwBi^BdBBb2B`%B_"B\BW�BU�BW�BT�BN�BO�BQ�BP�BL�BG�BG�BH�BF�B=QB6(B0B(�B(�B3B"�BRB@BB��B�B�9B��B��B�PB��B��B��B��B��B�aB��Bq�BhQB[BJ�B?^B6&B*�B&�B$�B�B2B�B�B �B��B��B��B��B��B�oB�?B�B��B��BˤB�\B�PB�DB�:B�B��B�pB�/B��B}�By�BoBhQBd8B\BU�BO�BAhB7*B*�BpB\BVB+B�B
��B
�dB
�B
��B
ǏB
�)B
��B
��B
�IB
��B
x�B
mtB
d=B
\B
O�B
G�B
?^B
0B
 �B
|B
/B
B	��B	��B	��B	��B	�WB	�,B	��B	οB	ɡB	ňB	�uB	�nB	�fB	�WB	�DB	�B	��B	�eB	�7B	�B	�B	�B	��B	��B	��B	�B	}�B	{�B	y�B	v�B	p�B	gUB	\B	V�B	S�B	R�B	Q�B	P�B	O�B	N�B	L�B	M�B	M�B	L�B	H�B	:GB	-�B	(�B	!�B	 �B	�B	�B	oB	FB	B	B��B��B�B�B�dB�?B�-B�B��BɧB�}B�kB�^B�DB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�cB�[B�OB�KB�AB�:B�+B�B�B��B�B}�Bz�Bv�Bo�Bm~BijBfVBePBb<B_)B[BZBYBYBV�BU�BS�BR�BP�BN�BH�BB~BAyBAwB@tB@qB?kB?mB=aB;UB9JB7<B50B4(B2B1B/B.B- B*�B(�B'�B%�B#�B"�B �B�B�B�B�B�B�B}B�B�B|B~B\B\B]BOBDBdBwB]B[B[B{B{B\BB�BmB{B�B�B�B�B�B!�B#�B$�B$�B$�B$�B&�B'�B(�B)�B*�B*�B*�B+�B+�B+�B+�B*�B,�B.B3 B5*B5,B5,B5,B5-B5-B61B79B8@B:KB<WB=\B>cB=_B>cBB|BD�BM�BU�B[B]B_'BdDBfOBgXBjgBluBn�Bp�Bq�Bs�Bu�Bu�Bv�Bv�Bu�Bu�Bu�B|�B��B�B�!B�!B�WB�aB�gB�vB��B��B��B��B��B��B��B��B�B�B�B�)B�JB�SB�UB�bBǑB͸B͸B͹BξB��B��B��B��B��B��B��B��B��B�B�%B�>B�IB�mB��B��B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	�B	�B	B	*B	0B	?B	BB	BB	BB	IB	PB	hB	�B	�B	�B	�B	�B	 �B	$�B	&�B	'�B	*�B	-�B	.�B	1B	2B	3B	4B	5B	7,B	:?B	=QB	@cB	CuB	D|B	E�B	F�B	F�B	G�B	H�B	L�B	O�B	S�B	U�B	U�B	U�B	Y�B	a$B	d8B	gKB	liB	�vB	��B	ʚB	�B	�FB	��B
�B
_B
%�B
7"B
?UB
G�B
N�B
U�B
]B
`B
d3B
hJB
ldB
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708252016053117082520160531170825  AO  ARCAADJP                                                                    20150627191642    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150627191642  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150627191642  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170825  IP                  G�O�G�O�G�O�                