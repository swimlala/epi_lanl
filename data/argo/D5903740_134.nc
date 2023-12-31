CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-28T10:16:09Z AOML 3.0 creation; 2016-06-01T00:08:27Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151128101609  20160531170828  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_134                   2C  D   APEX                            5374                            041511                          846 @ׂ
ޠ�1   @ׂ{�"@;|�hs�c���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dys3D��D�L�D�s3D�ٚD�fD�C3D�� D�� D� D�9�D�p D�ɚD���D�9�D�vfD���D��D�I�D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��GB��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
CJW
CLW
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
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA)DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtu�Dy��D�'�D�W�D�~D��{D�!GD�ND���D���D��D�D{D�z�D��{D��D�D{DځGD��D��D�T{D��D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�{A�$�A��A��A� �A�$�A��A�bA�VA�JA�1A�%A�%A�A���A���A���A���A���A���A��A��yA��;A���A�ƨAĺ^AĬAģ�Aě�AēuAąA�x�A�t�A�r�A�\)A�?}A��A��A��-A�JA��!A�S�A�(�A��RA��A��7A� �A���A��A��A��A��hA��A�l�A��A��A���A��\A�9XA��A��A��A��^A���A���A�A�A�z�A�  A�x�A�1A���A��yA���A� �A��A��A�{A�XA���A�n�A���A��A��`A���A���A��^A�|�A�`BA���A�x�A�hsA�A�A�A��A��/A�E�A���A��A���A��PA�z�A�^5A�XA�l�A���A�&�A�A�A��!A��AO�A|I�Ay�wAw�wAv�DAu`BAs�mArZAp��Ap �Ao�AoO�An�\Am��Al(�Ai�Ag��Afv�Ae�AeoAd�`Ad�9AdAbJA`$�A^Q�A]oAZ�yAYƨAXĜAV�yAV{AU
=AS��AR  APA�ANAL$�AKdZAJZAIl�AHĜAH=qAG��AF�/AF(�AD��AD9XAC33AB��AB5?AA��A@�`A@n�A?��A??}A>�/A>ffA>bA=�-A<��A<^5A<A�A:�`A:M�A9��A8z�A7��A7K�A6�+A5�A5�PA4�A4I�A3�A2�A1l�A0�9A0�\A0v�A0JA/�A.��A-��A,~�A+VA*=qA)�hA(�HA(=qA'�A'��A&�RA&bA%C�A$��A${A#�hA#"�A"(�A!p�A ��A �jA 5?A�Ax�AbNA1'A�A{A��A%AC�A�A�#A33AI�A�;AhsA�A�9A��At�A�A�^A�A\)A&�A
�A
�+A
A�A	&�A�7A�A��AK�A�\A��A�uAE�A�A��A�7A ��@�ƨ@��@���@��D@�A�@���@��R@��7@���@�Q�@�1@�t�@�
=@�-@��/@�F@�@�u@��m@�o@�V@�&�@�@�@���@�ƨ@⟾@�J@�G�@߅@���@�l�@��y@�ff@�&�@�  @��#@�1@�?}@�+@͙�@�|�@ɑh@���@�j@ǍP@���@�@�z�@�"�@�-@��@��
@�"�@�n�@��h@��/@��
@�C�@��H@��!@���@��\@�J@���@�dZ@�o@��@�ff@�5?@�$�@�r�@�{@��j@�b@���@�$�@��#@���@�hs@�7L@���@��9@�A�@���@��\@�@��-@�G�@���@�t�@���@�E�@�5?@��@��7@�7L@��@�V@���@��@���@���@�A�@��@���@���@�5?@�@��@�A�@�A�@��w@��H@�{@�x�@�?}@���@��@�Z@�(�@��@��@��w@�S�@�@��!@�ff@�J@���@�Ĝ@��j@��9@��@��@���@��D@��@�J@���@���@�X@���@��@�|�@�l�@�K�@�ȴ@���@���@���@��!@���@�ff@��T@��-@�O�@�z�@��
@�t�@�S�@�;d@�
=@���@�{@���@�x�@�hs@�7L@���@�Q�@�1@��m@�C�@��@���@���@���@�E�@��@��@��+@��R@���@��@�p�@�?}@�/@�%@��@���@��@�t�@��P@���@��F@��F@��@���@���@�C�@�o@�
=@�@�@��@���@��\@�n�@�=q@�$�@��@�hs@�%@���@���@��@��`@���@���@���@���@��D@�z�@�z�@�r�@�(�@�b@�  @��@�+@�"�@�@���@�  @u�-@lZ@f�+@_��@ZM�@R�@Kƨ@G
=@?�@2�@0bN@+�
@&V@"��@�@�@{@33@|�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�{A�$�A��A��A� �A�$�A��A�bA�VA�JA�1A�%A�%A�A���A���A���A���A���A���A��A��yA��;A���A�ƨAĺ^AĬAģ�Aě�AēuAąA�x�A�t�A�r�A�\)A�?}A��A��A��-A�JA��!A�S�A�(�A��RA��A��7A� �A���A��A��A��A��hA��A�l�A��A��A���A��\A�9XA��A��A��A��^A���A���A�A�A�z�A�  A�x�A�1A���A��yA���A� �A��A��A�{A�XA���A�n�A���A��A��`A���A���A��^A�|�A�`BA���A�x�A�hsA�A�A�A��A��/A�E�A���A��A���A��PA�z�A�^5A�XA�l�A���A�&�A�A�A��!A��AO�A|I�Ay�wAw�wAv�DAu`BAs�mArZAp��Ap �Ao�AoO�An�\Am��Al(�Ai�Ag��Afv�Ae�AeoAd�`Ad�9AdAbJA`$�A^Q�A]oAZ�yAYƨAXĜAV�yAV{AU
=AS��AR  APA�ANAL$�AKdZAJZAIl�AHĜAH=qAG��AF�/AF(�AD��AD9XAC33AB��AB5?AA��A@�`A@n�A?��A??}A>�/A>ffA>bA=�-A<��A<^5A<A�A:�`A:M�A9��A8z�A7��A7K�A6�+A5�A5�PA4�A4I�A3�A2�A1l�A0�9A0�\A0v�A0JA/�A.��A-��A,~�A+VA*=qA)�hA(�HA(=qA'�A'��A&�RA&bA%C�A$��A${A#�hA#"�A"(�A!p�A ��A �jA 5?A�Ax�AbNA1'A�A{A��A%AC�A�A�#A33AI�A�;AhsA�A�9A��At�A�A�^A�A\)A&�A
�A
�+A
A�A	&�A�7A�A��AK�A�\A��A�uAE�A�A��A�7A ��@�ƨ@��@���@��D@�A�@���@��R@��7@���@�Q�@�1@�t�@�
=@�-@��/@�F@�@�u@��m@�o@�V@�&�@�@�@���@�ƨ@⟾@�J@�G�@߅@���@�l�@��y@�ff@�&�@�  @��#@�1@�?}@�+@͙�@�|�@ɑh@���@�j@ǍP@���@�@�z�@�"�@�-@��@��
@�"�@�n�@��h@��/@��
@�C�@��H@��!@���@��\@�J@���@�dZ@�o@��@�ff@�5?@�$�@�r�@�{@��j@�b@���@�$�@��#@���@�hs@�7L@���@��9@�A�@���@��\@�@��-@�G�@���@�t�@���@�E�@�5?@��@��7@�7L@��@�V@���@��@���@���@�A�@��@���@���@�5?@�@��@�A�@�A�@��w@��H@�{@�x�@�?}@���@��@�Z@�(�@��@��@��w@�S�@�@��!@�ff@�J@���@�Ĝ@��j@��9@��@��@���@��D@��@�J@���@���@�X@���@��@�|�@�l�@�K�@�ȴ@���@���@���@��!@���@�ff@��T@��-@�O�@�z�@��
@�t�@�S�@�;d@�
=@���@�{@���@�x�@�hs@�7L@���@�Q�@�1@��m@�C�@��@���@���@���@�E�@��@��@��+@��R@���@��@�p�@�?}@�/@�%@��@���@��@�t�@��P@���@��F@��F@��@���@���@�C�@�o@�
=@�@�@��@���@��\@�n�@�=q@�$�@��@�hs@�%@���@���@��@��`@���@���@���@���@��D@�z�@�z�@�r�@�(�@�b@�  @��@�+@�"�@�@���@�  @u�-@lZ@f�+@_��@ZM�@R�@Kƨ@G
=@?�@2�@0bN@+�
@&V@"��@�@�@{@33@|�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B
=B	7B	7B	7B
=BbB�B�B�B�B�B�B �B �B"�B#�B%�B'�B)�B-B1'B49B7LB9XB;dB=qB?}B@�BA�BB�BC�BC�BC�BB�BA�B@�BD�BD�B@�B;dB2-B+B�B�B�B�B�B �B �B �B�B�B�BuBbBPBDB1BB  B��B�B�HB�B��B��BƨBÖB�}B�^B�?B��B�+Bm�BjB\)BG�B9XB+B�B��B�fB�TB��BĜB�qB�RB�RB�XB�?B�?B�-B�B�B��B��B�JBr�BXB>wB/B)�B�B
�B
�)B
��B
��B
��B
��B
�JB
p�B
`BB
YB
T�B
P�B
H�B
>wB
0!B
,B
,B
&�B
�B
�B
B	�mB	�/B	��B	ƨB	��B	��B	��B	�^B	��B	��B	�%B	z�B	o�B	iyB	cTB	XB	Q�B	L�B	E�B	;dB	1'B	&�B	�B	�B	�B	hB	\B	JB	
=B	1B	%B	B	B	B	B��B��B��B	  B	B	%B	+B	%B	%B	%B	B	B	B��B��B��B��B�B�B�B�sB�fB�NB�5B�#B�B��B��B��B��B��B��BɺBŢB�}B�^B�RB�FB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B�{B�oB�VB�%B}�B|�B|�B|�B{�By�Bu�Bo�BhsBaHB]/B\)BZBXBVBT�BS�BQ�BR�BR�BS�BR�BQ�BP�BO�BM�BK�BI�BG�BF�BE�BC�BA�B@�B?}B?}B>wB=qB;dB9XB8RB6FB6FB5?B49B33B2-B1'B0!B0!B0!B/B.B-B,B)�B+B)�B)�B(�B&�B%�B#�B"�B!�B!�B �B�B�B�B�B�B �B �B�B �B�B�B�B�B!�B#�B#�B#�B$�B$�B%�B&�B'�B(�B+B-B.B/B1'B2-B33B49B5?B5?B5?B49B49B:^B;dB<jB<jB=qB>wB=qB@�BE�BJ�BL�BT�BZB[#B\)B]/B]/B^5B_;B`BBaHBcTBdZBe`BiyBp�Bx�B}�B� B� B�B�%B�1B�1B�7B�7B�7B�=B�=B�JB�VB�bB�{B��B��B��B��B��B��B�B�B�3B�3B�9B�?B�FB�LB�LB�LB�LB�RB�XB�^B�jB��B��B��B��B��B��B��B��B��B��B�BB�HB�NB�TB�TB�fB�mB�mB�mB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	+B	
=B	DB	JB	\B	uB	�B	�B	�B	#�B	&�B	&�B	)�B	.B	49B	:^B	=qB	?}B	A�B	G�B	L�B	P�B	P�B	R�B	S�B	VB	W
B	YB	[#B	]/B	^5B	_;B	`BB	`BB	`BB	dZB	gmB	hsB	hsB	hsB	iyB	l�B	n�B	n�B	p�B	q�B	s�B	v�B	y�B	y�B	z�B	z�B	{�B	|�B	|�B	}�B	~�B	~�B	~�B	� B	� B	�B	�B	�B	�%B	�1B	�7B	�7B	�VB	��B	�jB	�B	�sB	��B
  B
hB
�B
�B
(�B
2-B
<jB
C�B
H�B
M�B
S�B
\)B
^5B
cTB
hsB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
!B
!B	B	B	B
#BFBiB�B�B�B�B�B �B �B"�B#�B%�B'�B)�B,�B1B4!B75B9AB;HB=XB?eB@lBArBBwBC~BC{BC{BBvBAnB@lBDBD�B@gB;MB2B*�B�BsB�B�B�B �B �B �B�B�BfBZBFB5B)BBB��B��B�iB�,B��B��BλBƉB�sB�`B�AB�!B��B�BmpBj^B\BG�B95B*�B_B��B�HB�3B��B�yB�QB�1B�/B�6B� B� B�B��B��B��B��B�*Br�BW�B>VB.�B)�BmB
�aB
�B
��B
�iB
��B
�bB
�/B
p�B
`&B
X�B
T�B
P�B
H�B
>ZB
0B
+�B
+�B
&�B
�B
lB
�B	�RB	�B	��B	ƎB	�nB	�jB	�lB	�DB	��B	�hB	�B	z�B	o�B	i^B	c<B	W�B	Q�B	L�B	E�B	;NB	1B	&�B	�B	�B	kB	TB	FB	6B	
'B	B	B		B	B	�B	 �B��B��B��B��B	�B	B	B	B	B	B	
B	�B	 �B��B��B��B��B�B�B�rB�`B�PB�;B�!B�B��B��B��B��B��B��B˳BɧBōB�hB�JB�>B�4B�'B�B�B�
B��B��B��B��B��B��B��B��B�vB�jB�\B�EB�B}�B|�B|�B|�B{�By�Bu�Bo�BhcBa8B]B\BZBX BU�BT�BS�BQ�BR�BR�BS�BR�BQ�BP�BO�BM�BK�BI�BG�BF�BE�BC�BAxB@sB?kB?mB>gB=bB;TB9IB8@B63B65B50B4)B3"B2B1B0B0B/�B.�B.B,�B+�B)�B*�B)�B)�B(�B&�B%�B#�B"�B!�B!�B �B�B�B�B�B�B �B �B�B �B�B�B�B�B!�B#�B#�B#�B$�B$�B%�B&�B'�B(�B*�B,�B-�B/B1B2B3"B4'B5+B5-B5(B4'B4'B:IB;PB<UB<VB=_B>eB=_B@mBE�BJ�BL�BT�BZB[B\B]B]B^B_'B`-Ba4Bc>BdABeIBiaBp�Bx�B}�B�B�B��B�B�B�B�B�!B�B�$B�$B�/B�>B�IB�cB�nB�zB��B��B��B��B��B� B�B�B� B�%B�-B�1B�1B�0B�2B�7B�>B�CB�PB�hBʦBͶB͸B͸BͷBνBξBξB��B�'B�,B�3B�9B�8B�LB�QB�QB�QB�aB�iB�iB�jB�jB�iB�hB�sB�|B�B�B��B��B��B��B��B��B	�B	B	
B	'B	+B	=B	UB	gB	hB	�B	#�B	&�B	&�B	)�B	-�B	4B	:@B	=OB	?[B	AiB	G�B	L�B	P�B	P�B	R�B	S�B	U�B	V�B	X�B	[B	]B	^B	_B	` B	` B	` B	d8B	gLB	hSB	hSB	hSB	iUB	liB	nwB	nvB	p�B	q�B	s�B	v�B	y�B	y�B	z�B	z�B	{�B	|�B	|�B	}�B	~�B	~�B	~�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�5B	�|B	�DB	��B	�OB	��B	��B
AB
mB
�B
(�B
2B
<BB
CqB
H�B
M�B
S�B
[�B
^B
c,B
hLB
k^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708282016053117082820160531170828  AO  ARCAADJP                                                                    20151128101609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151128101609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151128101609  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170828  IP                  G�O�G�O�G�O�                