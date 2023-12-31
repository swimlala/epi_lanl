CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:49Z AOML 3.0 creation; 2016-06-01T00:08:19Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230849  20160531170819  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               UA   AO  4055_7112_085                   2C  D   APEX                            5374                            041511                          846 @���{�1   @���� @;^5?|��dXbM�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    UA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D���D�)�D���D���D�� D�VfD��fD�� D�	�D�0 D��fD�� D��D�C3DږfD���D��D�C3D�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)BB\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8)D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY]DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy�]D�{D�4{D��{D�׮D���D�aGD��GD���D�{D�:�D��GD���D�${D�NDڡGD�׮D��D�ND�~D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AС�A�oA�z�A�JA�I�A��A�
=A�1A�A���A�Q�Aə�A�p�A�r�A�ȴAð!A��A�K�A��A��7A�|�A��A�A��\A��`A���A�=qA��A���A��A�bNA��A�O�A�ĜA�7LA���A���A��A���A�A�-A���A��DA���A�&�A�x�A�;dA�(�A��^A��A��A�XA��A���A�|�A�K�A���A��DA�/A�A���A���A�~�A�S�A��A��9A�t�A�9XA��`A��PA�dZA�;dA�$�A�$�A�JA��wA�~�A�A�A��;A���A�~�A�Q�A��A��#A�~�A�C�A�;dA�JA���A�jA��A���A��TA��jA�hsA��#A�~�A� �A�ƨA���A�z�A�n�A��A��!A�C�A�VA�=qA��
A��hA�~�A�hsA��A�7LA��A��-A��hA���A�~�A��A�=qA�~�A�C�A�"�A�hsA�x�A��A�%A���A&�A}��A|�9A|JAx��Aw�Au��Au&�AsS�Ar~�Ap��An�yAlv�AjȴAj{AiƨAidZAg�AgAd��Ab��AaS�A^��A\�DAZ�AY`BAX{AV��AU��AT��AT1'ATbAT  AS�AS�TASK�AQ�7APv�AO\)AL�AKƨAJbNAIx�AHjAGS�AFQ�ADQ�A?�A=��A;XA:�A9x�A7A5"�A3��A3C�A1��A1%A0�+A0JA/p�A.�`A.��A.��A. �A-�A,�jA,�+A+��A*A(=qA'S�A&�yA&I�A%�7A%?}A$�`A$��A$  A#A#%A"(�A�FA��A��A�-A|�A\)AC�A7LA+A��A��A�+Az�AE�A��AO�Ar�A��A+A"�A��AbNA`BAE�A��A\)A%A
ffA	A	x�A��A�;A�RA�#An�A�Ax�A�RA�\A  A|�A ��A ��A $�@�
=@�5?@�%@�A�@��@���@���@���@�ȴ@�G�@�9X@�@��y@���@�J@��@���@��/@�@��H@�bN@��@�`B@�j@��@�C�@���@�-@�7L@��
@�1'@���@��@�v�@�E�@��T@ٙ�@�?}@ش9@��m@ָR@ՙ�@�@��@�C�@�-@��@ͩ�@ͩ�@͑h@�`B@�/@���@�  @���@�  @�33@��@Ɵ�@��@�S�@��H@\@�5?@�@���@�S�@��@��9@�;d@��@�@��@��H@�E�@���@���@���@�x�@��@�%@��/@���@���@��u@�bN@�9X@�b@�ƨ@�t�@��!@�=q@���@��@���@��;@�K�@���@�~�@�ff@�=q@��-@��`@�|�@�E�@��-@�x�@���@�(�@��F@�l�@���@�^5@��#@�`B@�j@���@�;d@���@��T@���@��@��9@�ƨ@�K�@��@���@�V@��@�X@�Z@� �@��m@��w@���@�t�@�\)@�S�@�S�@�S�@�S�@���@���@��@��@�Z@��
@�K�@�@���@�b@��m@��
@�|�@�33@�
=@���@�v�@�V@�5?@��@���@��-@��@�X@�V@���@�bN@���@�S�@���@���@�V@��@���@�bN@�Q�@�I�@�(�@���@��R@�E�@�@��@��@���@��#@��@���@���@�bN@�b@�1@�1@��@��F@�|�@�@��\@�^5@�M�@�V@�E�@�$�@��T@���@���@�`B@�G�@�%@��9@�bN@�1@��F@�S�@�+@�o@�@���@�ȴ@�=q@�J@��@�@�X@���@��@�Z@�w@
=@
=@~�y@~ȴ@~�+@~E�@x�@mO�@gK�@\��@Tj@N�+@HbN@Ahs@:-@4��@/+@+C�@%�h@!�#@9X@ �@�F@��@o@�u@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AС�A�oA�z�A�JA�I�A��A�
=A�1A�A���A�Q�Aə�A�p�A�r�A�ȴAð!A��A�K�A��A��7A�|�A��A�A��\A��`A���A�=qA��A���A��A�bNA��A�O�A�ĜA�7LA���A���A��A���A�A�-A���A��DA���A�&�A�x�A�;dA�(�A��^A��A��A�XA��A���A�|�A�K�A���A��DA�/A�A���A���A�~�A�S�A��A��9A�t�A�9XA��`A��PA�dZA�;dA�$�A�$�A�JA��wA�~�A�A�A��;A���A�~�A�Q�A��A��#A�~�A�C�A�;dA�JA���A�jA��A���A��TA��jA�hsA��#A�~�A� �A�ƨA���A�z�A�n�A��A��!A�C�A�VA�=qA��
A��hA�~�A�hsA��A�7LA��A��-A��hA���A�~�A��A�=qA�~�A�C�A�"�A�hsA�x�A��A�%A���A&�A}��A|�9A|JAx��Aw�Au��Au&�AsS�Ar~�Ap��An�yAlv�AjȴAj{AiƨAidZAg�AgAd��Ab��AaS�A^��A\�DAZ�AY`BAX{AV��AU��AT��AT1'ATbAT  AS�AS�TASK�AQ�7APv�AO\)AL�AKƨAJbNAIx�AHjAGS�AFQ�ADQ�A?�A=��A;XA:�A9x�A7A5"�A3��A3C�A1��A1%A0�+A0JA/p�A.�`A.��A.��A. �A-�A,�jA,�+A+��A*A(=qA'S�A&�yA&I�A%�7A%?}A$�`A$��A$  A#A#%A"(�A�FA��A��A�-A|�A\)AC�A7LA+A��A��A�+Az�AE�A��AO�Ar�A��A+A"�A��AbNA`BAE�A��A\)A%A
ffA	A	x�A��A�;A�RA�#An�A�Ax�A�RA�\A  A|�A ��A ��A $�@�
=@�5?@�%@�A�@��@���@���@���@�ȴ@�G�@�9X@�@��y@���@�J@��@���@��/@�@��H@�bN@��@�`B@�j@��@�C�@���@�-@�7L@��
@�1'@���@��@�v�@�E�@��T@ٙ�@�?}@ش9@��m@ָR@ՙ�@�@��@�C�@�-@��@ͩ�@ͩ�@͑h@�`B@�/@���@�  @���@�  @�33@��@Ɵ�@��@�S�@��H@\@�5?@�@���@�S�@��@��9@�;d@��@�@��@��H@�E�@���@���@���@�x�@��@�%@��/@���@���@��u@�bN@�9X@�b@�ƨ@�t�@��!@�=q@���@��@���@��;@�K�@���@�~�@�ff@�=q@��-@��`@�|�@�E�@��-@�x�@���@�(�@��F@�l�@���@�^5@��#@�`B@�j@���@�;d@���@��T@���@��@��9@�ƨ@�K�@��@���@�V@��@�X@�Z@� �@��m@��w@���@�t�@�\)@�S�@�S�@�S�@�S�@���@���@��@��@�Z@��
@�K�@�@���@�b@��m@��
@�|�@�33@�
=@���@�v�@�V@�5?@��@���@��-@��@�X@�V@���@�bN@���@�S�@���@���@�V@��@���@�bN@�Q�@�I�@�(�@���@��R@�E�@�@��@��@���@��#@��@���@���@�bN@�b@�1@�1@��@��F@�|�@�@��\@�^5@�M�@�V@�E�@�$�@��T@���@���@�`B@�G�@�%@��9@�bN@�1@��F@�S�@�+@�o@�@���@�ȴ@�=q@�J@��@�@�X@���@��@�Z@�w@
=@
=@~�y@~ȴ@~�+@~E�@x�@mO�@gK�@\��@Tj@N�+@HbN@Ahs@:-@4��@/+@+C�@%�h@!�#@9X@ �@�F@��@o@�u@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBM�BK�BG�BD�B;dB8RB6FB6FB5?B2-B"�BDBB��B��B�fB��B��BB�LB�B��B��B�%Bs�BffBXBD�B6FB&�B�B�B�B�BoB\BVBDB1BB��B��B��B�B�B�mB�B�B�mB�B��B��B��B��BɺBŢBB�qB�XB�LB�?B�3B�-B�B�B��B��B��B��B��B��B�uB�{B��B�{B�hB�VB�=B�B�B}�Bz�Bv�Bq�Bk�BgmBgmBcTB\)BXBS�BQ�BP�BN�BE�B6FB+B"�B�B�BoBbB1B��B��B�B�)B��B��B��BǮB�}B�B��B��B��B�%B~�BiyBK�B(�BbB
��B
�B
�
B
ɺB
��B
�PB
w�B
ffB
[#B
Q�B
33B
"�B
�B
hB
B	��B	�B	�/B	��B	B	�jB	�RB	�9B	��B	��B	�hB	�B	w�B	ffB	\)B	S�B	N�B	I�B	B�B	=qB	;dB	:^B	9XB	9XB	8RB	7LB	33B	,B	'�B	!�B	�B	�B	bB	PB		7B	%B	B��B�`B�5B�B�B��B��BÖB��B�qB�XB�LB�FB�9B�3B�-B�'B�!B�B�B�B��B��B��B��B��B��B�{B�oB�hB�bB�\B�PB�DB�1B�B~�Bz�By�Bx�Bx�Bw�Bw�Bw�Bv�Bu�Bt�Bt�Br�Bo�BhsB_;B[#BYBT�BN�BJ�BG�BE�BB�B@�B?}B>wB=qB;dB:^B8RB5?B33B0!B/B-B,B+B)�B(�B'�B&�B%�B$�B#�B"�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBoBhB\BPBPBJB	7B	7B1BDBDBDBDBDB
=B
=B	7B1B1BDB
=BVBbBhBhBhBhBhBoBhBhB{B�B�B�B�B�B�B�B�B�B�B#�B#�B#�B'�B,B,B.B2-B:^B>wB@�BA�BB�BD�BG�BH�BI�BJ�BI�BK�BK�BL�BL�BL�BL�BL�BL�BN�BO�BQ�BT�BXBZB[#B\)B\)B^5B`BBe`BhsBk�Bk�Bl�Bo�Bq�Br�Bu�Bv�Bx�Bz�B� B�B�%B�1B�JB�PB�PB�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�9B�^B�jB�jB�wB��BÖB��B�#B�)B�/B�5B�BB�HB�NB�ZB�`B�fB�fB�mB�yB�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	B	DB	{B	�B	�B	�B	�B	 �B	#�B	&�B	(�B	.B	1'B	5?B	5?B	5?B	6FB	7LB	8RB	9XB	;dB	<jB	<jB	<jB	=qB	?}B	B�B	C�B	D�B	H�B	I�B	K�B	L�B	M�B	O�B	P�B	T�B	VB	VB	VB	VB	W
B	\)B	^5B	_;B	`BB	dZB	hsB	k�B	n�B	r�B	t�B	t�B	u�B	u�B	u�B	w�B	}�B	��B	ŢB	�BB	��B
+B
�B
"�B
,B
33B
:^B
@�B
H�B
M�B
T�B
ZB
`BB
ffB
k�B
o�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BM�BK�BG�BD�B;QB8>B63B63B5+B2B"�B1B�B��B��B�SB��BʧB�yB�2B��B��B�uB�Bs�BfHBW�BD�B6+B&�B�B�BvBcBRB?B9B'BB�B��B��B��B�B�fB�PB�cB�bB�QB��B��B��B��BϿBɛBŃB�qB�TB�8B�.B�#B�B�B��B��B��B��B��B��B�nB�`B�WB�]B�bB�ZB�GB�6B�B��B��B}�Bz�Bv�Bq�BkeBgOBgNBc4B\BW�BS�BQ�BP�BN�BE�B6%B*�B"�B~B`BOB@BB��B��B�nB�B��B̫BʢBǎB�_B��B��B��B�vB�B~�BiWBK�B(�BCB
��B
�kB
��B
əB
��B
�4B
w�B
fIB
[B
Q�B
3B
"�B
sB
MB
B	��B	�vB	�B	̰B	�wB	�OB	�8B	�B	��B	��B	�QB	��B	w�B	fNB	\B	S�B	N�B	I�B	ByB	=[B	;OB	:IB	9AB	9BB	8<B	77B	3B	+�B	'�B	!�B	�B	mB	MB	<B		#B	B	 �B��B�NB�$B�B��B��BʯBÅB�oB�^B�CB�8B�4B�%B� B�B�B�B�B��B��B��B��B��B��B��B�vB�kB�\B�WB�SB�KB�>B�2B�B�B~�Bz�By�Bx�Bx�Bw�Bw�Bw�Bv�Bu�Bt�Bt�Br�Bo�BhcB_)B[BY	BT�BN�BJ�BG�BE�BB~B@uB?oB>gB=dB;TB:NB8CB5.B3
B0B/B,�B+�B*�B)�B(�B'�B&�B%�B$�B#�B"�B!�B �B �B �B�B�B�B�B�B�B~B|B}BuBuB�B�B]BQBIBeB^B?BLB'B%BB	B	'B	B3BB1B4B5B
B
B	BBBB
B,BPB=B=B;B<B=B_B=BYBOB�B�B�B�B�B�B�B�B�B�B#�B#�B#�B'�B+�B+�B.B2B:JB>dB@pBAvBByBD�BG�BH�BI�BJ�BI�BK�BK�BL�BL�BL�BL�BL�BL�BN�BO�BQ�BT�BW�BZB[B\B\B^B`-BeKBh]BkoBknBluBo�Bq�Br�Bu�Bv�Bx�Bz�B�B��B�B�B�3B�8B�6B�MB�gB�yB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�CB�MB�NB�\B�nB�{BͷB�B�B�B�B�&B�,B�1B�?B�BB�IB�IB�OB�^B�bB�iB�hB�lB�{B�B�B��B��B��B	 �B	�B	�B	�B	�B	 B	%B	_B	|B	�B	�B	�B	 �B	#�B	&�B	(�B	-�B	1B	5B	5B	5B	6%B	7+B	80B	97B	;EB	<JB	<HB	<JB	=PB	?[B	BnB	CwB	D|B	H�B	I�B	K�B	L�B	M�B	O�B	P�B	T�B	U�B	U�B	U�B	U�B	V�B	\B	^B	_B	`"B	d7B	hTB	kdB	nvB	r�B	t�B	t�B	u�B	u�B	u�B	w�B	}�B	��B	�{B	�B	��B
B
XB
"�B
+�B
3B
:5B
@^B
H�B
M�B
T�B
Y�B
`B
f;B
k]B
otB
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708192016053117081920160531170819  AO  ARCAADJP                                                                    20140721230849    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230849  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230849  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170819  IP                  G�O�G�O�G�O�                