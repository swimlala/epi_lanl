CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-08-25T00:01:36Z AOML 3.0 creation; 2016-06-01T00:08:20Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140825000136  20160531170820  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               YA   AO  4055_7112_089                   2C  D   APEX                            5374                            041511                          846 @��O@1   @�Ϛ�P@:.V�u�dfffff1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    YA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyY�D�fD�Y�D���D��fD�3D�P D�� D��fD�3D�9�D�i�D�ٚD� D�9�D�ffD��fD�fD�VfD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B(�B ��B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)BiBq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
Cbp�CdW
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
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/)D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�)Dyo]D�GD�d{D���D��GD�D�Z�D���D��GD�D�D{D�t{D��{D��D�D{D�qGD��GD�GD�aGD�{D�׮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��/A��HA��mA��TA��mA��A��`A��/A��A���A���A�ȴA�A�ƨAڣ�A���A���AԑhA��A�?}AͅA� �Aȴ9A�^5A�M�A��`A�+A�;dA�x�A�;dA��A��A�hsA�$�A��DA�A�K�A�oA�n�A���A���A��FA�v�A�{A���A�oA�ĜA�v�A�;dA�$�A��7A���A�A�A���A�&�A�;dA�ĜA�JA�+A��A�^5A�A��A�G�A��^A���A��mA�33A���A�^5A���A��FA��!A�~�A�+A�(�A�"�A� �A�oA�p�A�A��9A���A� �A�z�A�/A��DA��!A�~�A��wA�O�A��A�M�A��A���A��A��TA��
A���A�I�A��A��!A��uA�~�A�I�A�A���A�A�S�A}VAz�jAzI�Ay��AyVAx��Ax5?AvVAu%Arv�Am�#AlȴAjE�Ah�jAg�AeoAcS�A`��A^�yA]33A\E�A[�AZI�AX��AW%ATbAS�AR�\AR1'AQ�hAPA�AO;dAM��AM
=AK��AJ �AH{AF1'AD(�AA��A@��A@bA?��A?K�A>�HA>�uA>ffA> �A=�#A=/A<�A9��A6ȴA6 �A5"�A4VA3�-A37LA2�yA2�A2{A1�-A17LA0�A0$�A/�A.��A-�A,�!A,5?A+�-A*�!A);dA(�+A'��A&��A&1A%�A%��A$�A#��A#?}A"��A"bNA"$�A!�TA!�hA!"�A��AbNA�hA��A  A��A�A�
Ar�AƨAXA33A^5A�;AXA�7AM�A�mA��A
=A�AĜA�AE�A�A��A�PA33Az�A��AA
��A��AXA�yA�uA-A�A�wA\)A^5AĜA �A  A��A�A A�@��@�~�@�hs@�%@�A�@���@�C�@���@�5?@��^@�?}@��`@�bN@�33@�/@� �@�o@�~�@��-@�@�X@��@�9@�r�@�t�@�r�@�ff@��@�R@��@�j@߶F@��@�1@�@��@�%@��`@���@؋D@�(�@��m@�33@�o@��@��y@�ȴ@�^5@�@ա�@�X@ԣ�@ҸR@���@Ͼw@�@�ȴ@��@�`B@�%@ț�@Ǿw@���@�z�@ÍP@���@���@���@�-@��@��@��@���@��@�`B@���@���@��9@���@��D@�1@�^5@�p�@��j@���@��@��@�ȴ@���@��u@��m@��y@�E�@��@���@�O�@���@��@�|�@���@�n�@�5?@��-@�hs@�/@��/@��9@��u@�9X@��w@��y@�=q@���@�p�@�hs@�r�@���@�o@��!@�~�@��T@���@��@�%@���@���@�b@�C�@���@�5?@�@��^@�X@���@�Ĝ@� �@�|�@��@���@�$�@��@��@��#@�/@��u@�A�@��
@��P@�"�@��R@���@���@���@��\@��\@�V@�5?@��-@�?}@���@�Z@� �@��
@�ƨ@���@���@���@���@���@�t�@�S�@�K�@�;d@���@�ȴ@��+@�M�@�{@��#@��^@�O�@��@��`@��@��@�r�@�Q�@�(�@�b@�1@���@�S�@��H@�~�@�ff@�E�@��T@�@�?}@��@��@�r�@�9X@�  @���@���@���@��#@��^@���@���@��7@�hs@�?}@�V@���@��j@��@��@�bN@�Q�@�1'@�1@l�@~��@~@}p�@}O�@|�@|��@{��@{�F@{��@{�@{o@z��@z^5@y��@yX@y�@x�9@xQ�@x�@t�@rM�@mO�@c33@[�
@P��@Kt�@G+@B��@<��@6�R@1�^@+�m@&��@!��@I�@�@��@ff@�@�@&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��/A��HA��mA��TA��mA��A��`A��/A��A���A���A�ȴA�A�ƨAڣ�A���A���AԑhA��A�?}AͅA� �Aȴ9A�^5A�M�A��`A�+A�;dA�x�A�;dA��A��A�hsA�$�A��DA�A�K�A�oA�n�A���A���A��FA�v�A�{A���A�oA�ĜA�v�A�;dA�$�A��7A���A�A�A���A�&�A�;dA�ĜA�JA�+A��A�^5A�A��A�G�A��^A���A��mA�33A���A�^5A���A��FA��!A�~�A�+A�(�A�"�A� �A�oA�p�A�A��9A���A� �A�z�A�/A��DA��!A�~�A��wA�O�A��A�M�A��A���A��A��TA��
A���A�I�A��A��!A��uA�~�A�I�A�A���A�A�S�A}VAz�jAzI�Ay��AyVAx��Ax5?AvVAu%Arv�Am�#AlȴAjE�Ah�jAg�AeoAcS�A`��A^�yA]33A\E�A[�AZI�AX��AW%ATbAS�AR�\AR1'AQ�hAPA�AO;dAM��AM
=AK��AJ �AH{AF1'AD(�AA��A@��A@bA?��A?K�A>�HA>�uA>ffA> �A=�#A=/A<�A9��A6ȴA6 �A5"�A4VA3�-A37LA2�yA2�A2{A1�-A17LA0�A0$�A/�A.��A-�A,�!A,5?A+�-A*�!A);dA(�+A'��A&��A&1A%�A%��A$�A#��A#?}A"��A"bNA"$�A!�TA!�hA!"�A��AbNA�hA��A  A��A�A�
Ar�AƨAXA33A^5A�;AXA�7AM�A�mA��A
=A�AĜA�AE�A�A��A�PA33Az�A��AA
��A��AXA�yA�uA-A�A�wA\)A^5AĜA �A  A��A�A A�@��@�~�@�hs@�%@�A�@���@�C�@���@�5?@��^@�?}@��`@�bN@�33@�/@� �@�o@�~�@��-@�@�X@��@�9@�r�@�t�@�r�@�ff@��@�R@��@�j@߶F@��@�1@�@��@�%@��`@���@؋D@�(�@��m@�33@�o@��@��y@�ȴ@�^5@�@ա�@�X@ԣ�@ҸR@���@Ͼw@�@�ȴ@��@�`B@�%@ț�@Ǿw@���@�z�@ÍP@���@���@���@�-@��@��@��@���@��@�`B@���@���@��9@���@��D@�1@�^5@�p�@��j@���@��@��@�ȴ@���@��u@��m@��y@�E�@��@���@�O�@���@��@�|�@���@�n�@�5?@��-@�hs@�/@��/@��9@��u@�9X@��w@��y@�=q@���@�p�@�hs@�r�@���@�o@��!@�~�@��T@���@��@�%@���@���@�b@�C�@���@�5?@�@��^@�X@���@�Ĝ@� �@�|�@��@���@�$�@��@��@��#@�/@��u@�A�@��
@��P@�"�@��R@���@���@���@��\@��\@�V@�5?@��-@�?}@���@�Z@� �@��
@�ƨ@���@���@���@���@���@�t�@�S�@�K�@�;d@���@�ȴ@��+@�M�@�{@��#@��^@�O�@��@��`@��@��@�r�@�Q�@�(�@�b@�1@���@�S�@��H@�~�@�ff@�E�@��T@�@�?}@��@��@�r�@�9X@�  @���@���@���@��#@��^@���@���@��7@�hs@�?}@�V@���@��j@��@��@�bN@�Q�@�1'@�1@l�@~��@~@}p�@}O�@|�@|��@{��@{�F@{��@{�@{o@z��@z^5@y��@yX@y�@x�9@xQ�@x�G�O�@rM�@mO�@c33@[�
@P��@Kt�@G+@B��@<��@6�R@1�^@+�m@&��@!��@I�@�@��@ff@�@�@&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDB	7B
=B\BhB �B�B&�B/B5?B6FB9XB<jBE�BR�BXBJ�B�B�;B�B��B��B  B1BDBDB	7B%BB��B�B�fB�#B��B��B�LB�LB�?B�!B�B��B��B��B�hB�JB�+B�B� B|�By�Bw�Bq�BiyBcTBT�BB�B8RB(�B�B�B\B��B�B�NB�#B��BB�RB�!B��B�hB{�Be`B>wB'�B"�B!�B �B�B�BJB�B�B�}B�'B�bBw�Bl�B`BB]/BQ�B:^B(�B#�B{B1B+B%BB
��B
��B
�B
�B
�B
�yB
�ZB
�)B
��B
�{B
v�B
XB
F�B
B�B
?}B
7LB
49B
/B
�B
oB	��B	�/B	��B	ƨB	�}B	�FB	��B	��B	�B	z�B	q�B	k�B	ffB	`BB	XB	M�B	?}B	:^B	7LB	5?B	1'B	+B	%�B	�B	�B	�B	VB	1B	+B��B��B��B��B��B��B�B�B�B�B�B�B�`B�)B��B��B��BɺBǮBŢBĜBB��B�wB�jB�^B�FB�-B�!B�B��B��B��B��B��B��B��B��B�uB�uB�hB�VB�JB�=B�1B�+B�%B�B�B� B{�Bx�Bv�Bs�Bq�Bo�Bm�BiyBffBe`BdZBbNB`BB^5B[#BXBVBT�BS�BS�BR�BR�BQ�BQ�BP�BO�BN�BL�BH�BF�BE�BB�B?}B>wB=qB<jB<jB;dB:^B7LB49B33B5?B7LB7LB6FB5?B2-B33B33B2-B2-B1'B1'B0!B0!B/B/B.B-B,B)�B(�B(�B'�B'�B'�B'�B'�B&�B%�B#�B&�B'�B)�B,B.B.B.B0!B2-B2-B2-B33B33B33B33B49B49B5?B49B49B49B49B5?B5?B49B49B33B49B2-B0!B+B.B9XB;dB;dB;dB:^B:^B;dB:^B;dB=qB=qB?}B?}B>wB=qB@�B@�BC�BD�BD�BD�BD�BD�BC�BF�BG�BH�BH�BM�BP�BR�BT�BYBZB]/B_;B`BBaHBbNBcTBhsBiyBm�Bo�Bp�Br�Bs�Bt�Bv�Bw�Bw�Bx�Bz�B~�B�B�+B�1B�+B�\B�{B��B��B��B��B��B��B��B�B�B�B�9B�^B�qB�}B��BÖBƨBǮB��B��B��B�
B�B�5B�5B�BB�`B�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	
=B	PB	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	(�B	)�B	.B	0!B	2-B	49B	6FB	6FB	7LB	9XB	:^B	:^B	;dB	?}B	A�B	C�B	D�B	E�B	F�B	G�B	I�B	L�B	M�B	M�B	O�B	P�B	R�B	W
B	^5B	_;B	_;B	`BB	`BB	aHB	bNB	dZB	e`B	ffB	gmB	hsB	iyB	jB	k�B	l�B	m�B	n�B	q�B	r�B	s�B	t�B	t�B	t�B	v�B	w�B	w�B	x�B	y�B	z�B	{�B	}�B	� B	�B	�B	�B	�1B	ƨB	��B	�B	��B	�fB
B
VB
�B
�B
(�B
2-B
9XB
@�B
G�B
M�B
T�B
YB
^5B
ffB
n�B
r�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B2B	%B
+BJBVB �B�B&�B/B5-B67B9IB<[BE�BR�BXBJ�B�B�)B�B��B��B��BB2B.B	&BB �B��B�sB�PB�B��B�kB�3B�5B�%B�B��B��B��B��B�OB�2B�B��B�B|�By�Bw�Bq�Bi_Bc9BT�BBrB8:B(�B�BeB>B��B�hB�3B�B��B�pB�6B� B��B�HB{�Be=B>YB'�B"�B!�B �B�B�B)B�~B��B�^B�B�CBw�BlkB`$B]BQ�B:<B(�B#�B\BBBB�B
��B
��B
�B
�sB
�eB
�WB
�;B
�	B
�jB
�_B
v�B
W�B
F�B
BsB
?`B
7.B
4B
/ B
�B
PB	��B	�B	��B	ƏB	�bB	�,B	��B	�hB	�B	z�B	q�B	koB	fMB	`*B	W�B	M�B	?hB	:HB	76B	5*B	1B	*�B	%�B	�B	�B	jB	BB	B	B��B��B��B��B��B��B�B�B�B�B�B�lB�JB�B��B��B̺BɨBǙBŌBĈB�}B�rB�dB�VB�LB�5B�B�B��B��B��B��B��B��B��B��B�oB�aB�dB�WB�DB�9B�*B� B�B�B�B�B�B{�Bx�Bv�Bs�Bq�Bo�Bm�BigBfVBeOBdGBb?B`1B^%B[BX BU�BT�BS�BS�BR�BR�BQ�BQ�BP�BO�BN�BL�BH�BF�BE�BB~B?nB>gB=`B<ZB<YB;SB:PB7>B4)B3#B5+B7;B7<B65B5-B2B3#B3"B2B2B0�B1B/�B0B/B/B.B,�B+�B)�B(�B(�B'�B'�B'�B'�B'�B&�B%�B#�B&�B'�B)�B+�B.B.B.B0B2B2B2B3"B3!B3"B3#B4(B4*B5.B4'B4&B4(B4)B5.B5,B4(B4%B3!B4(B2B0B*�B.B9IB;RB;RB;PB:KB:LB;QB:IB;PB=aB=`B?gB?iB>cB=^B@oB@oBC�BD�BD�BD�BD�BD�BC�BF�BG�BH�BH�BM�BP�BR�BT�BYBZB]B_%B`-Ba3Bb7Bc:Bh^BicBmzBo�Bp�Br�Bs�Bt�Bv�Bw�Bw�Bx�Bz�B~�B�B�B�B�B�BB�bB�mB�{B��B��B��B��B��B��B��B�B�!B�FB�VB�cB�nB�yBƊBǏB˪B��B��B��B�B�B�B�%B�AB�]B�nB�B��B�B��B��B��B��B��B��B��B��B��B	�B	
 B	2B	=B	VB	bB	dB	bB	bB	hB	tB	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	(�B	)�B	-�B	0B	2B	4B	6$B	6"B	7-B	99B	:=B	:?B	;GB	?\B	AiB	CuB	D|B	E�B	F�B	G�B	I�B	L�B	M�B	M�B	O�B	P�B	R�B	V�B	^B	_B	_B	`B	`B	a$B	b-B	d:B	e>B	fDB	gKB	hQB	iVB	j\B	kcB	lhB	mnB	nwB	q�B	r�B	s�B	t�B	t�B	t�B	v�B	w�B	w�B	x�B	y�B	z�B	{�B	}�B	�B	��B	��B	��B	�G�O�B	�~B	��B	��B	�AB
 �B
/B
aB
�B
(�B
2B
90B
@YB
G�B
M�B
T�B
X�B
^B
f>B
noB
r�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708202016053117082020160531170820  AO  ARCAADJP                                                                    20140825000136    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140825000136  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140825000136  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170820  IP                  G�O�G�O�G�O�                