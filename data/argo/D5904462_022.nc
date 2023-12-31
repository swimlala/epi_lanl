CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:23Z AOML 3.0 creation; 2016-08-07T21:51:12Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221423  20160807145112  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_022                   2C  D   APEX                            6529                            072314                          846 @�)*�r 1   @�)+)���@2$�t�j�d*5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy�fD�3D�S3D��fD���D�3D�C3D��fD��fD�3D�<�D�i�DǦfD��fD�S3Dڌ�D�� D�3D�L�D�c3D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B�z�B��B��B��B��B��B��GB��B��B��B��B��B��B��B��BĮBȮB�z�BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
Cp�CW
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
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�)DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dto]Dy�)D�D�^D��GD�׮D�D�ND��GD��GD�D�G�D�t{DǱGD�GD�^Dڗ�D���D�D�W�D�nD�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�z�A�|�A�|�A�~�AӁAӁAӃAӃAӅAӅAӇ+AӇ+AӉ7AӋDAӋDAӍPAӍPAӏ\Aӏ\AӑhAӑhAӓuAӑhAӑhAӓuAӓuAӓuAӕ�Aӕ�Aӗ�Aә�Aӛ�Aӛ�Aӝ�Aӝ�Aӝ�Aӟ�Aӟ�Aӡ�Aӟ�Aӝ�Aә�Aӗ�Aӗ�A�ffA�33A�"�A��A�"�A�VAɇ+AƍPAò-A¾wA�n�A�C�A���A���A�hsA�hsA�33A��A�1'A�JA��wA�r�A�%A�S�A��\A���A�K�A��#A��-A��A�9XA��-A��
A��`A��\A�A�Q�A�5?A�5?A�1A���A�VA�$�A�A�jA�/A�t�A�n�A��wA��A�z�A��A��A���A���A�v�A���A�(�A�bA��7A�%A�t�A�dZA�hsA� �A� �A���A��-A���A��\A|��Az�DAy&�ArVAlA�Ai
=Ae�PA`�HA\��AYG�AV��AT�AR�jAPn�AKK�AIXAH�AHM�AGVAE�;AC�AA;dA?��A>1A<�\A8��A6-A5��A4�A3G�A1�A1p�A0�!A0�+A0r�A0$�A/��A/G�A.��A.��A-7LA(��A&��A%�
A%+A$�uA#�A!p�A VA�wAS�A�Ax�A�uA5?A�RAffA��A��AdZAn�A�
A�PA�`A33A��A��A7LA�-An�AbAO�A
�/A
�!A
  A	p�A	p�A	`BA	+A�RA�At�A+A&�AoA�/A�!A�+AM�A��A�hA�A|�A��AAO�A�+A=qA�FAl�A/AĜAQ�A�A|�A �@��@�M�@�p�@�7L@��/@��@�l�@�-@��@�(�@���@���@�G�@�V@��u@��@�|�@�+@�
=@�@�J@�`B@�9@�ƨ@�+@��T@�Ĝ@��@�@띲@�P@�@�l�@�l�@�S�@�!@�ff@��#@�1'@���@�-@�@�X@�(�@��@�  @�R@�^@�9@�Q�@�1'@�;d@�M�@���@ݑh@�=q@�G�@�\)@�dZ@��@�-@��/@�dZ@��@֗�@�5?@Ձ@�&�@�Ĝ@�1'@ӶF@�dZ@�
=@Ұ!@�v�@���@��@��/@�(�@϶F@�dZ@�
=@θR@Χ�@�ff@�=q@��@͡�@�p�@�hs@�?}@��/@�z�@�Z@��m@ˮ@�C�@��@��@ʏ\@�$�@��@ɉ7@���@ȓu@�Q�@�1@��
@�\)@��@��T@��T@ŉ7@���@ēu@�(�@Å@�;d@���@¸R@\@��@�hs@��D@���@��w@��@�;d@��!@�n�@�n�@�v�@�M�@��@��7@��j@� �@��;@�ƨ@��F@���@���@�ff@��@�p�@��`@�(�@��F@�o@��!@�-@�p�@���@��@�I�@�  @��
@���@�+@�ȴ@�^5@��@���@���@���@��D@�I�@��@�"�@���@�n�@�$�@���@���@�/@���@�1'@�ƨ@�C�@��@���@��@���@�X@���@�j@�b@�1@��F@�\)@�+@��H@�~�@�V@�$�@��@��h@�X@��@��@�Z@���@�ƨ@�l�@��y@�ȴ@���@���@�{@�@���@�p�@�?}@���@��j@��u@�Z@���@���@�ƨ@���@�\)@�C�@��@�@��@��R@�V@��@��@��7@�`B@�?}@��j@�bN@�(�@�(�@��@��@�\)@���@�M�@�-@���@���@���@���@��h@��@�p�@�7L@�%@���@��/@��9@�j@�1'@�  @�|�@�+@��@�^5@��@��@��h@�Ĝ@��@��7@���@�J@�Z@wl�@qx�@g��@_�P@V�R@N�+@Gl�@@A�@:�@4z�@-?}@&�@"J@��@��@�j@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�v�A�z�A�|�A�|�A�~�AӁAӁAӃAӃAӅAӅAӇ+AӇ+AӉ7AӋDAӋDAӍPAӍPAӏ\Aӏ\AӑhAӑhAӓuAӑhAӑhAӓuAӓuAӓuAӕ�Aӕ�Aӗ�Aә�Aӛ�Aӛ�Aӝ�Aӝ�Aӝ�Aӟ�Aӟ�Aӡ�Aӟ�Aӝ�Aә�Aӗ�Aӗ�A�ffA�33A�"�A��A�"�A�VAɇ+AƍPAò-A¾wA�n�A�C�A���A���A�hsA�hsA�33A��A�1'A�JA��wA�r�A�%A�S�A��\A���A�K�A��#A��-A��A�9XA��-A��
A��`A��\A�A�Q�A�5?A�5?A�1A���A�VA�$�A�A�jA�/A�t�A�n�A��wA��A�z�A��A��A���A���A�v�A���A�(�A�bA��7A�%A�t�A�dZA�hsA� �A� �A���A��-A���A��\A|��Az�DAy&�ArVAlA�Ai
=Ae�PA`�HA\��AYG�AV��AT�AR�jAPn�AKK�AIXAH�AHM�AGVAE�;AC�AA;dA?��A>1A<�\A8��A6-A5��A4�A3G�A1�A1p�A0�!A0�+A0r�A0$�A/��A/G�A.��A.��A-7LA(��A&��A%�
A%+A$�uA#�A!p�A VA�wAS�A�Ax�A�uA5?A�RAffA��A��AdZAn�A�
A�PA�`A33A��A��A7LA�-An�AbAO�A
�/A
�!A
  A	p�A	p�A	`BA	+A�RA�At�A+A&�AoA�/A�!A�+AM�A��A�hA�A|�A��AAO�A�+A=qA�FAl�A/AĜAQ�A�A|�A �@��@�M�@�p�@�7L@��/@��@�l�@�-@��@�(�@���@���@�G�@�V@��u@��@�|�@�+@�
=@�@�J@�`B@�9@�ƨ@�+@��T@�Ĝ@��@�@띲@�P@�@�l�@�l�@�S�@�!@�ff@��#@�1'@���@�-@�@�X@�(�@��@�  @�R@�^@�9@�Q�@�1'@�;d@�M�@���@ݑh@�=q@�G�@�\)@�dZ@��@�-@��/@�dZ@��@֗�@�5?@Ձ@�&�@�Ĝ@�1'@ӶF@�dZ@�
=@Ұ!@�v�@���@��@��/@�(�@϶F@�dZ@�
=@θR@Χ�@�ff@�=q@��@͡�@�p�@�hs@�?}@��/@�z�@�Z@��m@ˮ@�C�@��@��@ʏ\@�$�@��@ɉ7@���@ȓu@�Q�@�1@��
@�\)@��@��T@��T@ŉ7@���@ēu@�(�@Å@�;d@���@¸R@\@��@�hs@��D@���@��w@��@�;d@��!@�n�@�n�@�v�@�M�@��@��7@��j@� �@��;@�ƨ@��F@���@���@�ff@��@�p�@��`@�(�@��F@�o@��!@�-@�p�@���@��@�I�@�  @��
@���@�+@�ȴ@�^5@��@���@���@���@��D@�I�@��@�"�@���@�n�@�$�@���@���@�/@���@�1'@�ƨ@�C�@��@���@��@���@�X@���@�j@�b@�1@��F@�\)@�+@��H@�~�@�V@�$�@��@��h@�X@��@��@�Z@���@�ƨ@�l�@��y@�ȴ@���@���@�{@�@���@�p�@�?}@���@��j@��u@�Z@���@���@�ƨ@���@�\)@�C�@��@�@��@��R@�V@��@��@��7@�`B@�?}@��j@�bN@�(�@�(�@��@��@�\)@���@�M�@�-@���@���@���@���@��h@��@�p�@�7L@�%@���@��/@��9@�j@�1'@�  @�|�@�+@��@�^5@��@��@��h@�ĜG�O�@��7@���@�J@�Z@wl�@qx�@g��@_�P@V�R@N�+@Gl�@@A�@:�@4z�@-?}@&�@"J@��@��@�j@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B�
B�B��B��B�9B�^B.Bm�B�PB��B��B��B��B��B��B��B��B��B�Bw�Bo�B\)BO�BJ�BM�BA�B8RB49B5?B49B'�B$�B�B�B�B�BuBhBPBbB1B�sB�BB��B�^B�B��B�JBhsBE�B<jB.B�B  B
�B
��B
ĜB
�'B
�VB
|�B
ffB
T�B
E�B
)�B
%B	�B	�TB	�3B	�7B	q�B	YB	B�B	49B	$�B	�B	{B	VB	B��B��B�B�B�B�yB�TB�;B�)B�B��B��B��B��B��BɺBȴBȴBȴBǮBǮBƨBŢBĜBB�}B�dB�?B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�BɺB�
B�)B�NB��B��B��B��B��B	B	1B	VB	{B	�B	/B	6FB	8RB	9XB	;dB	B�B	J�B	J�B	I�B	I�B	H�B	H�B	H�B	I�B	J�B	J�B	J�B	K�B	L�B	Q�B	VB	VB	ZB	\)B	ZB	YB	XB	VB	T�B	R�B	P�B	R�B	S�B	S�B	T�B	T�B	VB	VB	VB	YB	\)B	]/B	^5B	`BB	aHB	aHB	cTB	cTB	jB	s�B	u�B	v�B	x�B	x�B	y�B	y�B	~�B	�B	�B	�B	�1B	�7B	�7B	�7B	�7B	�DB	�\B	�hB	�hB	�hB	�bB	�bB	�oB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�?B	�!B	�3B	�?B	�9B	�3B	�!B	�3B	�3B	�RB	�LB	�FB	�LB	�XB	�^B	�jB	�qB	�qB	�wB	�}B	��B	��B	ÖB	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�5B	�5B	�;B	�HB	�BB	�;B	�/B	�/B	�5B	�;B	�BB	�NB	�TB	�TB	�TB	�`B	�fB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
VB
�B
�B
 �B
&�B
-B
5?B
;dB
A�B
E�B
J�B
Q�B
W
B
^5B
bNB
ffB
jB
n�B
s�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�nB��B��B˩B�eB�B�@B-�BmsB�7B�lB��B��B��B��B��B��B��B�nB��Bw�Bo�B\	BO�BJ�BM�BAkB81B4B5B4B'�B$�B�B�B�BcBUBJB2BABB�TB�&B��B�?B��B��B�(BhRBE�B<JB-�BzB
��B
�^B
��B
�B
�	B
�9B
|�B
fLB
T�B
E�B
)�B
B	�B	�<B	�B	�B	q�B	YB	BwB	4%B	$�B	�B	hB	AB	B��B��B�B�B�~B�fB�BB�(B�B��B��B��B��B̺BʬBɦBȠBȠBȡBǘBǙBƓBŎBĉB�|B�jB�OB�,B�B�B�	B��B��B��B��B��B��B��B�~B�yB�oB�{B��B��B��B��BɣB��B�B�7B��B��B��B��B��B	 B	B	=B	cB	�B	/ B	6+B	87B	9;B	;IB	BsB	J�B	J�B	I�B	I�B	H�B	H�B	H�B	I�B	J�B	J�B	J�B	K�B	L�B	Q�B	U�B	U�B	ZB	\B	ZB	X�B	W�B	U�B	T�B	R�B	P�B	R�B	S�B	S�B	T�B	T�B	U�B	U�B	U�B	X�B	\B	]B	^B	`#B	a*B	a(B	c4B	c6B	jcB	s�B	u�B	v�B	x�B	x�B	y�B	y�B	~�B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�=B	�IB	�HB	�JB	�DB	�BB	�NB	�OB	�SB	�TB	�TB	��B	�zB	�jB	�rB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�0B	�*B	�#B	�-B	�3B	�;B	�IB	�MB	�PB	�VB	�ZB	�fB	�gB	�rB	�rB	�yB	ƇB	ǌB	ȐB	ʟB	ʟB	̨B	̨B	ͯB	ͭB	ζB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�B	�*B	�0B	�/B	�-B	�<B	�AB	�=B	�;B	�7B	�7B	�4B	�7B	�5B	�7B	�6B	�9B	�;B	�=B	�7B	�=B	�>B	�<B	�<B	�<B	�;B	�;B	�:B	�;B	�<B	�;B	�=B	�>B	�CB	�BB	�BB	�:B	�CB	�BB	�CB	�BB	�AB	�HB	�OB	�UB	�[B	�YB	�ZB	�ZB	�YB	�bB	�fB	�gB	�eB	�hB	�`B	�_B	�`B	�fB	�eB	�eB	�fB	�fB	�gB	�eB	�gB	�oB	�nB	�mB	�jB	�tB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
B
0B
aB
�B
 �B
&�B
,�B
5B
;>B
AbB
EzB
J�B
Q�B
V�B
^B
b)B
f@B
jVB
nnB
s�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451122016080714511220160807145112  AO  ARCAADJP                                                                    20150226221423    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221423  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221423  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145112  IP                  G�O�G�O�G�O�                