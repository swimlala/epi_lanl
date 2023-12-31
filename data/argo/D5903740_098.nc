CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-11-24T23:00:11Z AOML 3.0 creation; 2016-06-01T00:08:21Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20141124230011  20160531170822  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               bA   AO  4055_7112_098                   2C  D   APEX                            5374                            041511                          846 @�%�r(@1   @�%�(�?�@9š����d)x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    bA   A   A   @�ff@�  A   A#33A@  A`  A�  A�33A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyl�D��D�L�D���D�ٚD�	�D�<�D�ffD��fD�3D�I�D�vfD��3D���D�c3DږfD�ɚD�  D�<�D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��HAp�A(��AEp�Aep�A��RA��A��RA��A¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BIBQBY\)Ba\)Bh��Bp��By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��GB��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�'�D�W�D���D��{D�{D�G�D�qGD��GD�D�T{D��GD��D�{D�nDڡGD��{D�
�D�G�D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�I�A�Q�A�K�A�O�A�M�A�VA�S�A�VA�ZA�n�A�jA�l�A�jA�n�A�jA�t�A�p�A�t�A�t�A�n�A�VA�VA���A��AƁA�5?A� �A�;dA�ƨA�|�A��A��A�A�ƨA�+A�"�A�A�ZA��yA�M�A�{A��+A��
A�hsA�A��TA��A�ffA�I�A�ZA�ȴA��-A�dZA��A��A�9XA��PA�VA�A��A��A�E�A���A��PA�`BA���A���A��\A�(�A���A�VA�t�A���A�1'A���A�A�-A�bA�z�A��A�A��+A��A��wA� �A|�A~ĜA|��Ay��Ax�jAw��Av�/AtĜAq��AoC�Al9XAi�TAi/Ah��AhE�AgXAeK�Ad�\Ad$�Ac�mAb��Aa33A`  A_ƨA_��A_K�A^ȴA^ZA]�TA]%A[\)AX��AWdZAVĜAV�+AVbNAV=qAUp�ATI�ASK�AQx�APA�ANQ�AL�ALJAK��AK"�AJ�RAI��AHVAG�AGt�AF�`AE�;AD�HADM�AC�AA�
AAA@Q�A?�
A>�HA>=qA<jA;/A:��A9ƨA9�-A9C�A8z�A6JA5
=A3dZA2^5A21A1;dA/A/��A/�A/XA.��A.ZA-�#A-t�A+A*��A)�A)
=A'��A'"�A&ĜA&=qA%�TA%��A%\)A$�/A#�A#p�A"ZA!�TA!t�A ĜA�
A�yA��AJA��A;dA�RA��A�`A��AI�A�A1A1A�-A��Ar�A�#A��A�A;dA�mA�yA|�A�A�`A�+A��A �A�;A�Ax�A�/A-A�A;dA
I�A	;dA��AA�AdZAffAQ�A�A7LA�RAE�AXA�RA�\AffA�FA
=A �jA V@��@���@�I�@���@�
=@��@��@��@�@�\@�x�@�V@�j@��@�ff@�@�p�@�9@睲@��@��@�7@��@��@���@�|�@�5?@�@ݡ�@�G�@�l�@�=q@ٲ-@�A�@ָR@�1'@�=q@��@���@У�@� �@�33@�`B@ɺ^@��@ǅ@ƸR@�/@���@���@��@���@�C�@���@�x�@���@��m@���@��@�S�@�E�@�&�@��D@�b@�S�@�@�?}@�/@��@��@���@��9@�9X@��@���@�r�@�(�@���@��@�^5@���@��u@���@�M�@��`@�j@�A�@��@��@��
@��P@�|�@�dZ@�@��R@�J@�G�@���@�\)@�K�@�C�@�33@���@��\@��@���@��m@��y@���@�ff@�@��h@�`B@�&�@��9@�(�@���@�S�@�@���@�$�@�Ĝ@�  @���@�"�@�=q@�O�@��u@��@��w@��@��@���@���@�ff@�M�@�5?@�@���@��h@�p�@�?}@��@��/@��D@�j@�I�@��@��@��F@��P@�S�@�o@���@�ff@��@�%@��u@�(�@��@�b@���@��F@��@�C�@�@��y@��@���@�n�@�{@��#@���@���@���@��@�p�@�p�@�hs@�`B@�O�@�?}@��@��/@�r�@��@���@�S�@�C�@�o@��!@���@���@�~�@�$�@�@��@�@��-@���@���@��h@��@�`B@�/@��@���@��@�j@�j@�I�@� �@�1@�w@�P@|�@�@~E�@}p�@}�@|�/@|��@|�@|�D@|z�@|Z@|9X@|�@{ƨ@z�@z=q@y�@y�^@yX@x��@x�u@xb@w
=@v�+@v�+@vV@v5?@v@u��@u@sdZ@i%@a%@W�w@Q�@L�@F��@A�7@:=q@2��@,(�@&$�@$�@G�@E�@33@1'@��@��@��@J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�I�A�Q�A�K�A�O�A�M�A�VA�S�A�VA�ZA�n�A�jA�l�A�jA�n�A�jA�t�A�p�A�t�A�t�A�n�A�VA�VA���A��AƁA�5?A� �A�;dA�ƨA�|�A��A��A�A�ƨA�+A�"�A�A�ZA��yA�M�A�{A��+A��
A�hsA�A��TA��A�ffA�I�A�ZA�ȴA��-A�dZA��A��A�9XA��PA�VA�A��A��A�E�A���A��PA�`BA���A���A��\A�(�A���A�VA�t�A���A�1'A���A�A�-A�bA�z�A��A�A��+A��A��wA� �A|�A~ĜA|��Ay��Ax�jAw��Av�/AtĜAq��AoC�Al9XAi�TAi/Ah��AhE�AgXAeK�Ad�\Ad$�Ac�mAb��Aa33A`  A_ƨA_��A_K�A^ȴA^ZA]�TA]%A[\)AX��AWdZAVĜAV�+AVbNAV=qAUp�ATI�ASK�AQx�APA�ANQ�AL�ALJAK��AK"�AJ�RAI��AHVAG�AGt�AF�`AE�;AD�HADM�AC�AA�
AAA@Q�A?�
A>�HA>=qA<jA;/A:��A9ƨA9�-A9C�A8z�A6JA5
=A3dZA2^5A21A1;dA/A/��A/�A/XA.��A.ZA-�#A-t�A+A*��A)�A)
=A'��A'"�A&ĜA&=qA%�TA%��A%\)A$�/A#�A#p�A"ZA!�TA!t�A ĜA�
A�yA��AJA��A;dA�RA��A�`A��AI�A�A1A1A�-A��Ar�A�#A��A�A;dA�mA�yA|�A�A�`A�+A��A �A�;A�Ax�A�/A-A�A;dA
I�A	;dA��AA�AdZAffAQ�A�A7LA�RAE�AXA�RA�\AffA�FA
=A �jA V@��@���@�I�@���@�
=@��@��@��@�@�\@�x�@�V@�j@��@�ff@�@�p�@�9@睲@��@��@�7@��@��@���@�|�@�5?@�@ݡ�@�G�@�l�@�=q@ٲ-@�A�@ָR@�1'@�=q@��@���@У�@� �@�33@�`B@ɺ^@��@ǅ@ƸR@�/@���@���@��@���@�C�@���@�x�@���@��m@���@��@�S�@�E�@�&�@��D@�b@�S�@�@�?}@�/@��@��@���@��9@�9X@��@���@�r�@�(�@���@��@�^5@���@��u@���@�M�@��`@�j@�A�@��@��@��
@��P@�|�@�dZ@�@��R@�J@�G�@���@�\)@�K�@�C�@�33@���@��\@��@���@��m@��y@���@�ff@�@��h@�`B@�&�@��9@�(�@���@�S�@�@���@�$�@�Ĝ@�  @���@�"�@�=q@�O�@��u@��@��w@��@��@���@���@�ff@�M�@�5?@�@���@��h@�p�@�?}@��@��/@��D@�j@�I�@��@��@��F@��P@�S�@�o@���@�ff@��@�%@��u@�(�@��@�b@���@��F@��@�C�@�@��y@��@���@�n�@�{@��#@���@���@���@��@�p�@�p�@�hs@�`B@�O�@�?}@��@��/@�r�@��@���@�S�@�C�@�o@��!@���@���@�~�@�$�@�@��@�@��-@���@���@��h@��@�`B@�/@��@���@��@�j@�j@�I�@� �@�1@�w@�P@|�@�@~E�@}p�@}�@|�/@|��@|�@|�D@|z�@|Z@|9X@|�@{ƨ@z�@z=q@y�@y�^@yX@x��@x�u@xb@w
=@v�+@v�+@vV@v5?@v@u��@u@sdZ@i%@a%@W�w@Q�@L�@F��@A�7@:=q@2��@,(�@&$�@$�@G�@E�@33@1'@��@��@��@J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�B�%B�B�B�B�B�B�%B�%B�%B�B�B}�B{�By�Bn�BF�BhB��Bu�BK�B1'B(�B$�B�BB��B�fB��BÖB�B��B�uB�7B�B}�B{�Bv�Bp�BZBG�B-B�B�BbB+B��B�yB�ZB�)BǮB�-B��B��B��B��B�1B|�Bz�Bs�BjB`BBVBK�B:^B �BhBB
�B
�;B
�qB
��B
�+B
l�B
XB
L�B
E�B
>wB
,B
�B
JB
B	��B	�sB	��B	�wB	�B	��B	��B	�{B	�bB	�1B	{�B	u�B	r�B	o�B	ffB	^5B	YB	XB	VB	S�B	P�B	M�B	J�B	D�B	<jB	/B	)�B	'�B	&�B	%�B	$�B	!�B	�B	�B	uB	VB	1B	B	B��B��B��B��B�B�B�B�B�B�B�B�B�B�`B�TB�fB�NB�5B�#B�#B�)B�
B�)B�/B�#B��B��BBBĜBÖB�wB�wB�}B��B�wB�qB�jB�XB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�PB�=B�%B�B� B}�B{�Bx�Bu�Br�Bo�Bo�Bn�Bm�Bm�Bl�BjBiyBiyBhsBffBbNB\)BXBS�BP�BN�BL�BI�BG�BF�BF�BE�BC�BB�BA�B@�B>wB<jB;dB:^B7LB5?B49B33B2-B0!B/B.B-B,B,B+B(�B'�B&�B$�B!�B�B�B�B�B�B�B�B�B{B{BuBuBoBoBoBhBbBbBbBbB\BVBPBJBJBJBDBDB	7B+B%B1B+B%BBB%BBBBBB%B1B1B	7B	7BJBPBbBbBhBhBuBuB{B�B�B�B�B �B!�B"�B#�B'�B(�B(�B(�B(�B(�B(�B)�B,B2-B49B49B6FB7LB9XB;dB>wB?}BE�BK�BL�BM�BM�BN�BN�BO�BO�BO�BP�BQ�BS�BW
BZBaHBaHBaHBaHBbNBcTBffBjBp�Bv�Bx�By�B{�B}�B~�B� B�B�%B�=B�JB�PB�\B�oB��B��B��B��B�B�9B�^B�qB��B��BŢBǮBɺB��B��B��B��B��B��B��B��B�B�B�#B�)B�/B�;B�BB�NB�ZB�fB�sB�B�B�B��B	B	DB	JB	JB	PB	\B	hB	uB	�B	�B	�B	�B	 �B	$�B	'�B	(�B	+B	-B	0!B	33B	5?B	6FB	6FB	7LB	9XB	=qB	C�B	G�B	L�B	P�B	R�B	S�B	VB	[#B	[#B	[#B	\)B	`BB	bNB	cTB	e`B	e`B	ffB	ffB	gmB	hsB	iyB	k�B	n�B	n�B	r�B	s�B	s�B	t�B	v�B	w�B	x�B	z�B	z�B	{�B	�B	�%B	�1B	�7B	�=B	�=B	�DB	�DB	�JB	�PB	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ȴB	�5B	��B
B
hB
�B
"�B
-B
9XB
A�B
J�B
S�B
[#B
^5B
bNB
e`B
iyB
n�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B}�B{�By�Bn�BF�BTB̷Bu�BK�B1B(�B$�B�BB��B�KB��B�vB��B��B�TB�B��B}�B{�Bv�Bp�BY�BG�B,�B�B`BBB
B��B�YB�8B�BǍB�B��B�~B�xB�`B�B|�Bz�Bs�Bj^B`BU�BK�B:>B �BHB�B
�rB
�B
�SB
��B
�B
lpB
W�B
L�B
E�B
>\B
+�B
kB
/B
�B	��B	�XB	��B	�]B	��B	��B	�tB	�dB	�MB	�B	{�B	u�B	r�B	o�B	fOB	^B	YB	W�B	U�B	S�B	P�B	M�B	J�B	D�B	<SB	/B	)�B	'�B	&�B	%�B	$�B	!�B	�B	�B	aB	AB	B	B	 �B��B��B��B��B�B�B�B�~B�B�qB�|B�B�kB�NB�@B�OB�<B�"B�B�B�B��B�B�B�B˴BʰB�}B�|BĊBÂB�fB�fB�iB�oB�eB�^B�VB�DB�!B�B��B��B��B��B��B��B��B��B��B��B��B�oB�]B�QB�=B�+B�B��B�B}�B{�Bx�Bu�Br�Bo�Bo�Bn�Bm�BmBlzBjoBijBiiBhcBfVBb=B\BW�BS�BP�BN�BL�BI�BG�BF�BF�BE�BC�BB�BAyB@rB>gB<[B;TB:MB7=B52B4+B3$B2B0B.�B.B,�B+�B+�B*�B(�B'�B&�B$�B!�B�B|B�B}BbBbBxBoBRBQBKBeBDBBBaB@B8B6BSB8BKB-BABB BB5BB	BBBBB�B�B�BB�B�BB�B�BBB!B	)B	'B8BAB6BQB;BXBcBdBMBwB�B�B�B �B!�B"�B#�B'�B(�B(�B(�B(�B(�B(�B)�B+�B2B4$B4&B61B77B9CB;QB>cB?hBE�BK�BL�BM�BM�BN�BN�BO�BO�BO�BP�BQ�BS�BV�BZBa2Ba4Ba2Ba3Bb8Bc=BfNBjeBp�Bv�Bx�By�B{�B}�B~�B�B��B�B�#B�2B�6B�CB�VB��B��B��B��B��B�B�CB�UB�eB�mBŇBǑBɞBʧB˪B̱B��B��B��B��B��B��B��B�B�B�B�B�&B�0B�=B�JB�SB�`B�zB�B��B	�B	$B	+B	,B	1B	@B	HB	XB	iB	nB	zB	�B	 �B	$�B	'�B	(�B	*�B	,�B	0B	3B	5 B	6%B	6%B	7*B	95B	=NB	CuB	G�B	L�B	P�B	R�B	S�B	U�B	[ B	[B	[ B	\	B	` B	b/B	c1B	e>B	e>B	fDB	fDB	gKB	hSB	iWB	kcB	nsB	nuB	r�B	s�B	s�B	t�B	v�B	w�B	x�B	z�B	z�B	{�B	��B	�B	�B	�B	�B	�B	� B	� B	�(B	�+B	�+B	�3B	�MB	�\B	�cB	�iB	�vB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȏB	�B	��B
�B
BB
tB
"�B
,�B
90B
AbB
J�B
S�B
Z�B
^B
b%B
e:B
iQB
npB
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708222016053117082220160531170822  AO  ARCAADJP                                                                    20141124230011    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141124230011  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141124230011  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170822  IP                  G�O�G�O�G�O�                