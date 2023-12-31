CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-27T02:15:19Z AOML 3.0 creation; 2016-06-01T00:08:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150427021519  20160531170824  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               qA   AO  4055_7112_113                   2C  D   APEX                            5374                            041511                          846 @�L<�K@1   @�L=�� @;F�x����dB5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    qA   A   A   @�33@�33@���A!��A@  A`  A���A�  A�33A�ffA�  A�  A�  A�  B ffB  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyy�D�  D�` D���D��fD��D�C3D��fD��3D�	�D�0 D�vfDǦfD��D�I�Dډ�D�ɚD���D�,�D�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�{A�
A'
>AEp�Aep�A��A��RA��A��A¸RAҸRA�RA�RBB	\)B\)B\)B!B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)BqBy\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
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
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�]D�
�D�j�D���D��GD��D�ND��GD��D�{D�:�D��GDǱGD�${D�T{Dڔ{D��{D�{D�7�D�GD��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A��A��
A��
A��
A���A���A�ĜA��jA��A�v�A�ĜA��HA��;A���A�E�A�A�p�A�+A���A���A�1A�7LA��7A��A�p�A��!A�ĜA�hsA�dZA��jA�~�A�C�A�+A�S�A�A�A�|�A���A�^5A�1'A�%A���A���A�XA���A�7LA�t�A��^A��A�\)A�5?A��#A�r�A��A�ĜA��hA���A�n�A�+A���A���A�=qA�(�A��A��uA� �A�33A�I�A�A���A��A��HA�{A��A��hA���A��;A� �A���A��A~��A~��A|I�Ax��At^5Ar��Aqp�Aq�As;dArbNAm��Ak33AiG�Ag�AfffAd��Ab��AbAa�^Aa33A`�A^ȴA^Q�A]��AZ��AV��AVbAUx�AT�AS��AR�9AQ��AQ&�AOO�AM��AK��AK�AIx�AG��AF�jAFZAE�AE��AE�AD9XACC�ABĜAA��A@=qA>�/A>M�A> �A>{A=�A=?}A<Q�A;;dA:bA9|�A9�A9��A:�A8�!A8A�A8(�A7��A7��A7��A7�A6n�A6�A5�#A5�A3��A3oA2��A2^5A2{A1�-A1?}A0�A/�wA/�A.ffA+��A*M�A)��A)+A(��A(ffA(bA'/A&v�A%��A$5?A#`BA#
=A"�jA"ffA!�A ��A (�A�#A�FAp�A+A��A�
A33AA�A��A�!A�TA�!AVA|�A�uA�A��AA�\A��Ax�AA�jAJA�A33A�`A��A�+AjAM�A�A��Ax�A
��A
1A	�A=qAXA$�A9XA+A{AG�@��F@��y@�E�@��#@���@��9@��F@�n�@��@��w@��R@���@��+@�^5@���@��@���@�V@� �@�|�@��H@�n�@��T@�X@�%@��/@���@�{@�dZ@�x�@���@�@�@� �@�V@�\)@���@���@۝�@���@ڗ�@�n�@�@٩�@�%@�33@�~�@���@Ӿw@��@��@�r�@��m@·+@̬@�\)@�M�@�G�@ȃ@ǥ�@���@ř�@�  @�^5@��u@�ȴ@��#@�O�@��9@�9X@��@���@���@�Ĝ@�Ĝ@�(�@���@�M�@�A�@���@��R@��\@�~�@�v�@�ff@�ff@�M�@��@���@�-@�@��@��P@��H@��-@�o@���@�S�@��@��@��+@���@���@�/@��@�j@�9X@�1@��m@��;@�ƨ@���@��@�dZ@�C�@�"�@��@�
=@��!@��#@��@� �@�M�@��@���@���@�`B@�/@���@��D@�1@�ƨ@�dZ@�C�@�+@��!@�@��T@�?}@�%@���@��D@�Z@�Q�@�I�@�(�@�b@��
@�+@�&�@�Z@���@�
=@��\@�{@���@�O�@�O�@�?}@���@��u@�9X@� �@��@�b@�  @��m@�ƨ@���@�33@�J@��@��#@���@���@�hs@�%@��@���@���@�z�@�9X@���@���@�+@���@��@�ȴ@��!@��\@�E�@��@��^@��@�X@��@���@��9@��D@�z�@�z�@�Z@�A�@�1'@� �@��@��P@��F@��P@�@��+@�v�@�n�@�ff@�^5@�M�@�=q@���@�@��h@��@�b@
=@~�R@~{@}��@}�T@}��@}�-@}�-@}��@}p�@}p�@}`B@}O�@}/@|�@|��@|Z@|�@|�@|�@{��@{ƨ@z�@z�\@zn�@z^5@zJ@y�@y��@y��@y��@x1'@ix�@ax�@[��@W;d@PĜ@J^5@A�@<��@6@0A�@+@%�h@!�7@��@  @S�@��@
�\@K�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A��A��
A��
A��
A���A���A�ĜA��jA��A�v�A�ĜA��HA��;A���A�E�A�A�p�A�+A���A���A�1A�7LA��7A��A�p�A��!A�ĜA�hsA�dZA��jA�~�A�C�A�+A�S�A�A�A�|�A���A�^5A�1'A�%A���A���A�XA���A�7LA�t�A��^A��A�\)A�5?A��#A�r�A��A�ĜA��hA���A�n�A�+A���A���A�=qA�(�A��A��uA� �A�33A�I�A�A���A��A��HA�{A��A��hA���A��;A� �A���A��A~��A~��A|I�Ax��At^5Ar��Aqp�Aq�As;dArbNAm��Ak33AiG�Ag�AfffAd��Ab��AbAa�^Aa33A`�A^ȴA^Q�A]��AZ��AV��AVbAUx�AT�AS��AR�9AQ��AQ&�AOO�AM��AK��AK�AIx�AG��AF�jAFZAE�AE��AE�AD9XACC�ABĜAA��A@=qA>�/A>M�A> �A>{A=�A=?}A<Q�A;;dA:bA9|�A9�A9��A:�A8�!A8A�A8(�A7��A7��A7��A7�A6n�A6�A5�#A5�A3��A3oA2��A2^5A2{A1�-A1?}A0�A/�wA/�A.ffA+��A*M�A)��A)+A(��A(ffA(bA'/A&v�A%��A$5?A#`BA#
=A"�jA"ffA!�A ��A (�A�#A�FAp�A+A��A�
A33AA�A��A�!A�TA�!AVA|�A�uA�A��AA�\A��Ax�AA�jAJA�A33A�`A��A�+AjAM�A�A��Ax�A
��A
1A	�A=qAXA$�A9XA+A{AG�@��F@��y@�E�@��#@���@��9@��F@�n�@��@��w@��R@���@��+@�^5@���@��@���@�V@� �@�|�@��H@�n�@��T@�X@�%@��/@���@�{@�dZ@�x�@���@�@�@� �@�V@�\)@���@���@۝�@���@ڗ�@�n�@�@٩�@�%@�33@�~�@���@Ӿw@��@��@�r�@��m@·+@̬@�\)@�M�@�G�@ȃ@ǥ�@���@ř�@�  @�^5@��u@�ȴ@��#@�O�@��9@�9X@��@���@���@�Ĝ@�Ĝ@�(�@���@�M�@�A�@���@��R@��\@�~�@�v�@�ff@�ff@�M�@��@���@�-@�@��@��P@��H@��-@�o@���@�S�@��@��@��+@���@���@�/@��@�j@�9X@�1@��m@��;@�ƨ@���@��@�dZ@�C�@�"�@��@�
=@��!@��#@��@� �@�M�@��@���@���@�`B@�/@���@��D@�1@�ƨ@�dZ@�C�@�+@��!@�@��T@�?}@�%@���@��D@�Z@�Q�@�I�@�(�@�b@��
@�+@�&�@�Z@���@�
=@��\@�{@���@�O�@�O�@�?}@���@��u@�9X@� �@��@�b@�  @��m@�ƨ@���@�33@�J@��@��#@���@���@�hs@�%@��@���@���@�z�@�9X@���@���@�+@���@��@�ȴ@��!@��\@�E�@��@��^@��@�X@��@���@��9@��D@�z�@�z�@�Z@�A�@�1'@� �@��@��P@��F@��P@�@��+@�v�@�n�@�ff@�^5@�M�@�=q@���@�@��h@��@�b@
=@~�R@~{@}��@}�T@}��@}�-@}�-@}��@}p�@}p�@}`B@}O�@}/@|�@|��@|Z@|�@|�@|�@{��@{ƨ@z�@z�\@zn�@z^5@zJ@y�@y��@y��@y��@x1'@ix�@ax�@[��@W;d@PĜ@J^5@A�@<��@6@0A�@+@%�h@!�7@��@  @S�@��@
�\@K�@3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�;B�;B�;B�5B�5B�;B�;B�;B�;B�;B�5B�5B�5B�5B�BB�HB�TB�TB�TB�mB��BoB(�BG�B[#BbNBaHB`BB^5BYBI�B:^B6FB.B#�B�BhB��B�yB�#B��B��BȴB�RB��B�oB�%B}�By�Bu�Bm�B`BBZBO�B5?B"�BPB�sB��Bn�BcTBW
BM�BB�B+B�BbBJBB
�B
�yB
�mB
�NB
�#B
��B
�#B
�ZB
�`B
�TB
�BBVBJB+B
��B
�B
�B
ǮB
�3B
��B
��B
|�B
E�B
oB
+B
B
VB
8RB
1'B	��B	�5B	ȴB	�3B	��B	��B	�B	� B	|�B	x�B	q�B	hsB	dZB	]/B	J�B	0!B	(�B	"�B	�B	�B	PB	1B	B�B�sB�/B�B��BŢBB��B�wB�jB�XB�FB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�BŢB��B�}B��B�}B�}B�}BĜBƨBŢBĜBB��B�wB�qB�jB�^B�XB�LB�?B�'B�B��B��B��B��B��B��B��B��B�{B�oB�\B�VB�PB�DB�=B�+B�B}�B{�Bz�By�Bw�Bt�Bq�Bl�BhsBcTB`BB]/B\)BVBO�BJ�BG�BE�BC�BA�B@�B?}B?}B>wB=qB<jB<jB<jB;dB;dB;dB;dB:^B:^B9XB8RB7LB6FB5?B5?B33B/B-B.B-B+B+B+B+B+B)�B(�B'�B%�B$�B#�B$�B$�B#�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB{B{B{B{BuBhBhBoBhBoBoBoBoBoBhBhBuBuBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B#�B$�B%�B&�B'�B(�B)�B.B.B-B.B0!B0!B49B9XB9XB:^B:^B:^B:^B:^B:^B:^B;dBA�B@�B@�BD�BD�BE�BN�BXB_;B`BBaHBbNBdZBffBgmBjBk�Bl�Bm�Bm�Bm�Bn�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bu�Bz�B}�B�7B�JB�JB�PB�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�3BBǮB��B��B��B�B�)B�BB�BB�BB�NB�fB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B	+B		7B	JB	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	&�B	)�B	+B	,B	,B	-B	.B	/B	/B	0!B	49B	33B	49B	:^B	@�B	A�B	A�B	B�B	C�B	C�B	D�B	F�B	H�B	J�B	N�B	T�B	YB	[#B	]/B	^5B	^5B	^5B	_;B	_;B	_;B	_;B	_;B	_;B	`BB	`BB	bNB	cTB	e`B	gmB	gmB	gmB	hsB	iyB	o�B	q�B	s�B	t�B	w�B	x�B	y�B	y�B	{�B	�B	��B	��B	�fB	�B	��B
bB
�B
'�B
2-B
;dB
A�B
I�B
O�B
W
B
\)B
aHB
gmB
l�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�$B�-B�6B�6B�6B�SB��BTB(�BG�B[Bb3Ba*B`(B^BX�BI�B:?B6)B-�B#�B�BIB��B�ZB�B��B̭BȒB�7B��B�LB�B}�By�Bu�BmqB` BY�BO�B5B"�B/B�QB��BnwBc5BV�BM�BBrB*�B�B@B+B�B
�B
�WB
�LB
�,B
�B
��B
�B
�:B
�AB
�1B
�B�B6B,BB
��B
�\B
��B
ǏB
�B
��B
��B
|�B
E�B
QB
B
 �B
9B
85B
1
B	��B	�B	șB	�B	��B	�hB	�B	�B	|�B	x�B	q�B	h_B	dDB	]B	J�B	0B	(�B	"�B	�B	qB	=B	B	 �B�B�_B�B�B��BőB�B�oB�dB�XB�FB�4B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BőB�uB�hB�nB�kB�iB�kBĉBƔBŎBĊB�|B�pB�fB�]B�XB�KB�DB�:B�.B�B�B��B��B��B��B��B��B�{B�oB�iB�^B�IB�BB�?B�2B�+B�B�B}�B{�Bz�By�Bw�Bt�Bq�BlyBhdBcEB`2B]B\BU�BO�BJ�BG�BE�BC�BA{B@tB?mB?mB>iB=aB<ZB<\B<YB;TB;VB;UB;TB:PB:NB9HB8CB7=B68B5/B51B3"B.�B- B.B,�B*�B*�B*�B*�B*�B)�B(�B'�B%�B$�B#�B$�B$�B#�B#�B!�B�B�B�BoBoBoB�BhB�BgB�B|BlBKBkBRBQBPBcB=B=BBBXBBBbB_B`BBB=B>BeBcBCBdBrBpBvBpBxBvB�B�B�B�B�B�B�B�B�B �B#�B$�B%�B&�B'�B(�B)�B-�B.B,�B-�B0B0B4%B9EB9FB:IB:JB:JB:JB:JB:LB:MB;PBArB@oB@lBD�BD�BE�BN�BW�B_$B`+Ba3Bb9BdBBfPBgVBjgBkoBltBmxBmzBmxBn�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bu�Bz�B}�B�B�0B�1B�9B�<B�DB�IB�ZB�mB�vB��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�sBǒB̲B��B��B��B�B�%B�$B�%B�1B�JB�bB�`B�hB�gB�gB�mB�rB�wB��B��B��B��B��B��B��B	�B	�B	B	B	B		B	*B	<B	UB	`B	gB	gB	nB	sB	�B	�B	�B	"�B	#�B	%�B	&�B	)�B	*�B	+�B	+�B	,�B	-�B	.�B	.�B	0B	4B	3B	4B	:=B	@cB	AjB	AjB	BoB	CuB	CuB	DyB	F�B	H�B	J�B	N�B	T�B	X�B	[B	]B	^B	^B	^B	_B	_B	_B	_B	_B	_B	` B	` B	b+B	c2B	e@B	gJB	gJB	gJB	hSB	iWB	o~B	q�B	s�B	t�B	w�B	x�B	y�B	y�B	{�B	��B	��B	��B	�?B	�B	��B
=B
�B
'�B
2B
;=B
A_B
I�B
O�B
V�B
[�B
aB
gEB
leB
p}B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708242016053117082420160531170824  AO  ARCAADJP                                                                    20150427021519    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150427021519  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150427021519  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170824  IP                  G�O�G�O�G�O�                