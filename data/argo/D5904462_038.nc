CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-08T19:17:28Z AOML 3.0 creation; 2016-08-07T21:51:15Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150308191728  20160807145115  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               &A   AO  5287_9017_038                   2C  D   APEX                            6529                            072314                          846 @�>=!��1   @�>>UU@
@1�S����d��;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    &A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�fC  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D�&fD�I�D���D��fD���D�33D��fD���D��D�,�D��fD��3D�	�D�FfD�L�D��fD� D�@ D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!B)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮B��GBԮBخBܮB�B�B�B�B�B��B��B��C p�C=pCW
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
C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$�)D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn)Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy��D�1GD�T{D��{D��GD��D�>D��GD�׮D��D�7�D��GD��D�{D�QGD�W�D��GD��D�J�D�{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A΍PA΁A�~�A΁A΃A΃A·+AΏ\AΏ\AΏ\AΏ\AΏ\A΅A΅A·+A΍PAΑhA�hsA�I�A�\)A�l�A�bA�ƨA���A�ƨA̡�A̶FA̟�A̓uA̝�A�v�A���A˲-A�VA��A�%A��A��A��mA��TA���Aʥ�A�+Aɺ^A�v�A�K�A�7LA�oA��AȁA�~�A�ZA�oAÕ�A¬A7A�VA��mA�A���A�K�A�VA�ƨA�1'A�+A�$�A�  A�A�|�A��A���A��A���A�I�A��A��\A��
A��A��hA�/A�+A���A��DA�S�A�/A�JA��wA��yA��PA��jA���A�%A�"�A�~�A��^A�G�A���A��PA�\)A~��Ax1Ap�/Ajv�AgAc��AaXA_&�A[33AZAW�ASAQAM�AI�AG�
AF=qAEC�ACO�AAoA>I�A<�HA;K�A:9XA9��A8�yA6�/A5�FA4r�A0��A-�A,��A,�\A,5?A+A*JA'��A'�A&ZA#��A bNA��A^5A�!A��AK�A-Ax�A^5A1A�A9XA��A7LAjAx�A�DA��A�jAA�Ax�A
�A
  A	K�A�HA�AI�A��A�hAt�A��AA��AffAA��AdZA�AbNA��A��Ax�AhsAdZA��A��A��A�hA|�A|�A|�A|�A7LA�AAXA�A �D@��;@�S�@��y@�V@��@�Z@�t�@�l�@�+@�v�@�J@���@���@�%@��u@�ƨ@�@��@��@@�@�7L@��/@��@�
=@�J@��@���@�r�@��@�
=@�"�@�-@��@��@�hs@��`@�r�@���@�C�@⟾@�+@�J@�hs@�@��@�@��@�r�@�S�@ޗ�@ް!@�n�@��@���@݉7@݁@�x�@�O�@��@��`@���@���@ܣ�@�9X@�;d@�=q@�x�@�G�@���@�  @�l�@�K�@�"�@�@���@�^5@���@թ�@�hs@Ԭ@�9X@��@Ӆ@�~�@�hs@�Q�@�l�@Ώ\@��@Ͳ-@�p�@��@̼j@�Z@�  @ˍP@�t�@���@ʗ�@��@ɩ�@�/@���@���@ȓu@��@�|�@�33@�ȴ@�ff@���@ģ�@�1'@å�@�@���@�Z@��@�@�n�@�@���@�V@��/@�Ĝ@��u@��m@�t�@��!@�ff@�{@���@��7@�r�@�;d@�{@���@���@���@��@�`B@�bN@��w@�l�@�33@���@��h@���@�1@��m@��@���@�  @�1@�  @��@��@��@��@�C�@��@�@�=q@�%@��@�r�@�r�@��D@�j@�I�@�ƨ@���@��@��F@��P@�"�@�ȴ@��+@�ff@���@�bN@���@���@�=q@�{@��@��@���@�p�@��@���@�A�@���@�ȴ@��+@�^5@�@��^@���@�G�@���@���@�I�@�(�@�  @���@�l�@��P@��@�"�@�n�@�{@���@���@�$�@��@���@�&�@�%@���@��@�1'@��@�1@���@��m@�ƨ@��@�l�@��@��H@�~�@��@�@���@�p�@�7L@���@�r�@�1'@��@��w@�t�@�+@�
=@��y@��R@���@�ff@�J@��@���@��9@�Q�@�(�@�1@���@���@�K�@�+@�o@��y@���@��+@�V@�$�@���@�/@��`@��9@�bN@��@��F@�"�@�ȴ@�n�@�~�@��\@�^5@�=q@�5?@�-@�$�@�@���@�X@��@��j@���@�Z@�  @�5?@���@~��@w
=@n��@ihs@`A�@W�P@K�F@A�^@9��@3t�@-�h@(�9@%V@!7L@1@l�@��@%@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A΍PA΁A�~�A΁A΃A΃A·+AΏ\AΏ\AΏ\AΏ\AΏ\A΅A΅A·+A΍PAΑhA�hsA�I�A�\)A�l�A�bA�ƨA���A�ƨA̡�A̶FA̟�A̓uA̝�A�v�A���A˲-A�VA��A�%A��A��A��mA��TA���Aʥ�A�+Aɺ^A�v�A�K�A�7LA�oA��AȁA�~�A�ZA�oAÕ�A¬A7A�VA��mA�A���A�K�A�VA�ƨA�1'A�+A�$�A�  A�A�|�A��A���A��A���A�I�A��A��\A��
A��A��hA�/A�+A���A��DA�S�A�/A�JA��wA��yA��PA��jA���A�%A�"�A�~�A��^A�G�A���A��PA�\)A~��Ax1Ap�/Ajv�AgAc��AaXA_&�A[33AZAW�ASAQAM�AI�AG�
AF=qAEC�ACO�AAoA>I�A<�HA;K�A:9XA9��A8�yA6�/A5�FA4r�A0��A-�A,��A,�\A,5?A+A*JA'��A'�A&ZA#��A bNA��A^5A�!A��AK�A-Ax�A^5A1A�A9XA��A7LAjAx�A�DA��A�jAA�Ax�A
�A
  A	K�A�HA�AI�A��A�hAt�A��AA��AffAA��AdZA�AbNA��A��Ax�AhsAdZA��A��A��A�hA|�A|�A|�A|�A7LA�AAXA�A �D@��;@�S�@��y@�V@��@�Z@�t�@�l�@�+@�v�@�J@���@���@�%@��u@�ƨ@�@��@��@@�@�7L@��/@��@�
=@�J@��@���@�r�@��@�
=@�"�@�-@��@��@�hs@��`@�r�@���@�C�@⟾@�+@�J@�hs@�@��@�@��@�r�@�S�@ޗ�@ް!@�n�@��@���@݉7@݁@�x�@�O�@��@��`@���@���@ܣ�@�9X@�;d@�=q@�x�@�G�@���@�  @�l�@�K�@�"�@�@���@�^5@���@թ�@�hs@Ԭ@�9X@��@Ӆ@�~�@�hs@�Q�@�l�@Ώ\@��@Ͳ-@�p�@��@̼j@�Z@�  @ˍP@�t�@���@ʗ�@��@ɩ�@�/@���@���@ȓu@��@�|�@�33@�ȴ@�ff@���@ģ�@�1'@å�@�@���@�Z@��@�@�n�@�@���@�V@��/@�Ĝ@��u@��m@�t�@��!@�ff@�{@���@��7@�r�@�;d@�{@���@���@���@��@�`B@�bN@��w@�l�@�33@���@��h@���@�1@��m@��@���@�  @�1@�  @��@��@��@��@�C�@��@�@�=q@�%@��@�r�@�r�@��D@�j@�I�@�ƨ@���@��@��F@��P@�"�@�ȴ@��+@�ff@���@�bN@���@���@�=q@�{@��@��@���@�p�@��@���@�A�@���@�ȴ@��+@�^5@�@��^@���@�G�@���@���@�I�@�(�@�  @���@�l�@��P@��@�"�@�n�@�{@���@���@�$�@��@���@�&�@�%@���@��@�1'@��@�1@���@��m@�ƨ@��@�l�@��@��H@�~�@��@�@���@�p�@�7L@���@�r�@�1'@��@��w@�t�@�+@�
=@��y@��R@���@�ff@�J@��@���@��9@�Q�@�(�@�1@���@���@�K�@�+@�o@��y@���@��+@�V@�$�@���@�/@��`@��9@�bN@��@��F@�"�@�ȴ@�n�@�~�@��\@�^5@�=q@�5?@�-@�$�@�@���@�X@��@��j@���@�ZG�O�@�5?@���@~��@w
=@n��@ihs@`A�@W�P@K�F@A�^@9��@3t�@-�h@(�9@%V@!7L@1@l�@��@%@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
x�B
x�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
}�B
~�B
�B
�PB
��B
�B49B;dB`BB�B{�B`BBn�Bw�B�VB��B�'B��BƨB��B��B�BB�B��BBB%B%B	7B
=B�B$�B(�B)�B(�B%�B �B�B�B�B�B#�B+B'�B�BoB
=BPB�B#�B�BDB�NB�-B��B��B��B�{B��B�3B��B��B�ZB}�B2-B  B�NB/BoBVBVBJB	7BB��B��B��B�B^5BQ�B@�B�B
�/B
�B
��B
��B
cTB
.B	��B	��B	��B	��B	�%B	z�B	t�B	e`B	]/B	K�B	8RB	-B	�B	PB	B��B��B�B�NB�#B�
B��B��B��BȴBB�qB�FB�3B�9B�-B�'B�'B�B�B��B��B��B��B��B��B��B�{B�{B�{B�oB�bB�\B�PB�JB�=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�bB�VB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B�B��B�B�yB��B��B��B��B��B��B	B	B	B	B	
=B	
=B	
=B	DB	oB	{B	{B	{B	�B	 �B	 �B	�B	!�B	"�B	$�B	'�B	&�B	&�B	)�B	+B	+B	+B	,B	/B	49B	8RB	=qB	>wB	@�B	D�B	K�B	N�B	N�B	N�B	P�B	P�B	P�B	P�B	T�B	XB	\)B	`BB	cTB	e`B	l�B	q�B	u�B	v�B	w�B	z�B	}�B	�B	�+B	�JB	�JB	�VB	�VB	�\B	�bB	�uB	�{B	�{B	�uB	�{B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�!B	�!B	�!B	�'B	�-B	�3B	�?B	�FB	�LB	�XB	�dB	�jB	�jB	�jB	�qB	�wB	��B	��B	��B	��B	��B	B	ŢB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�B	�B	�
B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�fB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
DB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
{B
oB
�B
�B
%�B
)�B
-B
49B
?}B
F�B
L�B
Q�B
VB
\)B
`BB
e`B
iyB
o�B
s�B
v�B
x�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
x�B
x�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
}�B
~�B
��B
�:B
��B
�B4B;GB`(B�B{�B`$Bn~Bw�B�=B�dB�B�jBƍBʦBμB�(B�B��B�B�B
B
B	B
 BrB$�B(�B)�B(�B%�B �B�B�B�B�B#�B*�B'�B�BQB
!B3B�B#�BiB'B�.B�B��B��B��B�]B�zB�B��B��B�=B}�B2B��B�,B.�BPB5B3B*B	B�B��B̭B��B��B^BQ�B@eB�B
�B
��B
��B
�bB
c8B
-�B	��B	͸B	��B	�zB	�B	z�B	t�B	eJB	]B	K�B	8;B	,�B	�B	<B	�B��B��B�zB�:B�B��B��B��B��BȢB�}B�\B�4B� B�'B�B�B�B� B��B��B��B��B��B��B��B�lB�gB�hB�gB�^B�PB�JB�>B�8B�*B�yB��B��B��B��B��B��B��B��B��B��B��B��B�B�jB�YB�TB�OB�?B�ZB�gB�fB�mB�lB�xB�~B��B��B��B��B��B��B�BʪB��B�`B��B��B��B��B��B��B	 �B	�B	�B	�B	
!B	
!B	
"B	(B	QB	aB	bB	`B	lB	 �B	 �B	�B	!�B	"�B	$�B	'�B	&�B	&�B	)�B	*�B	*�B	*�B	+�B	/ B	4B	85B	=UB	>ZB	@gB	DB	K�B	N�B	N�B	N�B	P�B	P�B	P�B	P�B	T�B	W�B	\B	`#B	c7B	eCB	lkB	q�B	u�B	v�B	w�B	z�B	}�B	��B	�
B	�)B	�(B	�4B	�2B	�:B	�EB	�UB	�ZB	�ZB	�TB	�[B	�`B	�eB	�ZB	�fB	�eB	�dB	�lB	�qB	�mB	�sB	�kB	�lB	�oB	�rB	�rB	�qB	�sB	�lB	�mB	�lB	�gB	�kB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	��B	��B	�B	�
B	�B	�B	�#B	�(B	�7B	�CB	�HB	�IB	�FB	�NB	�VB	�^B	�eB	�eB	�eB	�_B	�kB	�B	ȓB	ɘB	ɗB	ɖB	ɖB	ɗB	ˤB	ͮB	ͰB	ηB	δB	̨B	ʟB	ɔB	ʞB	ʠB	ˣB	̩B	ͱB	ζB	ϺB	ϹB	ϻB	��B	��B	��B	��B	��B	εB	ͱB	ͯB	εB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�$B	�"B	�)B	�'B	�.B	�;B	�?B	�;B	�BB	�ZB	�ZB	�_B	�fB	�lB	�fB	�kB	�oB	�lB	�lB	�kB	�qB	�tB	�rB	�vB	�uB	�sB	�qB	�vB	�rB	�{B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
	B
!B
$B
(B
(B
*B
(B
(B
)B
*B
1B
0B
1B
0B
6G�O�B
FB
tB
�B
%�B
)�B
,�B
4B
?UB
F�B
L�B
Q�B
U�B
\ B
`B
e8B
iPB
osB
s�B
v�B
x�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.34 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451152016080714511520160807145115  AO  ARCAADJP                                                                    20150308191728    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150308191728  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150308191728  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145115  IP                  G�O�G�O�G�O�                