CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-02-26T03:47:54Z creation;2021-02-26T03:47:56Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  s(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ː   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210226034754  20210226035235  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               iA   JA                                  2B  A   APEX                            7906                            051216                          846 @�a@g(��1   @�aA <�v@4Qhr� ��d�/��w1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр DѼ�D�  D�@ DҀ D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�3D�@ Dـ D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�<�D�|�D޼�D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB\)B��B\)B\)B!\)B)\)B0��B8��BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخB��GB�B�B�B�B�B��B��B��C W
CW
CW
Cp�CW
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
C\=pC^W
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
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�8RC�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C�+�C��C��C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	�]D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D)D��D�D��D�D�]D�D��D�D��D�D��D�D��D�D��D�D��D]D��D�D��D�D��D)D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2)D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@�)DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN)DN�)DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr)Dr��Ds�Ds��Dt�Dt��Du�Du�)Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D�]D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�ND���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�D�ND���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�ND���D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D�ǮD�
�D�J�D���D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D��D�J�D���D�ǮD��D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D�D���D�
�D�J�DÊ�D���D�
�D�J�DĊ�D���D�
�D�J�DŊ�D���D�
�D�J�DƊ�D���D�
�D�J�DǊ�D���D�
�D�J�Dȇ�D���D�
�D�J�DɊ�D���D�
�D�G�Dʊ�D���D�
�D�J�Dˊ�D���D�
�D�J�D̊�D���D�
�D�J�D͊�D���D�
�D�J�DΊ�D���D�
�D�J�Dϊ�D���D�
�D�J�DЊ�D���D�
�D�J�Dъ�D�ǮD�
�D�J�DҊ�D���D�
�D�J�Dӊ�D��D�
�D�J�DԊ�D���D�
�D�J�DՊ�D���D�
�D�J�D֊�D���D�
�D�J�D׊�D���D�
�D�J�D؊�D��D�D�J�Dي�D���D�
�D�J�Dڊ�D��D�
�D�J�Dۊ�D���D�
�D�J�D܊�D���D�
�D�J�D݊�D���D�
�D�G�Dއ�D�ǮD�
�D�J�Dߊ�D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D��D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�G�D臮D���D�
�D�J�D��D���D�
�D�J�D�D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�W�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A��A��A��#A��/A��/A��HA��TA��`A��mA��mA��mA��mA��yA��mA��A��A��A��A��A��A��A��A��A��A���A���A���A���A�A�1A�VA�{A�(�A�A�A�jA˝�A�ƨA�oA��A��A�-A��A��A�VA���A��A˩�A���A�p�A�JA�?}Aɲ-A�p�A�^5A�G�A�;dA���A�z�A��A��A�ȴA�ĜA�=qA��A�v�A� �A��-A�VA��A�&�A� �A�;dA�VA�1'A�33A�A���A���A��7A�=qA��7A��7A�%A���A���A�;dA�|�A�O�A�$�A�=qA��;A�A�;dA��A���A��A�z�A��`A�7LA��^A��RA�7LA���A���A�33A��A��mA�JA�VA�dZA�/A�9XA��A�33A�9XA�wA}�-A{�7Ay;dAw?}AuoAq�TAoS�Am��Al��AkC�Ai��Aet�Ad��Ac�mAbI�A`��A_|�A]%AY|�AV�yARbNAOXAN^5ALbNAJ��AHz�AF�!AD�yAC+A@�HA?33A<�/A;|�A9��A8=qA6��A4v�A2�A1�A0�uA/�mA/�7A/?}A.��A.VA-\)A+%A)�TA(�A'�^A'dZA&�/A%x�A$JA#�A"�yA"=qA!p�A �DA�hAM�AdZA�Ar�A\)A�A��AhsA1'AhsA"�A�;A�AVA�FA�DAG�A�AbNA�A�A\)A;dA��A5?AVA
$�A	K�A��AbA�AjAl�AȴAE�A�^Al�A��AƨAoA ��@��;@���@��j@���@��m@�@��@�I�@���@���@��`@�dZ@���@�?}@웦@��@�@���@�V@�h@�j@�!@�@�bN@��@�-@�p�@�S�@ޟ�@�E�@ݩ�@�`B@�X@��@�j@��@٩�@�j@֏\@���@��@�1'@�;d@ҧ�@��@�&�@�b@�"�@��H@ΰ!@�V@ͩ�@���@�ƨ@��y@�M�@���@���@ȼj@�I�@��m@��H@�M�@���@Ų-@ċD@�Z@�9X@�@�~�@�J@��@��7@���@��9@�  @�|�@��@��!@�v�@��@��-@�G�@��@�V@��`@��@��w@�;d@�ȴ@�V@���@��h@�/@���@��D@� �@�ƨ@���@�+@�;d@���@���@�C�@�o@�v�@��/@���@��`@���@���@�Ĝ@��j@��D@��
@�K�@�;d@�S�@�@���@��!@��R@�5?@�J@��#@�p�@�%@��9@�A�@�1'@�(�@���@���@��w@�l�@���@��!@�5?@��-@�`B@�G�@�r�@� �@�b@��m@���@�;d@�
=@���@���@��y@���@�=q@�@���@��@��D@���@�S�@���@��+@��@��-@��7@��@�7L@�I�@�ƨ@���@�+@���@�@�@��-@��@���@�&�@���@��j@���@�Z@��
@��P@�S�@�@���@��+@�^5@�v�@�v�@���@���@�v�@�E�@��@�@���@�X@�/@��/@��D@�bN@�I�@�1@���@��P@�S�@�33@�+@��@���@��+@�v�@�^5@�=q@�$�@�@���@��^@���@��@��@��`@���@��@�Q�@�(�@��m@�|�@�\)@�
=@���@�V@�J@��@�@�hs@�?}@��@��j@�I�@�  @��@�;d@�
=@���@���@�ff@�E�@�J@��@��@�/@�%@��@���@���@�j@� �@��@�1@�1@�  @���@���@��@�l�@�+@���@�-@��T@��h@�G�@��j@��D@�j@�A�@��;@���@��@�t�@�33@��y@��R@�n�@�$�@��T@��-@���@�X@�G�@�V@���@��u@��@��;@��w@���@�l�@�C�@�o@��y@�ȴ@�~�@�5?@�{@��@��h@��7@�x�@�p�@�`B@�G�@�?}@��@��`@��9@��D@�z�@�r�@�Q�@�1'@� �@�  @���@�t�@��@��y@���@��@���@��@�V@��@��`@���@���@�Z@�9X@�@K�@~��@~5?@~{@}�-@}�h@}O�@}/@|�@|I�@{ƨ@{��@{o@z�H@z�@{@z��@z^5@y�^@y7L@x�9@xbN@x1'@x �@x �@x �@x �@xb@x  @w�w@w��@wl�@w+@w
=@v�R@v$�@u�-@up�@u`B@u?}@uV@t��@tZ@t9X@sƨ@s"�@r�H@rn�@r=q@q�@q�7@qX@p�`@pr�@p  @o;d@n��@nV@n$�@m@mO�@l�@l�D@lI�@l9X@kƨ@kS�@j�H@j~�@i��@ihs@h�`@h1'@g�P@g+@fff@f@eO�@d��@dI�@cƨ@cdZ@b�H@b�\@bM�@b�@a��@a�7@a7L@`r�@_�;@_�P@_\)@^�y@^�R@^��@^ff@^V@^$�@^@]�T@]@]?}@\�@\��@\�@\9X@[��@[t�@[C�@Z��@Z~�@Z=q@ZJ@Y��@Yx�@Y7L@Y%@X��@X�`@XĜ@X�9@XbN@Xb@WK�@V�R@V�+@VE�@V$�@V@U�-@U�h@U�h@U�h@U�@UO�@T�@TI�@T(�@T1@S��@S33@R��@R�!@R�!@R�!@RM�@Q��@Qx�@P�`@P�9@P�u@P �@Ol�@N�y@N��@N��@Nv�@NV@N$�@M�T@M��@M?}@L��@L�/@L�@Lj@L�@K�
@Kt�@K"�@K@J�!@Jn�@J-@I��@I�#@Ihs@H��@H�9@H �@G��@G�w@G�P@G;d@F��@FV@F@Ep�@E�@D��@D��@Dz�@D(�@C�
@C��@CdZ@B�@B�\@BM�@A��@A�7@Ax�@A&�@@ �@?�w@?�@?�@?\)@?�@>ȴ@>5?@=�-@=�@=?}@<�/@<�D@<j@<Z@<(�@;��@;�@;�@;�@;S�@:��@:M�@:-@:�@:J@9�#@9��@8�`@8�@81'@7�@7�@7�@7�;@7��@7�w@7��@7|�@7+@6ȴ@6��@6E�@6$�@5�T@5�@5`B@5�@4�j@4��@4j@4I�@41@3ƨ@3t�@2��@2��@2^5@1�@1�^@1�7@1G�@1&�@0Ĝ@0��@0A�@/�w@/l�@/+@.�y@.�R@.ff@.{@-�@-��@-p�@-?}@,�/@,�D@,z�@,j@,I�@,9X@,(�@+�F@+�@+dZ@+C�@+33@+@*��@*��@*�!@*~�@*�@)��@)hs@)7L@)�@)%@(��@(��@(�u@(A�@'�w@'�P@'l�@'K�@'
=@&ȴ@&V@&{@%�@%@%�@%O�@%V@$��@$�D@$Z@$Z@$I�@$9X@#��@#�F@#�@#C�@#"�@#@"��@"=q@!�#@!��@!X@!&�@ ��@ �9@ �u@ bN@  �@   @�@�@;d@��@�R@E�@{@@@@�@��@�h@`B@�@V@�@��@��@z�@j@I�@�@��@�
@t�@33@�@��@^5@-@J@�@��@7L@Ĝ@�9@��@r�@Q�@A�@ �@�;@�@��@|�@\)@+@�y@��@V@5?@$�@�@�T@@�h@O�@�@��@��@�/@�@z�@Z@I�@1@�
@�F@��@�@"�@�@�!@~�@~�@�@�#@�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A���A��A��A��#A��/A��/A��HA��TA��`A��mA��mA��mA��mA��yA��mA��A��A��A��A��A��A��A��A��A��A���A���A���A���A�A�1A�VA�{A�(�A�A�A�jA˝�A�ƨA�oA��A��A�-A��A��A�VA���A��A˩�A���A�p�A�JA�?}Aɲ-A�p�A�^5A�G�A�;dA���A�z�A��A��A�ȴA�ĜA�=qA��A�v�A� �A��-A�VA��A�&�A� �A�;dA�VA�1'A�33A�A���A���A��7A�=qA��7A��7A�%A���A���A�;dA�|�A�O�A�$�A�=qA��;A�A�;dA��A���A��A�z�A��`A�7LA��^A��RA�7LA���A���A�33A��A��mA�JA�VA�dZA�/A�9XA��A�33A�9XA�wA}�-A{�7Ay;dAw?}AuoAq�TAoS�Am��Al��AkC�Ai��Aet�Ad��Ac�mAbI�A`��A_|�A]%AY|�AV�yARbNAOXAN^5ALbNAJ��AHz�AF�!AD�yAC+A@�HA?33A<�/A;|�A9��A8=qA6��A4v�A2�A1�A0�uA/�mA/�7A/?}A.��A.VA-\)A+%A)�TA(�A'�^A'dZA&�/A%x�A$JA#�A"�yA"=qA!p�A �DA�hAM�AdZA�Ar�A\)A�A��AhsA1'AhsA"�A�;A�AVA�FA�DAG�A�AbNA�A�A\)A;dA��A5?AVA
$�A	K�A��AbA�AjAl�AȴAE�A�^Al�A��AƨAoA ��@��;@���@��j@���@��m@�@��@�I�@���@���@��`@�dZ@���@�?}@웦@��@�@���@�V@�h@�j@�!@�@�bN@��@�-@�p�@�S�@ޟ�@�E�@ݩ�@�`B@�X@��@�j@��@٩�@�j@֏\@���@��@�1'@�;d@ҧ�@��@�&�@�b@�"�@��H@ΰ!@�V@ͩ�@���@�ƨ@��y@�M�@���@���@ȼj@�I�@��m@��H@�M�@���@Ų-@ċD@�Z@�9X@�@�~�@�J@��@��7@���@��9@�  @�|�@��@��!@�v�@��@��-@�G�@��@�V@��`@��@��w@�;d@�ȴ@�V@���@��h@�/@���@��D@� �@�ƨ@���@�+@�;d@���@���@�C�@�o@�v�@��/@���@��`@���@���@�Ĝ@��j@��D@��
@�K�@�;d@�S�@�@���@��!@��R@�5?@�J@��#@�p�@�%@��9@�A�@�1'@�(�@���@���@��w@�l�@���@��!@�5?@��-@�`B@�G�@�r�@� �@�b@��m@���@�;d@�
=@���@���@��y@���@�=q@�@���@��@��D@���@�S�@���@��+@��@��-@��7@��@�7L@�I�@�ƨ@���@�+@���@�@�@��-@��@���@�&�@���@��j@���@�Z@��
@��P@�S�@�@���@��+@�^5@�v�@�v�@���@���@�v�@�E�@��@�@���@�X@�/@��/@��D@�bN@�I�@�1@���@��P@�S�@�33@�+@��@���@��+@�v�@�^5@�=q@�$�@�@���@��^@���@��@��@��`@���@��@�Q�@�(�@��m@�|�@�\)@�
=@���@�V@�J@��@�@�hs@�?}@��@��j@�I�@�  @��@�;d@�
=@���@���@�ff@�E�@�J@��@��@�/@�%@��@���@���@�j@� �@��@�1@�1@�  @���@���@��@�l�@�+@���@�-@��T@��h@�G�@��j@��D@�j@�A�@��;@���@��@�t�@�33@��y@��R@�n�@�$�@��T@��-@���@�X@�G�@�V@���@��u@��@��;@��w@���@�l�@�C�@�o@��y@�ȴ@�~�@�5?@�{@��@��h@��7@�x�@�p�@�`B@�G�@�?}@��@��`@��9@��D@�z�@�r�@�Q�@�1'@� �@�  @���@�t�@��@��y@���@��@���@��@�V@��@��`@���@���@�Z@�9X@�@K�@~��@~5?@~{@}�-@}�h@}O�@}/@|�@|I�@{ƨ@{��@{o@z�H@z�@{@z��@z^5@y�^@y7L@x�9@xbN@x1'@x �@x �@x �@x �@xb@x  @w�w@w��@wl�@w+@w
=@v�R@v$�@u�-@up�@u`B@u?}@uV@t��@tZ@t9X@sƨ@s"�@r�H@rn�@r=q@q�@q�7@qX@p�`@pr�@p  @o;d@n��@nV@n$�@m@mO�@l�@l�D@lI�@l9X@kƨ@kS�@j�H@j~�@i��@ihs@h�`@h1'@g�P@g+@fff@f@eO�@d��@dI�@cƨ@cdZ@b�H@b�\@bM�@b�@a��@a�7@a7L@`r�@_�;@_�P@_\)@^�y@^�R@^��@^ff@^V@^$�@^@]�T@]@]?}@\�@\��@\�@\9X@[��@[t�@[C�@Z��@Z~�@Z=q@ZJ@Y��@Yx�@Y7L@Y%@X��@X�`@XĜ@X�9@XbN@Xb@WK�@V�R@V�+@VE�@V$�@V@U�-@U�h@U�h@U�h@U�@UO�@T�@TI�@T(�@T1@S��@S33@R��@R�!@R�!@R�!@RM�@Q��@Qx�@P�`@P�9@P�u@P �@Ol�@N�y@N��@N��@Nv�@NV@N$�@M�T@M��@M?}@L��@L�/@L�@Lj@L�@K�
@Kt�@K"�@K@J�!@Jn�@J-@I��@I�#@Ihs@H��@H�9@H �@G��@G�w@G�P@G;d@F��@FV@F@Ep�@E�@D��@D��@Dz�@D(�@C�
@C��@CdZ@B�@B�\@BM�@A��@A�7@Ax�@A&�@@ �@?�w@?�@?�@?\)@?�@>ȴ@>5?@=�-@=�@=?}@<�/@<�D@<j@<Z@<(�@;��@;�@;�@;�@;S�@:��@:M�@:-@:�@:J@9�#@9��@8�`@8�@81'@7�@7�@7�@7�;@7��@7�w@7��@7|�@7+@6ȴ@6��@6E�@6$�@5�T@5�@5`B@5�@4�j@4��@4j@4I�@41@3ƨ@3t�@2��@2��@2^5@1�@1�^@1�7@1G�@1&�@0Ĝ@0��@0A�@/�w@/l�@/+@.�y@.�R@.ff@.{@-�@-��@-p�@-?}@,�/@,�D@,z�@,j@,I�@,9X@,(�@+�F@+�@+dZ@+C�@+33@+@*��@*��@*�!@*~�@*�@)��@)hs@)7L@)�@)%@(��@(��@(�u@(A�@'�w@'�P@'l�@'K�@'
=@&ȴ@&V@&{@%�@%@%�@%O�@%V@$��@$�D@$Z@$Z@$I�@$9X@#��@#�F@#�@#C�@#"�@#@"��@"=q@!�#@!��@!X@!&�@ ��@ �9@ �u@ bN@  �@   @�@�@;d@��@�R@E�@{@@@@�@��@�h@`B@�@V@�@��@��@z�@j@I�@�@��@�
@t�@33@�@��@^5@-@J@�@��@7L@Ĝ@�9@��@r�@Q�@A�@ �@�;@�@��@|�@\)@+@�y@��@V@5?@$�@�@�T@@�h@O�@�@��@��@�/@�@z�@Z@I�@1@�
@�F@��@�@"�@�@�!@~�@~�@�@�#@�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	^5B	_;B	_;B	^5B	^5B	^5B	^5B	^5B	_;B	`BB	`BB	aHB	`BB	`BB	`BB	aHB	`BB	aHB	aHB	`BB	aHB	bNB	dZB	dZB	dZB	cTB	cTB	cTB	dZB	gmB	k�B	r�B	x�B	}�B	�7B	�B	�B
+B
VB
k�B
�7B
�bB
�bB
��B
��B
��B
��B
��B
��B
��B
��B
�XB
ŢB
��B�BJBVB
��B
�TB
�By�B�B��B��B�B�B��B��B��B��B��B��B��B��B�
B�B��BBVB�BuB�BuBJBB�B�B�fB�NB�B��B��B��B�wB�B��B�uB�oB�7B�B~�Bx�BaHBH�B@�B=qB2-B/B$�BuBB
�)B
ƨB
�FB
��B
�oB
�B
p�B
k�B
]/B
O�B
<jB
1'B
%�B
hB
B	��B	��B	�B	�fB	ȴB	��B	�^B	�'B	�B	��B	�PB	x�B	gmB	W
B	C�B	?}B	9XB	2-B	,B	"�B	�B	oB		7B	B��B�B�B�`B�BB�B��B��BȴBǮBĜBĜBB��B�qB�XB�3B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�{B�bB�PB�PB�DB�7B�1B�7B�+B�+B�%B�%B�B�B�B�B�B�B�B�B�B�B�B~�B}�B|�B|�B�B�B�B�B�B~�B�B�B�B�B�%B�1B�+B�+B�+B�1B�1B�1B�=B�DB�PB�bB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�3B�3B�FB�LB�RB�XB�^B�dB�wBBÖBĜBǮBȴB��B��B��B��B��B��B��B��B�B�
B�B�#B�BB�ZB�`B�sB�B�B�B�B��B��B��B��B��B��B	B	B	+B	1B		7B	DB	JB	\B	oB	{B	�B	�B	�B	�B	!�B	"�B	"�B	#�B	&�B	,B	/B	2-B	49B	5?B	8RB	;dB	@�B	B�B	E�B	G�B	I�B	L�B	T�B	aHB	cTB	gmB	jB	l�B	jB	m�B	p�B	r�B	t�B	v�B	v�B	w�B	w�B	v�B	z�B	� B	�B	�B	�B	�+B	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�9B	�FB	�FB	�LB	�XB	�jB	�qB	�qB	�qB	�wB	�wB	��B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�NB	�TB	�ZB	�`B	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
W
B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	^5B	_;B	_;B	^5B	^5B	^5B	^5B	^5B	_;B	`BB	`BB	aHB	`BB	`BB	`BB	aHB	`BB	aHB	aHB	`BB	aHB	bNB	dZB	dZB	dZB	cTB	cTB	cTB	dZB	gmB	k�B	r�B	x�B	}�B	�7B	�B	�B
+B
VB
k�B
�7B
�bB
�bB
��B
��B
��B
��B
��B
��B
��B
��B
�XB
ŢB
��B�BJBVB
��B
�TB
�By�B�B��B��B�B�B��B��B��B��B��B��B��B��B�
B�B��BBVB�BuB�BuBJBB�B�B�fB�NB�B��B��B��B�wB�B��B�uB�oB�7B�B~�Bx�BaHBH�B@�B=qB2-B/B$�BuBB
�)B
ƨB
�FB
��B
�oB
�B
p�B
k�B
]/B
O�B
<jB
1'B
%�B
hB
B	��B	��B	�B	�fB	ȴB	��B	�^B	�'B	�B	��B	�PB	x�B	gmB	W
B	C�B	?}B	9XB	2-B	,B	"�B	�B	oB		7B	B��B�B�B�`B�BB�B��B��BȴBǮBĜBĜBB��B�qB�XB�3B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�{B�bB�PB�PB�DB�7B�1B�7B�+B�+B�%B�%B�B�B�B�B�B�B�B�B�B�B�B~�B}�B|�B|�B�B�B�B�B�B~�B�B�B�B�B�%B�1B�+B�+B�+B�1B�1B�1B�=B�DB�PB�bB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�3B�3B�FB�LB�RB�XB�^B�dB�wBBÖBĜBǮBȴB��B��B��B��B��B��B��B��B�B�
B�B�#B�BB�ZB�`B�sB�B�B�B�B��B��B��B��B��B��B	B	B	+B	1B		7B	DB	JB	\B	oB	{B	�B	�B	�B	�B	!�B	"�B	"�B	#�B	&�B	,B	/B	2-B	49B	5?B	8RB	;dB	@�B	B�B	E�B	G�B	I�B	L�B	T�B	aHB	cTB	gmB	jB	l�B	jB	m�B	p�B	r�B	t�B	v�B	v�B	w�B	w�B	v�B	z�B	� B	�B	�B	�B	�+B	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�9B	�FB	�FB	�LB	�XB	�jB	�qB	�qB	�qB	�wB	�wB	��B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�NB	�TB	�ZB	�`B	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
W
B
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210226124745  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210226034754  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210226034755  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210226034755  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210226034755  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210226034755  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210226034755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210226034755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210226034756  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210226034756                      G�O�G�O�G�O�                JA  ARUP                                                                        20210226035235                      G�O�G�O�G�O�                