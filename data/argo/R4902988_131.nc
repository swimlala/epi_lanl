CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-12T10:02:23Z creation;2023-05-12T10:02:24Z conversion to V3.1      
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230512100223  20230512100413  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�*����1   @�*�Ѻ��@;���$��c�� ě�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   AA��A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�  B�33B�  B�  B�  B���B���B�  B�33C   C  C  C  C�fC
�C  C  C  C  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C��3C��3C�  C�  C�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C��3C�  C��C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D��D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D  D�fDfD� DfD� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"y�D"��D#� D$  D$� D%  D%�fD&  D&� D'  D'y�D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Dt��Duy�Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D�� D���D�  D�@ D�|�D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�<�D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�3D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D��3D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D���D�<�DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�C3DӀ D�� D�  D�@ DԀ DԼ�D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݃3D��3D�  D�@ Dހ D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�|�D�� D�3D�@ D� D�� D���D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D��3D�� D�  D�<�D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��]@���A z�A z�AB{A`z�A�
>A�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B/�RB8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�B�B�B�B�\B�B�B�\B�\B�\B��)B��)B�\B�B�C �C�C�C�C�C
!HC�C�C�C�C�C�C�C�C�C�C �C"!HC$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct!HCv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��
C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��
C��C��C��C��C��C��C��
C��C��C��
C��C��C��C��C��
C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��
C��
C��
C��C��C��C��C��
C��C��C��
C��
C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D��D��D	�D	��D
�D
��DRD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D��D{�D��D��D�D��D�D��D�D�RDRD��DRD��D�D��D�D��D�D��D�D��D�D��D�D�RD RD ��D!�D!��D"�D"{�D"��D#��D$�D$��D%�D%�RD&�D&��D'�D'{�D(�D(��D(��D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2RD2��D3�D3��D4�D4��D5�D5��D6RD6��D7�D7��D8�D8��D9�D9{�D:�D:��D;�D;��D<�D<��D=RD=��D>�D>��D?�D?��D@�D@{�DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DGRDG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR{�DS�DS��DT�DT��DU�DU��DV�DV��DWRDW��DXRDX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df{�Df��Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl�RDm�Dm��Dn�Dn��Dn��Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Ds��Dt{�Dt��Du{�Dv�Dv{�Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���D�@�D��)D���D� �D�@�D���D���D� �D�@�D�}�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���D�@�D���D���D� �D�=�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�=�D���D���D� �D�@�D�}�D���D� �D�@�D���D���D�)D�D)D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�=�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�=�D���D���D�)D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D��)D���D� �D�@�D���D��)D� �D�@�D�D���D� �D�@�DÀ�Dý�D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�Dǽ�D���D�=�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�=�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�D)DӀ�D���D� �D�@�DԀ�DԽ�D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D���D�@�D݄)D��)D� �D�@�Dހ�D���D� �D�@�D�}�D���D� �D�@�D���D���D���D�@�D��D��)D� �D�@�D��D���D� �D�@�D��D���D� �D�=�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D��)D� �D�@�D�}�D���D�)D�@�D��D���D���D�@�D��D���D� �D�=�D��D���D� �D�@�D�}�D���D� �D�@�D��D���D� �D�=�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D��)D�)D�D)D��)D���D� �D�=�D�}�D���D�)D�@�D���D���D� �D�@�D���D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�t�A�tA�u�A�sMA�W?A�JXA�?HA�($A��A�gmA���A��A���A���A���A�x8A�iyA�]�A�0!A�MjA�e�A�T,A�YA�|�A��8A���A�p�A��LA���A�5�A��TA���A�(XA���A�g�A�VA�AUA��A���A���A���A���A�
=A��A�'�A�VA��*A��_A�}VA���A��!A�.�A��A��A��;A��qA�aHA�/OA�[WA�aHA�_�A��]A�=qA��'A���A�l�A���A�o�A���A�+�A�~�A�5tA���A���A��BA���A�"�A���A��JA��A��\A�OA���A�E�A��PA�,A��sA���A��A�`�A�X�A���A�!A�уA�zA�I�A��A���A��{A�^�A�A���A�P�A� A��A�`A�r|A�O�A�&A��!A�"hA~�A}C�A{�eAz)�AyX�AxK�Aw1Au��Au7�At�At�kAtI�As�gAs7�Ao�pAk?}AiB[Af��Ab�AAa��A`�[A_��A^ĜA]�hA\��A\'�A[Q�A[�1AZ�jAYW�AW_pAV�=AUQ�AP]dAO��AO�7AOIRAO:*AN�AMݘAMH�AL�jAK�9AKbNAK$�AJ�AH�AGAE��AEADTaAD&�AC�.AB�ABA AA�tA?��A=�KA;�+A;A:��A9K^A7VA5�A4\�A4�A3l"A3A2u�A2`�A2;A1��A1;�A1�A0��A/��A.~�A,p�A+?A*��A*A)�A)�A(zA'��A&��A&��A&,=A%p;A%
=A$�.A$%A#�\A"��A!��A ��AT�A��A��AB�A�A��Aw�A[WA/�A��Aw2A)�AĜAVAA�A�	AkQA�A��A��A�A�A��A��A��A��A�A��AE9A�AA��AOvA��A�|AA A�A��A�EA
��A	c A]�A�]ACAA�A"�A��A �@��q@���@���@�2a@�G@���@�rG@���@��>@�@O@�N�@���@�U�@�4�@�z�@�x@엍@��@�1@��@�@@�d�@���@��y@�@�@��o@݅@�,�@��"@�PH@۩*@��)@�Q�@��@�  @�Ft@���@У@�Ov@��@�!�@���@��@�S�@ǟV@�4n@�.I@�l"@�ی@��r@�Y@�(�@��@�D�@��@�|�@�e@��_@��0@�͟@�Ov@�%�@�W?@��@���@�֡@��7@��@��h@��F@�e,@�%F@���@��@�W?@�֡@���@�S�@���@��@���@�c @��@���@�Y�@��@���@��L@���@��@��$@�$t@�-@�}�@�P�@�Q�@�RT@�O@�/�@��K@�Ta@���@�C@�U�@��@�~�@�8�@���@��I@��1@�� @� �@�Z�@�>�@� \@���@�^5@�1'@�_@��@��j@�� @��H@���@�H�@�	l@�w�@�� @��M@��@�	@���@�iD@��M@�~@�6z@��X@��z@�u�@�r�@�d�@�D�@�:�@�	�@���@��@�͟@���@���@���@���@���@�}�@�s@�\)@�+@��`@��@���@���@�RT@��@��@���@���@�~�@�j@���@�Ɇ@�M@��;@���@�a@�Mj@���@���@�z�@�H�@��@��
@���@��C@��M@�%@��E@��)@���@�ff@�N�@�A�@�+k@�4@��@���@��@��@���@�f�@�2a@��@�҉@��}@��o@�N�@��@��)@���@��@���@�|@���@�E�@�@��@\)@@O@Y@�@~�@~�s@~��@~��@~�@~~�@~V@~0U@~
�@}��@}�d@}��@}��@}�@|�@{�@{�@zi�@zTa@y�^@y^�@yQ�@y#�@x��@w�g@we�@wP�@w&@v�8@v@u=�@t��@toi@tC-@tb@s�a@sn/@r��@r��@r1�@q�@p[�@oخ@oU�@oJ#@o�@n�M@n�@n�@n�A@nZ�@n.�@m��@ma�@m4@k�;@k@O@jn�@jC�@j6�@i��@i�@iV@hی@hoi@h�@gJ#@f�\@fL0@f8�@f{@e�'@e8�@e�@dĜ@c�+@c.I@b�@b��@b6�@a��@`�@`e�@`,=@_��@_�A@_�A@_�@_�W@_�@_��@__p@_
=@^�X@^�F@^)�@]ϫ@]�@[�r@[�:@[Mj@[&@Z�@Z��@Z_@Y��@Y#�@X�|@X�p@X�o@X �@X�@X@We�@W6z@W1�@W+@V�@V�2@V��@VO@Uo @UX@U�@T�I@TXy@T �@S�g@S��@S��@Sx@Se�@S(@Rȴ@R�x@RM�@RJ@Q�o@Q�"@QJ�@Q@P�$@Py>@P`�@P?�@P�@O��@O��@O��@O�*@O�$@O
=@N��@N��@N��@N��@N�@N��@NW�@N0U@M��@M�@L�_@L*�@L  @Kƨ@KdZ@KO@K"�@J��@J+k@I��@I��@I�@I��@Ic@I(�@Hq@G�+@G�@G��@G��@GJ#@F�@F8�@E��@E^�@E+�@E+@E�@E�@D��@D�@C�
@C˒@C�[@Cl�@C8@B��@B�!@B��@Bd�@Be@A�@A��@A+�@@��@@��@@j@@�@?�&@?��@?�f@>�@>a|@>	@=�@=m]@=0�@=@@<�E@<��@<z�@<:�@<1'@< �@<  @;�@;��@;�w@;RT@;,�@:�y@:��@:Ta@9�@9��@9�@8�I@7�]@6�c@6v�@5�@5��@5\�@55�@4�@4Z@4*�@4x@3˒@31�@3�@2ȴ@2�@2��@2��@2��@2n�@2.�@1�D@1��@1f�@1G�@1-w@1#�@0�@0"h@0�@/��@/�r@/��@/��@.��@.��@.0U@-�-@-X@-q@,�)@,�4@,q@,Xy@,PH@,4n@+��@+��@+��@+��@+\)@+o@*��@*�h@*R�@)��@)ԕ@)��@)��@)[W@)�@)�@)%F@)&�@)V@(��@(�@($@(�@'ݘ@'��@'��@'Z�@'K�@'C@&�M@&��@&�X@&��@&i�@&@�@%��@%��@%c�@%X@%j@%k�@%\�@%^�@%`B@%c�@%`B@%Y�@%T�@%Y�@%Vm@%B�@%0�@%�@%�@$�@$��@$��@$K^@$%�@$@$  @#�6@#�@@#�$@#s@#S�@"��@"+k@!|@!#�@ ѷ@ ��@ j@ _@ :�@ 1@�@�$@�@n/@b�@a@S�@��@�@��@=q@�@��@��@�@�X@2a@�|@�[@��@c�@Z@9X@�@�@b�@(@�"@��@�A@p;@^5@Z�@1�@�N@��@�=@��@s�@F@!�@�@Ĝ@l"@H@A�@>B@4n@�@�g@��@v`@H�@o@�c@��@s�@E�@3�@�@��@��@c�@J�@�@w�@S�@Q�@<�@��@a@F�@9�@C@�@�m@L0@x�@V@�@�@��@j@j@G@�P@l�@iD@n/@l�@]�@�A@0U@!�@�@}�@Dg@�@�v@��@�@��@w�@e�@Xy@<�@4n@-�@x@�@خ@�@E9@
�\@
-@
�@	��@	��@	^�@		l@�K@�P@	@ی@��@��@c�@_@D�@H@C-@Ft@Ft@?�@1'@b@  @�}@��@��@�0@�w@�@�$@�:@�$@��@�:@��@�P@�	@�P@��@��@�P@�P@�{@s@s@j�@.I@͟@�+@+k@�@�@�@��@��@��@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�t�A�tA�u�A�sMA�W?A�JXA�?HA�($A��A�gmA���A��A���A���A���A�x8A�iyA�]�A�0!A�MjA�e�A�T,A�YA�|�A��8A���A�p�A��LA���A�5�A��TA���A�(XA���A�g�A�VA�AUA��A���A���A���A���A�
=A��A�'�A�VA��*A��_A�}VA���A��!A�.�A��A��A��;A��qA�aHA�/OA�[WA�aHA�_�A��]A�=qA��'A���A�l�A���A�o�A���A�+�A�~�A�5tA���A���A��BA���A�"�A���A��JA��A��\A�OA���A�E�A��PA�,A��sA���A��A�`�A�X�A���A�!A�уA�zA�I�A��A���A��{A�^�A�A���A�P�A� A��A�`A�r|A�O�A�&A��!A�"hA~�A}C�A{�eAz)�AyX�AxK�Aw1Au��Au7�At�At�kAtI�As�gAs7�Ao�pAk?}AiB[Af��Ab�AAa��A`�[A_��A^ĜA]�hA\��A\'�A[Q�A[�1AZ�jAYW�AW_pAV�=AUQ�AP]dAO��AO�7AOIRAO:*AN�AMݘAMH�AL�jAK�9AKbNAK$�AJ�AH�AGAE��AEADTaAD&�AC�.AB�ABA AA�tA?��A=�KA;�+A;A:��A9K^A7VA5�A4\�A4�A3l"A3A2u�A2`�A2;A1��A1;�A1�A0��A/��A.~�A,p�A+?A*��A*A)�A)�A(zA'��A&��A&��A&,=A%p;A%
=A$�.A$%A#�\A"��A!��A ��AT�A��A��AB�A�A��Aw�A[WA/�A��Aw2A)�AĜAVAA�A�	AkQA�A��A��A�A�A��A��A��A��A�A��AE9A�AA��AOvA��A�|AA A�A��A�EA
��A	c A]�A�]ACAA�A"�A��A �@��q@���@���@�2a@�G@���@�rG@���@��>@�@O@�N�@���@�U�@�4�@�z�@�x@엍@��@�1@��@�@@�d�@���@��y@�@�@��o@݅@�,�@��"@�PH@۩*@��)@�Q�@��@�  @�Ft@���@У@�Ov@��@�!�@���@��@�S�@ǟV@�4n@�.I@�l"@�ی@��r@�Y@�(�@��@�D�@��@�|�@�e@��_@��0@�͟@�Ov@�%�@�W?@��@���@�֡@��7@��@��h@��F@�e,@�%F@���@��@�W?@�֡@���@�S�@���@��@���@�c @��@���@�Y�@��@���@��L@���@��@��$@�$t@�-@�}�@�P�@�Q�@�RT@�O@�/�@��K@�Ta@���@�C@�U�@��@�~�@�8�@���@��I@��1@�� @� �@�Z�@�>�@� \@���@�^5@�1'@�_@��@��j@�� @��H@���@�H�@�	l@�w�@�� @��M@��@�	@���@�iD@��M@�~@�6z@��X@��z@�u�@�r�@�d�@�D�@�:�@�	�@���@��@�͟@���@���@���@���@���@�}�@�s@�\)@�+@��`@��@���@���@�RT@��@��@���@���@�~�@�j@���@�Ɇ@�M@��;@���@�a@�Mj@���@���@�z�@�H�@��@��
@���@��C@��M@�%@��E@��)@���@�ff@�N�@�A�@�+k@�4@��@���@��@��@���@�f�@�2a@��@�҉@��}@��o@�N�@��@��)@���@��@���@�|@���@�E�@�@��@\)@@O@Y@�@~�@~�s@~��@~��@~�@~~�@~V@~0U@~
�@}��@}�d@}��@}��@}�@|�@{�@{�@zi�@zTa@y�^@y^�@yQ�@y#�@x��@w�g@we�@wP�@w&@v�8@v@u=�@t��@toi@tC-@tb@s�a@sn/@r��@r��@r1�@q�@p[�@oخ@oU�@oJ#@o�@n�M@n�@n�@n�A@nZ�@n.�@m��@ma�@m4@k�;@k@O@jn�@jC�@j6�@i��@i�@iV@hی@hoi@h�@gJ#@f�\@fL0@f8�@f{@e�'@e8�@e�@dĜ@c�+@c.I@b�@b��@b6�@a��@`�@`e�@`,=@_��@_�A@_�A@_�@_�W@_�@_��@__p@_
=@^�X@^�F@^)�@]ϫ@]�@[�r@[�:@[Mj@[&@Z�@Z��@Z_@Y��@Y#�@X�|@X�p@X�o@X �@X�@X@We�@W6z@W1�@W+@V�@V�2@V��@VO@Uo @UX@U�@T�I@TXy@T �@S�g@S��@S��@Sx@Se�@S(@Rȴ@R�x@RM�@RJ@Q�o@Q�"@QJ�@Q@P�$@Py>@P`�@P?�@P�@O��@O��@O��@O�*@O�$@O
=@N��@N��@N��@N��@N�@N��@NW�@N0U@M��@M�@L�_@L*�@L  @Kƨ@KdZ@KO@K"�@J��@J+k@I��@I��@I�@I��@Ic@I(�@Hq@G�+@G�@G��@G��@GJ#@F�@F8�@E��@E^�@E+�@E+@E�@E�@D��@D�@C�
@C˒@C�[@Cl�@C8@B��@B�!@B��@Bd�@Be@A�@A��@A+�@@��@@��@@j@@�@?�&@?��@?�f@>�@>a|@>	@=�@=m]@=0�@=@@<�E@<��@<z�@<:�@<1'@< �@<  @;�@;��@;�w@;RT@;,�@:�y@:��@:Ta@9�@9��@9�@8�I@7�]@6�c@6v�@5�@5��@5\�@55�@4�@4Z@4*�@4x@3˒@31�@3�@2ȴ@2�@2��@2��@2��@2n�@2.�@1�D@1��@1f�@1G�@1-w@1#�@0�@0"h@0�@/��@/�r@/��@/��@.��@.��@.0U@-�-@-X@-q@,�)@,�4@,q@,Xy@,PH@,4n@+��@+��@+��@+��@+\)@+o@*��@*�h@*R�@)��@)ԕ@)��@)��@)[W@)�@)�@)%F@)&�@)V@(��@(�@($@(�@'ݘ@'��@'��@'Z�@'K�@'C@&�M@&��@&�X@&��@&i�@&@�@%��@%��@%c�@%X@%j@%k�@%\�@%^�@%`B@%c�@%`B@%Y�@%T�@%Y�@%Vm@%B�@%0�@%�@%�@$�@$��@$��@$K^@$%�@$@$  @#�6@#�@@#�$@#s@#S�@"��@"+k@!|@!#�@ ѷ@ ��@ j@ _@ :�@ 1@�@�$@�@n/@b�@a@S�@��@�@��@=q@�@��@��@�@�X@2a@�|@�[@��@c�@Z@9X@�@�@b�@(@�"@��@�A@p;@^5@Z�@1�@�N@��@�=@��@s�@F@!�@�@Ĝ@l"@H@A�@>B@4n@�@�g@��@v`@H�@o@�c@��@s�@E�@3�@�@��@��@c�@J�@�@w�@S�@Q�@<�@��@a@F�@9�@C@�@�m@L0@x�@V@�@�@��@j@j@G@�P@l�@iD@n/@l�@]�@�A@0U@!�@�@}�@Dg@�@�v@��@�@��@w�@e�@Xy@<�@4n@-�@x@�@خ@�@E9@
�\@
-@
�@	��@	��@	^�@		l@�K@�P@	@ی@��@��@c�@_@D�@H@C-@Ft@Ft@?�@1'@b@  @�}@��@��@�0@�w@�@�$@�:@�$@��@�:@��@�P@�	@�P@��@��@�P@�P@�{@s@s@j�@.I@͟@�+@+k@�@�@�@��@��@��@j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BU�BUBT�BU2BT�BT�BS�BS�BR�BR:BP�BMjBLdBNVBUgB_�B`\B`B_�B_�Bj�BxRB�1B��B��B�+B�NB�2B�-B��B�ABu�Bq�Bt�BraBu%Bv`BvFB�hB��B�
B��B��B�=B�EB}�Bj�Bd�Bh�BhXBabBZ�BW�B`\Bm]Bl�BbNBd@BeFBxRB{�Bz�Bp�B\�BL~B8�B.�B�B,BtB�B��BچB�\B�iB�xB�DB�dB��B��B�Bp�B6�B/�B/�B$tBWBuB�cB��B�:B��BBv�Bq'Bk�Bg�Bf�B`�B^5B\)BW�BP�BJ�BEBA�B8lB(sB�B
�HB
�B
�oB
�0B
�-B
��B
�hB
��B
��B
��B
��B
�UB
�B
�B
��B
��B
�B
�B
r|B
e�B
WYB
F�B
<jB
8�B
4�B
.B
(�B
$B
!�B
)B
/B
+B
�B
	�B
9B
�B	�B	��B	�B	��B	�wB	��B	�B	�B	�fB	�TB	�;B	�IB	�_B	�,B	ˬB	��B	�B	��B	��B	��B	��B	�GB	�OB	��B	��B	��B	��B	��B	�VB	��B	�iB	{dB	y�B	xB	v�B	tnB	s�B	r�B	qB	pB	n}B	m]B	i�B	ezB	_�B	Z�B	YKB	ZB	WsB	W
B	WYB	RoB	Q�B	PB	O�B	M6B	IlB	F�B	@�B	;�B	88B	3B	1'B	(XB	%�B	#nB	!|B	 �B	 �B	B	�B	B	dB	CB	=B	�B	1B	EB	�B	sB	SB	,B	B	�B	\B	�B	
#B		�B		7B	�B	[B	�B��B��B��B�$B��B�TB��B�;B�B�5B�6B�yB�B�B�-B��B��B��B�@BҽB� B�HB�BBΊB�6BʦB��B��BȀBǔBƎB�%B��B�3B�[B�B�iB��B�(B�B�qB��B�B��B��B�B�B�XB��B��B��B�B��B��B�B�2B�`B��B��B�?B�tB��B��B�zB��B��B��B��B��B��B�2B��B�XB��B��B��B�cB�B�;B�;B��B�oB�%B��BǮB�=B��B�{B�mB��B�?B��B�B�)B�B�dB�jB�B�B��B�nB��B��B�B��B��B��B�*B��B�=B�wB�[B��B�9B�9B�B�B�9B��B��B�RB��B	mB	�B	�B	(B	�B	oB	oB	TB	2B	1B	�B	B	/B	�B	 B	!-B	!�B	"hB	"�B	# B	$�B	&2B	'�B	,=B	0�B	8B	9�B	?}B	CB	D�B	H�B	O�B	V�B	Z�B	]B	_�B	`'B	`�B	b�B	b�B	d�B	gmB	j�B	l�B	mCB	m)B	oiB	x8B	{0B	|B	|�B	}�B	HB	��B	�KB	��B	��B	�&B	��B	��B	�B	�	B	��B	��B	�\B	��B	�0B	�WB	�aB	��B	�LB	� B	�UB	��B	ðB	�zB	�1B	ȀB	ɆB	�B	�BB	�}B	��B	�TB	ԕB	�gB	��B	֡B	�sB	�sB	��B	�B	��B	�kB	��B	��B	��B	�B	�%B	�FB	��B	�XB	�*B	�^B	�B	��B	��B
GB
�B
�B

#B
^B
�B
dB
�B
�B
B
�B
�B
�B
VB
�B
�B
hB
 B
�B
�B
@B
�B
�B
�B
�B
 vB
 �B
#TB
$B
$�B
%�B
(>B
+B
,"B
,WB
,�B
-]B
1vB
4TB
5�B
7�B
8B
8�B
9�B
;dB
=<B
>�B
@iB
A�B
GEB
IB
J�B
J�B
K�B
LB
L~B
MB
M�B
N<B
N�B
PbB
P�B
P�B
T�B
VB
X�B
ZkB
Z�B
]/B
^�B
_!B
_�B
`�B
a�B
d&B
f2B
gB
g�B
hXB
i_B
jKB
k6B
l�B
pB
s�B
v+B
v�B
w�B
y	B
z�B
}�B
�B
��B
�{B
��B
�B
�MB
��B
��B
�B
�fB
��B
��B
�^B
�0B
��B
��B
��B
�&B
�@B
��B
��B
��B
�sB
�yB
��B
�1B
�B
�WB
�qB
��B
��B
�5B
�B
�5B
��B
��B
��B
��B
�nB
��B
��B
��B
��B
�B
�$B
��B
�B
�DB
�yB
��B
�6B
��B
��B
�B
�B
�IB
��B
�5B
��B
�oB
�oB
��B
�[B
��B
��B
��B
�B
�B
��B
�9B
�9B
�B
��B
��B
��B
��B
��B
�zB
�LB
�lB
�rB
��B
�DB
��B
�0B
�B
��B
��B
�(B
�(B
�]B
�wB
��B
��B
�UB
�[B
B
�B
�{B
��B
�%B
�YB
�?B
ƎB
�B
�+B
�+B
�EB
�1B
�B
ɆB
ɆB
��B
ʦB
�B
ˬB
��B
��B
�0B
��B
�B
�B
ΥB
�(B
�vB
��B
бB
��B
�NB
уB
��B
��B
�aB
�2B
�B
�SB
�mB
��B
�$B
��B
�yB
�_B
�yB
��B
��B
��B
�B
��B
�B
ڠB
�=B
��B
ܒB
��B
�OB
�;B
�vB
�NB
�B
�&B
�ZB
��B
�B
�2B
�B
��B
�B
�RB
��B
��B
�_B
�yB
�_B
�B
��B
��B
�eB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�;B
��B
�B
�B
�|B
�B
�MB
�B
��B
��B
��B
�TB
��B
��B
��B
�B
��B
��B
��B
��B
�LB
�fB
��B
��B
�lB
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
��B
�B
�0B
�B
�dB
��B
�B
�6B
�PB
��B
��B
�"B
�B
�(B
�wB
��B
�]B
�wB
��B
��B
�wB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�.B
�cB
��B
��B OB �B �B �B B;BUB�B�B�B{BgB�BSB�B�B�BYB�BzBzB�B�B�B�BB�B�B	7B	�B	�B	�B
	B
=B
#B
�B^B�B�BJB0B�B~B~B�B�BB"B�B�B�B�B�B�B�B�B�B�B.BHB}B}BBhBNBNB�B�BB:B�B�B&BuB�BFB{B�B�B�B�BBBSB$B
B�B�B�B�B�B�B�B�B�BeB�BqB�B�BB]B)BB�B�B�B�B�BdB!BVBpB�B �B �B!-B!bB!�B!�B!�B!�B"B"B"hB"NB"NB"�B"�B"�B#B#�B%B%�B%�B%�B&LB&�B'�B'�B'RB'B'�B(
B(
B(>B($B(sB(XB(sB(sB(XB(sB(�B(�B(�B)*B)DB)*B)_B)DB)_B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B*KB*�B+6B+�B+�B+�B+�B,B,WB,WB,�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  BU�BUBT�BU2BT�BT�BS�BS�BR�BR:BP�BMjBLdBNVBUgB_�B`\B`B_�B_�Bj�BxRB�1B��B��B�+B�NB�2B�-B��B�ABu�Bq�Bt�BraBu%Bv`BvFB�hB��B�
B��B��B�=B�EB}�Bj�Bd�Bh�BhXBabBZ�BW�B`\Bm]Bl�BbNBd@BeFBxRB{�Bz�Bp�B\�BL~B8�B.�B�B,BtB�B��BچB�\B�iB�xB�DB�dB��B��B�Bp�B6�B/�B/�B$tBWBuB�cB��B�:B��BBv�Bq'Bk�Bg�Bf�B`�B^5B\)BW�BP�BJ�BEBA�B8lB(sB�B
�HB
�B
�oB
�0B
�-B
��B
�hB
��B
��B
��B
��B
�UB
�B
�B
��B
��B
�B
�B
r|B
e�B
WYB
F�B
<jB
8�B
4�B
.B
(�B
$B
!�B
)B
/B
+B
�B
	�B
9B
�B	�B	��B	�B	��B	�wB	��B	�B	�B	�fB	�TB	�;B	�IB	�_B	�,B	ˬB	��B	�B	��B	��B	��B	��B	�GB	�OB	��B	��B	��B	��B	��B	�VB	��B	�iB	{dB	y�B	xB	v�B	tnB	s�B	r�B	qB	pB	n}B	m]B	i�B	ezB	_�B	Z�B	YKB	ZB	WsB	W
B	WYB	RoB	Q�B	PB	O�B	M6B	IlB	F�B	@�B	;�B	88B	3B	1'B	(XB	%�B	#nB	!|B	 �B	 �B	B	�B	B	dB	CB	=B	�B	1B	EB	�B	sB	SB	,B	B	�B	\B	�B	
#B		�B		7B	�B	[B	�B��B��B��B�$B��B�TB��B�;B�B�5B�6B�yB�B�B�-B��B��B��B�@BҽB� B�HB�BBΊB�6BʦB��B��BȀBǔBƎB�%B��B�3B�[B�B�iB��B�(B�B�qB��B�B��B��B�B�B�XB��B��B��B�B��B��B�B�2B�`B��B��B�?B�tB��B��B�zB��B��B��B��B��B��B�2B��B�XB��B��B��B�cB�B�;B�;B��B�oB�%B��BǮB�=B��B�{B�mB��B�?B��B�B�)B�B�dB�jB�B�B��B�nB��B��B�B��B��B��B�*B��B�=B�wB�[B��B�9B�9B�B�B�9B��B��B�RB��B	mB	�B	�B	(B	�B	oB	oB	TB	2B	1B	�B	B	/B	�B	 B	!-B	!�B	"hB	"�B	# B	$�B	&2B	'�B	,=B	0�B	8B	9�B	?}B	CB	D�B	H�B	O�B	V�B	Z�B	]B	_�B	`'B	`�B	b�B	b�B	d�B	gmB	j�B	l�B	mCB	m)B	oiB	x8B	{0B	|B	|�B	}�B	HB	��B	�KB	��B	��B	�&B	��B	��B	�B	�	B	��B	��B	�\B	��B	�0B	�WB	�aB	��B	�LB	� B	�UB	��B	ðB	�zB	�1B	ȀB	ɆB	�B	�BB	�}B	��B	�TB	ԕB	�gB	��B	֡B	�sB	�sB	��B	�B	��B	�kB	��B	��B	��B	�B	�%B	�FB	��B	�XB	�*B	�^B	�B	��B	��B
GB
�B
�B

#B
^B
�B
dB
�B
�B
B
�B
�B
�B
VB
�B
�B
hB
 B
�B
�B
@B
�B
�B
�B
�B
 vB
 �B
#TB
$B
$�B
%�B
(>B
+B
,"B
,WB
,�B
-]B
1vB
4TB
5�B
7�B
8B
8�B
9�B
;dB
=<B
>�B
@iB
A�B
GEB
IB
J�B
J�B
K�B
LB
L~B
MB
M�B
N<B
N�B
PbB
P�B
P�B
T�B
VB
X�B
ZkB
Z�B
]/B
^�B
_!B
_�B
`�B
a�B
d&B
f2B
gB
g�B
hXB
i_B
jKB
k6B
l�B
pB
s�B
v+B
v�B
w�B
y	B
z�B
}�B
�B
��B
�{B
��B
�B
�MB
��B
��B
�B
�fB
��B
��B
�^B
�0B
��B
��B
��B
�&B
�@B
��B
��B
��B
�sB
�yB
��B
�1B
�B
�WB
�qB
��B
��B
�5B
�B
�5B
��B
��B
��B
��B
�nB
��B
��B
��B
��B
�B
�$B
��B
�B
�DB
�yB
��B
�6B
��B
��B
�B
�B
�IB
��B
�5B
��B
�oB
�oB
��B
�[B
��B
��B
��B
�B
�B
��B
�9B
�9B
�B
��B
��B
��B
��B
��B
�zB
�LB
�lB
�rB
��B
�DB
��B
�0B
�B
��B
��B
�(B
�(B
�]B
�wB
��B
��B
�UB
�[B
B
�B
�{B
��B
�%B
�YB
�?B
ƎB
�B
�+B
�+B
�EB
�1B
�B
ɆB
ɆB
��B
ʦB
�B
ˬB
��B
��B
�0B
��B
�B
�B
ΥB
�(B
�vB
��B
бB
��B
�NB
уB
��B
��B
�aB
�2B
�B
�SB
�mB
��B
�$B
��B
�yB
�_B
�yB
��B
��B
��B
�B
��B
�B
ڠB
�=B
��B
ܒB
��B
�OB
�;B
�vB
�NB
�B
�&B
�ZB
��B
�B
�2B
�B
��B
�B
�RB
��B
��B
�_B
�yB
�_B
�B
��B
��B
�eB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�;B
��B
�B
�B
�|B
�B
�MB
�B
��B
��B
��B
�TB
��B
��B
��B
�B
��B
��B
��B
��B
�LB
�fB
��B
��B
�lB
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
��B
�B
�0B
�B
�dB
��B
�B
�6B
�PB
��B
��B
�"B
�B
�(B
�wB
��B
�]B
�wB
��B
��B
�wB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�.B
�cB
��B
��B OB �B �B �B B;BUB�B�B�B{BgB�BSB�B�B�BYB�BzBzB�B�B�B�BB�B�B	7B	�B	�B	�B
	B
=B
#B
�B^B�B�BJB0B�B~B~B�B�BB"B�B�B�B�B�B�B�B�B�B�B.BHB}B}BBhBNBNB�B�BB:B�B�B&BuB�BFB{B�B�B�B�BBBSB$B
B�B�B�B�B�B�B�B�B�BeB�BqB�B�BB]B)BB�B�B�B�B�BdB!BVBpB�B �B �B!-B!bB!�B!�B!�B!�B"B"B"hB"NB"NB"�B"�B"�B#B#�B%B%�B%�B%�B&LB&�B'�B'�B'RB'B'�B(
B(
B(>B($B(sB(XB(sB(sB(XB(sB(�B(�B(�B)*B)DB)*B)_B)DB)_B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B*KB*�B+6B+�B+�B+�B+�B,B,WB,WB,�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230512100221  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230512100223  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230512100223  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230512100224                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230512100224  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230512100224  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230512100413                      G�O�G�O�G�O�                