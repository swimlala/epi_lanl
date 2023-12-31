CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T22:55:59Z creation;2022-06-04T22:56:00Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604225559  20220609221504  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ^A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��i�Q�1   @��iaG�@<��G�{�c���n�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  Aa��A�  A���A���A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B7��B@  BH  BP  BW��B_��Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC�fC  C
  C  C�C  C  C  C  C�fC  C  C  C   C"  C$  C%�fC(  C*�C,  C.  C0  C2  C4�C6  C7�fC:�C<  C>  C@  CB�CD  CF  CH  CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��3C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C��C�  C��C��C�  C��3C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  Dy�D��Dy�D  D� D  D� D��Dy�D  D� D��Dy�D  D� D  D� D  D� D  D� D��D� D  Dy�D  D� D  D� D  D�fDfD� DfD�fD  D�fDfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&�fD'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DVfDV�fDWfDW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dxy�Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  Dy�D��D�@ D��3D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�<�D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�3D�C3D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D��3D�3D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�|�D���D�  D�@ D�� D���D�  D�C3D��3D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�|�D���D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D D�� D�  D�@ D�|�Dü�D���D�@ DĀ D�� D�  D�C3Dŀ Dż�D���D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�3D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�C3DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�<�D�|�D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D��3D�3D�@ D�� D�� D�  D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��]@���A z�A z�A@z�Ab{A�=qA�
>A�
>A�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B7�RB@�BH�BP�BW�RB_�RBh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B��)B��)B�\B�\B�\B�\B�B�B�\B�\B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C!HC�C�C�C�C�C�C�C�C �C"�C$�C%�C(�C*!HC,�C.�C0�C2�C4!HC6�C7�C:!HC<�C>�C@�CB!HCD�CF�CH�CJ!HCL!HCN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cq�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D{�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D�RD�D{�D��D{�D�D��D�D��D��D{�D�D��D��D{�D�D��D�D��D�D��D�D��D��D��D�D{�D�D��D�D��D�D�RDRD��DRD�RD�D�RDRD��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&RD&�RD'RD'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3RD3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DSRDS��DT�DT��DU�DU��DVRDV�RDWRDW��DX�DX��DX��DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D]��D^��D_�D_��D_��D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx{�Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~�RD�D{�D��D�@�D��)D���D� �D�@�D���D���D���D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D��)D�)D�@�D�}�D���D� �D�@�D�}�D���D� �D�=�D���D���D���D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D�}�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�=�D���D���D�)D�D)D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D�}�D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D��)D� �D�@�D���D��)D�)D�@�D���D���D� �D�@�D���D���D���D�@�D�}�D���D� �D�@�D���D���D� �D�D)D��)D���D���D�=�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D��)D� �D�@�D�}�D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���D�@�D���D���D� �D�@�D�D���D� �D�@�D�}�Dý�D���D�@�DĀ�D���D� �D�D)Dŀ�DŽ�D���D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D�)D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�=�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�=�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�D)DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D���D�=�D�}�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�=�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D���D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D���D�@�D��D���D� �D�@�D��D���D� �D�D)D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�D)D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D���D�@�D��)D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D���D�@�D��)D��)D�)D�@�D���D���D� �D�=�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�.IA�3�A�4�A�-A�(�A�'�A�+6A�3�A�7LA�6�A�6FA�6FA�4A���A��-A�v�A��bA���A�<�A���A��A�x8A��nA���A�_A���A��A��A��JA���A�T�A���A�>BA��GA���A��A�F�A�`vA���A��`A�O�A��A��
A��:A��A���A��TA��PA�EA�x�A�I�A��FA��rA���A�QNA�$�A���A���A�IRA�(�A�SA��A��A��A�h>A���A�+�A��0A�($A�JXA��\A��uA���A�A�ǮA�cTA��7A��zA�  A�l�A���A�C�A��rA���A��A��`A���A���A�d�A�[#A�1�A�� A��RA��A�@A�z�A� A���A���A���A�YA�A}�A|��Az֡Aw%FAt��Ar� Ap�=An�Ak��Ai��Ah�Ag7�Ad�NAbbAaOvAaL�Aa;dA`�HA_�DA]��A[خA[�AY�AY~AXѷAX�=AX�AW�AT�mAS�zAS�AR�XARzxAQ��APخAPJ#AP33AP-wAP!-AP;AO��AO@�AN�AN�AN�AL]�AJ)�AH��AGc AE^5AE�AD��AD'RAB�AA��A@��A@oA?�`A?��A>L0A=MA=�A<�A<��A<6A;��A:��A8�`A8;dA7ĜA6��A5��A4�A3ɆA2�aA1�A1=A0�A0�$A0�4A0��A0� A0/�A/��A.�9A.\�A.A,�pA+�A+&A*�A)�A)�A)(�A'��A&�+A%�NA$��A$B[A#�'A#�A!FtA m�A�A��A.�A�UA!ATaA��AFtA�A��A��A�A>�A�UAe�A�A�A�>A;A�A�eA?A�2A;�AE�A�A�A2�A��A&�ArGA(AAW?Ay>A
�!A	�A	JAZ�A��A!�A��AsAVmA#�A��A�4AC�A��A�|A��A.IA��A 8@��,@��v@��@�U�@��T@��4@�j@�$@�0@�@�i�@��@�q@�k�@���@�+@�?�@�@栐@䟾@�@�҉@ߖS@�x@��,@��m@�!@պ^@ԗ�@� \@���@��@�=�@��@��.@�n/@ʬ@�<�@�ԕ@�q@���@Ƙ_@�xl@�*0@�M@�{�@�.I@���@�l"@�@�kQ@��8@��@�bN@�R�@�	@�u@��	@���@�@�@��@��~@��8@�l"@� �@�X@��d@��5@�L�@���@�J#@���@��@��A@��@�˒@��$@�4@��q@��o@��3@�Q�@�͟@��o@�?�@��D@�v`@�6z@���@�G@�S�@��$@�@��+@��o@�خ@��	@�V@�ԕ@�x@�4�@�S@���@��=@��L@�c @�N�@��@�2a@���@���@��-@���@�z@�8�@���@���@�[W@�/@��.@���@���@���@�4@�@@��E@��E@��@�?@�U�@�� @�}V@�,=@���@���@��S@�g�@�O�@��5@��B@�bN@�e@��@��9@��n@��'@�(@��\@�x@�ԕ@�c@�
=@��6@�bN@��>@�=@��P@��@���@�p;@��@��r@��.@�h
@���@�e,@�a@�zx@�;d@��L@�C-@�B[@�9X@�J@��@���@��j@���@�}�@�(�@�ی@���@���@���@���@��4@���@�� @���@�u%@�O@�S�@��]@���@�tT@�>B@�M�@�4n@�b@v`@�@~�6@~�A@~h
@~O@~�@}�.@}��@}[W@|�@{&@{1�@{Y@z�X@z�@zv�@y��@yS&@y!�@x�@x6@w+@v�L@v�@u��@u;@t��@tbN@t*�@s�V@r��@q��@q�-@p�|@oH�@o
=@n��@n�A@m�Z@m�=@mQ�@lѷ@l�4@l��@l�@l�D@le�@lFt@k��@kiD@kY@j�2@j��@j.�@i�@h~(@g�a@g�k@ge�@g1�@f҉@fxl@e��@ek�@e?}@d�O@c�A@cC@b�h@b\�@bp;@b�X@b�@b�B@b��@b��@bn�@b
�@a�-@a�#@b!�@b:*@b�@a��@a�@a��@au�@ahs@aN<@a+�@a&�@a#�@`��@`�9@`�D@_�&@_dZ@^�@^z@]�@]�~@]j@]Dg@]�@\Ĝ@\`�@[�@[{J@Z�@Z��@Zv�@Zl�@ZR�@Z5?@Y�o@Y�t@YIR@Y	l@X�e@XtT@XZ@X�@W�6@W�@WiD@W"�@Vȴ@V��@V�F@VH�@V�@U��@U@U��@U[W@T�K@T��@T��@Th�@T,=@S�
@R��@R�@R��@R��@R�@Qc�@P��@Pr�@Pu�@Pz�@Pu�@Pc�@Pc�@PPH@P�@O��@Os@N�@M�d@Ms�@M+�@M�@L�@L�@L��@L��@K��@K�@J\�@J�@I�j@I�N@IS&@H�P@H�p@H�@Hg8@H�@G�*@G�	@Ge�@G�@F�,@F_�@F8�@F+k@E�Z@E��@E%@D�	@D�z@D�o@C�@C��@CP�@B��@A�o@A�~@Aj@AO�@A/@@�`@@�O@@��@@S�@@ �@@G@?ݘ@?��@?O@?S@>��@>{@=��@=��@=c@=p�@<�?@<��@<z�@;�q@;8@;+@:�@:� @:�@9��@9�@9��@9�@9�S@9u�@9p�@9e,@9q@8�P@8�e@8tT@8Q�@8�@7� @7�$@7g�@7@6ߤ@6�b@6�A@6�A@6ff@6B[@5�@5j@5@@4�U@4?�@3�@3��@3�@@3�$@3~�@3>�@2�"@2s�@1�3@1�@0�u@/��@/|�@.�8@.n�@-��@-��@-��@-x�@-^�@-Q�@-IR@--w@-�@,�	@,֡@,�?@,�j@,�e@,��@,w�@,l"@,Z@,Xy@,PH@,D�@,4n@,�@+˒@+��@+�0@+�q@+�@+H�@+>�@+�@*�@*�@*�@)�@)��@)a�@)IR@)&�@(��@(�j@(��@(b@'��@'�:@'g�@'Y@'�@&��@&�B@&�B@&�B@&�@&�<@&�h@&v�@&R�@&8�@%��@%\�@%S&@%J�@%+�@$��@$�)@$��@$�@$�@$l"@$c�@$N�@$'R@#�&@#qv@#S�@#.I@#�@"��@"�,@"�'@"�!@"u%@!ϫ@!�"@!A @!�@ �E@ �O@ g8@ "h@ @�]@��@��@�g@�a@��@s@8@�@�@��@d�@��@�@p�@^�@F@�	@��@u�@h�@bN@]d@Q�@�@E9@��@3�@�-@A @!�@@�@�@@@	l@�P@��@�p@Ĝ@��@e�@�@�q@H�@ߤ@�'@��@�1@��@��@�A@z@n�@^5@�j@Y�@��@�j@��@�I@�@U2@9X@$@:�@!@�$@K�@�@xl@^5@!�@�@��@@@�/@�e@��@�u@�.@~(@[�@:�@�@x@�@s@��@��@��@kQ@L0@B[@8�@�@@�>@��@�H@��@x�@x�@rG@�E@�I@bN@Ft@,=@"h@"h@"h@�@�@G@�@�m@�}@��@iD@A�@�@
�@
�L@
q�@
e@	��@	��@	�X@	�@	Dg@�@��@<�@�@�]@�W@�
@�a@�k@Mj@'�@"�@�@@�y@҉@�}@�@Ta@?@($@�@�N@��@��@Y�@IR@G�@8�@@@��@��@�z@�@q@Ft@<�@1'@�@�@��@�6@�[@�q@��@�*@��@�$@�4@j�@]�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�.IA�3�A�4�A�-A�(�A�'�A�+6A�3�A�7LA�6�A�6FA�6FA�4A���A��-A�v�A��bA���A�<�A���A��A�x8A��nA���A�_A���A��A��A��JA���A�T�A���A�>BA��GA���A��A�F�A�`vA���A��`A�O�A��A��
A��:A��A���A��TA��PA�EA�x�A�I�A��FA��rA���A�QNA�$�A���A���A�IRA�(�A�SA��A��A��A�h>A���A�+�A��0A�($A�JXA��\A��uA���A�A�ǮA�cTA��7A��zA�  A�l�A���A�C�A��rA���A��A��`A���A���A�d�A�[#A�1�A�� A��RA��A�@A�z�A� A���A���A���A�YA�A}�A|��Az֡Aw%FAt��Ar� Ap�=An�Ak��Ai��Ah�Ag7�Ad�NAbbAaOvAaL�Aa;dA`�HA_�DA]��A[خA[�AY�AY~AXѷAX�=AX�AW�AT�mAS�zAS�AR�XARzxAQ��APخAPJ#AP33AP-wAP!-AP;AO��AO@�AN�AN�AN�AL]�AJ)�AH��AGc AE^5AE�AD��AD'RAB�AA��A@��A@oA?�`A?��A>L0A=MA=�A<�A<��A<6A;��A:��A8�`A8;dA7ĜA6��A5��A4�A3ɆA2�aA1�A1=A0�A0�$A0�4A0��A0� A0/�A/��A.�9A.\�A.A,�pA+�A+&A*�A)�A)�A)(�A'��A&�+A%�NA$��A$B[A#�'A#�A!FtA m�A�A��A.�A�UA!ATaA��AFtA�A��A��A�A>�A�UAe�A�A�A�>A;A�A�eA?A�2A;�AE�A�A�A2�A��A&�ArGA(AAW?Ay>A
�!A	�A	JAZ�A��A!�A��AsAVmA#�A��A�4AC�A��A�|A��A.IA��A 8@��,@��v@��@�U�@��T@��4@�j@�$@�0@�@�i�@��@�q@�k�@���@�+@�?�@�@栐@䟾@�@�҉@ߖS@�x@��,@��m@�!@պ^@ԗ�@� \@���@��@�=�@��@��.@�n/@ʬ@�<�@�ԕ@�q@���@Ƙ_@�xl@�*0@�M@�{�@�.I@���@�l"@�@�kQ@��8@��@�bN@�R�@�	@�u@��	@���@�@�@��@��~@��8@�l"@� �@�X@��d@��5@�L�@���@�J#@���@��@��A@��@�˒@��$@�4@��q@��o@��3@�Q�@�͟@��o@�?�@��D@�v`@�6z@���@�G@�S�@��$@�@��+@��o@�خ@��	@�V@�ԕ@�x@�4�@�S@���@��=@��L@�c @�N�@��@�2a@���@���@��-@���@�z@�8�@���@���@�[W@�/@��.@���@���@���@�4@�@@��E@��E@��@�?@�U�@�� @�}V@�,=@���@���@��S@�g�@�O�@��5@��B@�bN@�e@��@��9@��n@��'@�(@��\@�x@�ԕ@�c@�
=@��6@�bN@��>@�=@��P@��@���@�p;@��@��r@��.@�h
@���@�e,@�a@�zx@�;d@��L@�C-@�B[@�9X@�J@��@���@��j@���@�}�@�(�@�ی@���@���@���@���@��4@���@�� @���@�u%@�O@�S�@��]@���@�tT@�>B@�M�@�4n@�b@v`@�@~�6@~�A@~h
@~O@~�@}�.@}��@}[W@|�@{&@{1�@{Y@z�X@z�@zv�@y��@yS&@y!�@x�@x6@w+@v�L@v�@u��@u;@t��@tbN@t*�@s�V@r��@q��@q�-@p�|@oH�@o
=@n��@n�A@m�Z@m�=@mQ�@lѷ@l�4@l��@l�@l�D@le�@lFt@k��@kiD@kY@j�2@j��@j.�@i�@h~(@g�a@g�k@ge�@g1�@f҉@fxl@e��@ek�@e?}@d�O@c�A@cC@b�h@b\�@bp;@b�X@b�@b�B@b��@b��@bn�@b
�@a�-@a�#@b!�@b:*@b�@a��@a�@a��@au�@ahs@aN<@a+�@a&�@a#�@`��@`�9@`�D@_�&@_dZ@^�@^z@]�@]�~@]j@]Dg@]�@\Ĝ@\`�@[�@[{J@Z�@Z��@Zv�@Zl�@ZR�@Z5?@Y�o@Y�t@YIR@Y	l@X�e@XtT@XZ@X�@W�6@W�@WiD@W"�@Vȴ@V��@V�F@VH�@V�@U��@U@U��@U[W@T�K@T��@T��@Th�@T,=@S�
@R��@R�@R��@R��@R�@Qc�@P��@Pr�@Pu�@Pz�@Pu�@Pc�@Pc�@PPH@P�@O��@Os@N�@M�d@Ms�@M+�@M�@L�@L�@L��@L��@K��@K�@J\�@J�@I�j@I�N@IS&@H�P@H�p@H�@Hg8@H�@G�*@G�	@Ge�@G�@F�,@F_�@F8�@F+k@E�Z@E��@E%@D�	@D�z@D�o@C�@C��@CP�@B��@A�o@A�~@Aj@AO�@A/@@�`@@�O@@��@@S�@@ �@@G@?ݘ@?��@?O@?S@>��@>{@=��@=��@=c@=p�@<�?@<��@<z�@;�q@;8@;+@:�@:� @:�@9��@9�@9��@9�@9�S@9u�@9p�@9e,@9q@8�P@8�e@8tT@8Q�@8�@7� @7�$@7g�@7@6ߤ@6�b@6�A@6�A@6ff@6B[@5�@5j@5@@4�U@4?�@3�@3��@3�@@3�$@3~�@3>�@2�"@2s�@1�3@1�@0�u@/��@/|�@.�8@.n�@-��@-��@-��@-x�@-^�@-Q�@-IR@--w@-�@,�	@,֡@,�?@,�j@,�e@,��@,w�@,l"@,Z@,Xy@,PH@,D�@,4n@,�@+˒@+��@+�0@+�q@+�@+H�@+>�@+�@*�@*�@*�@)�@)��@)a�@)IR@)&�@(��@(�j@(��@(b@'��@'�:@'g�@'Y@'�@&��@&�B@&�B@&�B@&�@&�<@&�h@&v�@&R�@&8�@%��@%\�@%S&@%J�@%+�@$��@$�)@$��@$�@$�@$l"@$c�@$N�@$'R@#�&@#qv@#S�@#.I@#�@"��@"�,@"�'@"�!@"u%@!ϫ@!�"@!A @!�@ �E@ �O@ g8@ "h@ @�]@��@��@�g@�a@��@s@8@�@�@��@d�@��@�@p�@^�@F@�	@��@u�@h�@bN@]d@Q�@�@E9@��@3�@�-@A @!�@@�@�@@@	l@�P@��@�p@Ĝ@��@e�@�@�q@H�@ߤ@�'@��@�1@��@��@�A@z@n�@^5@�j@Y�@��@�j@��@�I@�@U2@9X@$@:�@!@�$@K�@�@xl@^5@!�@�@��@@@�/@�e@��@�u@�.@~(@[�@:�@�@x@�@s@��@��@��@kQ@L0@B[@8�@�@@�>@��@�H@��@x�@x�@rG@�E@�I@bN@Ft@,=@"h@"h@"h@�@�@G@�@�m@�}@��@iD@A�@�@
�@
�L@
q�@
e@	��@	��@	�X@	�@	Dg@�@��@<�@�@�]@�W@�
@�a@�k@Mj@'�@"�@�@@�y@҉@�}@�@Ta@?@($@�@�N@��@��@Y�@IR@G�@8�@@@��@��@�z@�@q@Ft@<�@1'@�@�@��@�6@�[@�q@��@�*@��@�$@�4@j�@]�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BxB	�BSB[B B �B 4B OB iB �B iB�B�8B�B�vBևB�BB��B̈́B�B�GB��B�DB�B��B�aB�B��B�KB�B��B�{B�By�Bt�Bm�B_BV�BM�BI�BF�BA�B;B4nB!�B�B�B�B�B�B�cB�:B��BרB՛B�TB�[BңB�NBϑB��B��B��B��B�=B��B�SB�BcBr�Bc�BQ�BG�BC{B<PB0oB \BmB BѝB�B�bB�"B��B�KB�oB��B�B�BB��B��Bz*Bl�Bc�BX�BT{BHKB5ZB+�B \BKB�B
=B  B
�B
�B
ɠB
�B
�sB
�kB
��B
��B
��B
wLB
i*B
bNB
a�B
a�B
`'B
[WB
P}B
F?B
@�B
:�B
5�B
3�B
2�B
0UB
,�B
&2B
!�B
�B
�B
	B
B
�B
B
�B
�B
�B
�B
�B
�B
 B
�B
�B
MB	��B	�XB	��B	�$B	��B	��B	�JB	�mB	��B	��B	�B	�PB	��B	�jB	�LB	��B	�ZB	��B	�B	�5B	��B	�sB	�nB	��B	��B	�KB	��B	��B	�pB	��B	�tB	�?B	�gB	��B	�aB	�-B	��B	~BB	{�B	w�B	u�B	raB	k�B	gB	b�B	_�B	^OB	\�B	V�B	R:B	NB	J�B	G+B	C�B	A�B	:�B	7�B	4nB	3B	0�B	/�B	,WB	)�B	&2B	#nB	!�B	�B	B	�B	�B	,B	TB	�B	)B	�B	9B	�B�cB�wB�6B�dB�2B��B�9B�vB�B�IB�B�6B��B�B�B�B�B�HB��B�B�WB��BخB��B�B�B�#B�xB�BۦB�qB�EB��B��B�TB��B�pB�6B��B�=BȴBǮB��B��B�-B��B��B��B�4B��B�.B�B��B��B��B�XB�B��B��B�nB�TB�3B��B��B�'B�B�oB�B��B�[B��B�UB��B��B��B��B�CB��B��B�B�vB��B�|B�|B��B�LB��B�B��B�2B��B�6B�(B��B��B�B�EBɺB�XB��B��B��B�MB�yB�7BݘB�BB�B��B�B�B�tB�QB�iB�B��B��B��B�hB�9B�B��B��B�B��B	;B	gB	�B	gB	�B	B	�B	B	VB	BB	.B	:B	�B	yB	�B	�B	�B	�B	B	!B	%,B	&2B	'8B	'�B	(�B	*B	*KB	*eB	-CB	/ B	/�B	2�B	3�B	5%B	6`B	6FB	6zB	88B	>B	G_B	HKB	K�B	L�B	M�B	MPB	NB	NpB	P�B	Q�B	U�B	X+B	X�B	Z7B	[�B	\]B	`�B	dtB	gB	hsB	k�B	o�B	q�B	q�B	s�B	x8B	z�B	|PB	�OB	��B	�lB	��B	��B	��B	�B	�{B	�mB	�xB	��B	�LB	�sB	�yB	�B	��B	�iB	�;B	��B	��B	�8B	��B	��B	�4B	��B	��B	�B	�[B	ªB	��B	�-B	ðB	��B	��B	�vB	ϑB	�TB	�,B	�SB	�?B	�EB	�#B	�IB	�;B	�'B	�B	�B	�B	�B	�B	��B	��B	�B	�iB	��B	�+B	�B	�rB	��B	��B
 �B
�B
MB
?B
YB
�B
�B

XB
�B
�B
\B
�B
�B
�B
�B
kB
)B
IB
�B
 �B
"�B
#�B
$�B
%�B
&fB
&�B
(�B
*�B
,�B
.B
0�B
1�B
2�B
3�B
4B
6+B
:*B
<B
=�B
>�B
?�B
@iB
AUB
B�B
E�B
F�B
G+B
HKB
J#B
K�B
L�B
NVB
O\B
Q�B
S�B
T�B
U�B
VB
U�B
V9B
W
B
X_B
[WB
^OB
_!B
_pB
`vB
a�B
bhB
b�B
d�B
ffB
g�B
i�B
k�B
lWB
l�B
o B
p�B
rGB
s�B
u�B
w2B
w�B
xB
y	B
y�B
{B
}�B
.B
�oB
�uB
��B
�B
�aB
��B
��B
�9B
��B
�+B
��B
�B
�RB
��B
�xB
��B
��B
�6B
�"B
�"B
�pB
��B
�vB
��B
�bB
��B
��B
�uB
��B
�B
�aB
��B
�gB
��B
�
B
�$B
�$B
�EB
�KB
�7B
�=B
��B
��B
��B
�dB
�B
��B
��B
��B
��B
�HB
��B
�B
�ZB
��B
��B
�,B
�`B
��B
�DB
�B
��B
�wB
��B
�B
�OB
�B
�B
�GB
��B
�nB
�ZB
��B
��B
�`B
��B
��B
�2B
��B
��B
�$B
�xB
��B
��B
�B
��B
��B
�B
��B
��B
��B
�B
�aB
ðB
�MB
��B
�9B
ŢB
��B
��B
�?B
ƎB
��B
�+B
�fB
�RB
�	B
�=B
�XB
�XB
ˬB
�B
��B
��B
�VB
�pB
��B
��B
�}B
бB
бB
бB
��B
�NB
�hB
уB
ѝB
� B
�:B
�B
�[B
өB
�FB
ԯB
�B
ՁB
��B
�B
֡B
ּB
��B
��B
�$B
�+B
��B
ٚB
�B
�	B
یB
��B
��B
��B
�)B
ܒB
��B
�B
�;B
�B
�-B
�NB
�B
�B
�tB
�FB
��B
��B
��B
�2B
�2B
�LB
�fB
�B
�B
�B
�B
�B
�RB
�mB
�B
�B
��B
��B
��B
��B
�$B
�sB
��B
��B
��B
�B
�*B
��B
�B
��B
�B
�B
�kB
�B
�WB
�B
��B
��B
�]B
�CB
�B
�B
�cB
�B
��B
�iB
�iB
�B
�B
�B
�!B
�;B
�;B
�UB
�B
��B
��B
��B
��B
�B
��B
�3B
�B
��B
�B
�nB
��B
��B
��B
��B
��B
�?B
��B
��B
��B
�+B
�zB
�zB
��B
��B
��B
�B
�B
��B
�>B
��B
��B
�xB
��B
�B
�0B
�0B
�0B
�dB
�dB
��B
��B
�B
�PB
��B
��B
�"B
��B
�(B
�B
�(B
�(B
��B
�}B
��B
��B
��B
��B
��B 4B�BoB'B�BaB�B�B�B�BB3BgB�B�BmBmB�B%B�B�B_B�BBKB�B�BfB�B�BfBKB�B	�B
#B
#B
#B
#B
=B
#B
#B
#B
rB)B�B�BdB�B�BPBPB�BVBpB�B�B�B�B�B(B\B�BvB�BHBNBhB�B�BB B:BoBoB�B�B�B&B[B@B&B{B�B�B2BMBgBgBgB�BgB�B�BBSB�B?B?BYB�B�B+B�B1BB�B1B�B�B�B�B�B�B�BBBxB�B/B/BB/B�B�BB5B�B�B�B�B;B�B�B B B B B \B �B �B �B!B!HB!|B!|B!�B!�B!�B!�B!�B"4B"B"4B"B"B"4B"4B"NB"444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�BxB	�BSB[B B �B 4B OB iB �B iB�B�8B�B�vBևB�BB��B̈́B�B�GB��B�DB�B��B�aB�B��B�KB�B��B�{B�By�Bt�Bm�B_BV�BM�BI�BF�BA�B;B4nB!�B�B�B�B�B�B�cB�:B��BרB՛B�TB�[BңB�NBϑB��B��B��B��B�=B��B�SB�BcBr�Bc�BQ�BG�BC{B<PB0oB \BmB BѝB�B�bB�"B��B�KB�oB��B�B�BB��B��Bz*Bl�Bc�BX�BT{BHKB5ZB+�B \BKB�B
=B  B
�B
�B
ɠB
�B
�sB
�kB
��B
��B
��B
wLB
i*B
bNB
a�B
a�B
`'B
[WB
P}B
F?B
@�B
:�B
5�B
3�B
2�B
0UB
,�B
&2B
!�B
�B
�B
	B
B
�B
B
�B
�B
�B
�B
�B
�B
 B
�B
�B
MB	��B	�XB	��B	�$B	��B	��B	�JB	�mB	��B	��B	�B	�PB	��B	�jB	�LB	��B	�ZB	��B	�B	�5B	��B	�sB	�nB	��B	��B	�KB	��B	��B	�pB	��B	�tB	�?B	�gB	��B	�aB	�-B	��B	~BB	{�B	w�B	u�B	raB	k�B	gB	b�B	_�B	^OB	\�B	V�B	R:B	NB	J�B	G+B	C�B	A�B	:�B	7�B	4nB	3B	0�B	/�B	,WB	)�B	&2B	#nB	!�B	�B	B	�B	�B	,B	TB	�B	)B	�B	9B	�B�cB�wB�6B�dB�2B��B�9B�vB�B�IB�B�6B��B�B�B�B�B�HB��B�B�WB��BخB��B�B�B�#B�xB�BۦB�qB�EB��B��B�TB��B�pB�6B��B�=BȴBǮB��B��B�-B��B��B��B�4B��B�.B�B��B��B��B�XB�B��B��B�nB�TB�3B��B��B�'B�B�oB�B��B�[B��B�UB��B��B��B��B�CB��B��B�B�vB��B�|B�|B��B�LB��B�B��B�2B��B�6B�(B��B��B�B�EBɺB�XB��B��B��B�MB�yB�7BݘB�BB�B��B�B�B�tB�QB�iB�B��B��B��B�hB�9B�B��B��B�B��B	;B	gB	�B	gB	�B	B	�B	B	VB	BB	.B	:B	�B	yB	�B	�B	�B	�B	B	!B	%,B	&2B	'8B	'�B	(�B	*B	*KB	*eB	-CB	/ B	/�B	2�B	3�B	5%B	6`B	6FB	6zB	88B	>B	G_B	HKB	K�B	L�B	M�B	MPB	NB	NpB	P�B	Q�B	U�B	X+B	X�B	Z7B	[�B	\]B	`�B	dtB	gB	hsB	k�B	o�B	q�B	q�B	s�B	x8B	z�B	|PB	�OB	��B	�lB	��B	��B	��B	�B	�{B	�mB	�xB	��B	�LB	�sB	�yB	�B	��B	�iB	�;B	��B	��B	�8B	��B	��B	�4B	��B	��B	�B	�[B	ªB	��B	�-B	ðB	��B	��B	�vB	ϑB	�TB	�,B	�SB	�?B	�EB	�#B	�IB	�;B	�'B	�B	�B	�B	�B	�B	��B	��B	�B	�iB	��B	�+B	�B	�rB	��B	��B
 �B
�B
MB
?B
YB
�B
�B

XB
�B
�B
\B
�B
�B
�B
�B
kB
)B
IB
�B
 �B
"�B
#�B
$�B
%�B
&fB
&�B
(�B
*�B
,�B
.B
0�B
1�B
2�B
3�B
4B
6+B
:*B
<B
=�B
>�B
?�B
@iB
AUB
B�B
E�B
F�B
G+B
HKB
J#B
K�B
L�B
NVB
O\B
Q�B
S�B
T�B
U�B
VB
U�B
V9B
W
B
X_B
[WB
^OB
_!B
_pB
`vB
a�B
bhB
b�B
d�B
ffB
g�B
i�B
k�B
lWB
l�B
o B
p�B
rGB
s�B
u�B
w2B
w�B
xB
y	B
y�B
{B
}�B
.B
�oB
�uB
��B
�B
�aB
��B
��B
�9B
��B
�+B
��B
�B
�RB
��B
�xB
��B
��B
�6B
�"B
�"B
�pB
��B
�vB
��B
�bB
��B
��B
�uB
��B
�B
�aB
��B
�gB
��B
�
B
�$B
�$B
�EB
�KB
�7B
�=B
��B
��B
��B
�dB
�B
��B
��B
��B
��B
�HB
��B
�B
�ZB
��B
��B
�,B
�`B
��B
�DB
�B
��B
�wB
��B
�B
�OB
�B
�B
�GB
��B
�nB
�ZB
��B
��B
�`B
��B
��B
�2B
��B
��B
�$B
�xB
��B
��B
�B
��B
��B
�B
��B
��B
��B
�B
�aB
ðB
�MB
��B
�9B
ŢB
��B
��B
�?B
ƎB
��B
�+B
�fB
�RB
�	B
�=B
�XB
�XB
ˬB
�B
��B
��B
�VB
�pB
��B
��B
�}B
бB
бB
бB
��B
�NB
�hB
уB
ѝB
� B
�:B
�B
�[B
өB
�FB
ԯB
�B
ՁB
��B
�B
֡B
ּB
��B
��B
�$B
�+B
��B
ٚB
�B
�	B
یB
��B
��B
��B
�)B
ܒB
��B
�B
�;B
�B
�-B
�NB
�B
�B
�tB
�FB
��B
��B
��B
�2B
�2B
�LB
�fB
�B
�B
�B
�B
�B
�RB
�mB
�B
�B
��B
��B
��B
��B
�$B
�sB
��B
��B
��B
�B
�*B
��B
�B
��B
�B
�B
�kB
�B
�WB
�B
��B
��B
�]B
�CB
�B
�B
�cB
�B
��B
�iB
�iB
�B
�B
�B
�!B
�;B
�;B
�UB
�B
��B
��B
��B
��B
�B
��B
�3B
�B
��B
�B
�nB
��B
��B
��B
��B
��B
�?B
��B
��B
��B
�+B
�zB
�zB
��B
��B
��B
�B
�B
��B
�>B
��B
��B
�xB
��B
�B
�0B
�0B
�0B
�dB
�dB
��B
��B
�B
�PB
��B
��B
�"B
��B
�(B
�B
�(B
�(B
��B
�}B
��B
��B
��B
��B
��B 4B�BoB'B�BaB�B�B�B�BB3BgB�B�BmBmB�B%B�B�B_B�BBKB�B�BfB�B�BfBKB�B	�B
#B
#B
#B
#B
=B
#B
#B
#B
rB)B�B�BdB�B�BPBPB�BVBpB�B�B�B�B�B(B\B�BvB�BHBNBhB�B�BB B:BoBoB�B�B�B&B[B@B&B{B�B�B2BMBgBgBgB�BgB�B�BBSB�B?B?BYB�B�B+B�B1BB�B1B�B�B�B�B�B�B�BBBxB�B/B/BB/B�B�BB5B�B�B�B�B;B�B�B B B B B \B �B �B �B!B!HB!|B!|B!�B!�B!�B!�B!�B"4B"B"4B"B"B"4B"4B"NB"444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604213054  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604225559  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604225600  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604225600                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605075613  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605075613  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220609221504                      G�O�G�O�G�O�                