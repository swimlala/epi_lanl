CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-18T06:35:33Z creation;2018-05-18T06:35:36Z conversion to V3.1;2019-12-23T06:21:47Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180518063533  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA  I2_0675_061                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�c����1   @�c�����@7��PH�b�vȴ9X1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ D�|�D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@XQ�@�\)@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���C �{Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J=C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-%D-�D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL%DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv�RDw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D��D�O\D��\D��\D�\D�O\D��)D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��)D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D\D��\D�\D�O\DÏ\D��\D�\D�O\Dď\D��\D�\D�O\Dŏ\D��\D�\D�O\Dƌ)D��\D�\D�O\DǏ\D��\D�\D�O\Dȏ\D��\D�\D�O\Dɏ\D��\D�\D�O\Dʏ\D��\D�\D�O\Dˏ\D��\D�\D�O\D̏\D��\D�\D�O\D͏\D��\D�\D�O\DΏ\D��\D�\D�O\DϏ\D��\D�\D�O\DЏ\D��\D�\D�O\Dя\D��\D�\D�O\Dҏ\D��\D�\D�O\Dӏ\D��\D�\D�O\Dԏ\D��\D�\D�O\DՏ\D��\D�\D�O\D֏\D��\D�\D�O\D׏\D��\D�\D�O\D؏\D��\D�\D�O\Dُ\D��\D�\D�O\Dڏ\D��\D�\D�O\Dۏ\D��\D�\D�O\D܏\D��\D�\D�O\Dݏ\D��\D�\D�O\Dޏ\D��\D�\D�O\Dߏ\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�)D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A�O�A��yA�r�A�-A���A��wA��A�VA�hsA��A���A�|�A�C�A�  A��wA��9A���A�v�A�C�A��A�%A�x�A��yA�v�A��yA�\)A���A�jA��`A�5?A�A�A�ZA��#A�\)A���A�Q�A���A�5?A��`A�jA��RA��A�M�A�;dA�{A���A���A��uA�^5A�5?A��A��A�JA��`A��FA���A��7A��A�\)A�C�A�"�A�JA���A��A��A���A�ƨA��wA��9A���A��+A�C�A��A�33A��A��A�{A�I�A�/A�E�A�1A��uA�$�A�5?A�\)A���A���A��
A���A���A��/A��A��A���A�Q�A���A���A��A�=qA�S�A�ĜA�=qA���A?}A}G�A|�RA|E�A{O�Ay�wAv�yAu�At��At=qAs��Aq�mAo%AmAkl�Ai|�Ah�AhjAgAdQ�Aa��AaVA_��A]?}A[+AZ�DAYS�AV�AUƨAS�AQ��AP�AO�7AM�mAL��ALE�AK��AJjAIl�AHbAE|�AD��AC�
ACS�ABffAA7LA>�HA<�HA<��A:��A9�^A8r�A7|�A733A5�TA4��A4bNA3��A3��A3/A2~�A0�/A/l�A-x�A,ȴA,A+�A+�A)��A(�`A'A&�/A%�#A$I�A"�jA!�mA z�AoAn�A �AA��A"�AI�A�AĜA�A�-A��A��AVA�+A(�A��A^5A�A�uA-A��AVA�+AXA
�A
M�A	��A	�hA�AAt�A��At�A�yA�A�DA|�A �/A I�@��P@�$�@�A�@�@���@��u@��m@�v�@��7@��/@�@�-@���@�|�@��@��T@��`@��@�!@�1'@�
=@�5?@�@�1'@��@�C�@�ff@�I�@�5?@�z�@��@ە�@�S�@�$�@ّh@���@�S�@և+@���@�?}@ԋD@�ƨ@ҏ\@�p�@мj@Ͼw@���@�bN@���@�+@�~�@�/@�A�@��;@���@ź^@ě�@�9X@�ƨ@��y@§�@�J@���@�1'@��R@��T@�p�@��@�|�@�
=@��7@���@���@�Z@��m@�\)@��H@�E�@���@�j@�1@�o@�hs@���@��@�I�@���@���@��@�p�@�bN@��@���@�dZ@��\@���@��@��u@���@���@��P@�t�@��y@�{@�hs@���@�A�@��m@�ff@�@�`B@���@�dZ@��@�@�E�@���@�7L@��@�1'@�r�@��u@��u@��@��D@�z�@�1'@�A�@�r�@��/@��@�@�ȴ@�E�@���@��!@�J@���@��h@�hs@�O�@�&�@��@� �@�b@��;@���@��@�K�@�;d@�l�@�+@�o@��y@��!@��\@��@��^@���@��@�V@�%@�%@��u@��@�hs@�`B@�X@���@�j@�  @���@�;d@�"�@���@���@��+@�-@��#@���@�X@��@��j@���@���@�A�@�b@��F@�\)@��@��R@��\@�^5@�=q@��@��-@��h@��@�hs@��@�Ĝ@��u@�Z@� �@�1@��@��w@��P@�C�@��@���@�~�@�n�@�{@��@���@�@���@��@�p�@�`B@�G�@�V@���@���@�j@�Z@�(�@���@�dZ@�;d@�33@�+@��@���@��@���@�bN@�Q�@�A�@� �@�1@��;@��@��@�S�@�+@��@�@���@��@��y@��H@��@���@�$�@��T@���@��-@���@��h@�X@�7L@�V@��`@��j@��@���@��@�t�@�dZ@�S�@�+@���@��@��y@��@���@��@��#@���@�p�@�hs@�7L@��9@���@��u@�1'@��@~�@~v�@~5?@}��@}��@}p�@}`B@}?}@}?}@}�@|��@|Z@|I�@|�@{ƨ@{��@{��@{t�@z�@z��@z��@z��@z�\@z^5@zJ@yx�@y�@xĜ@xbN@xb@w�w@w\)@vv�@v@u�-@up�@t��@t�@s��@s�@sS�@r�H@r�!@q�#@q%@p��@p��@p�u@p1'@o��@o��@oK�@n�y@nȴ@n��@nff@n$�@n@m��@mO�@m�@l�D@l9X@kt�@j�H@j��@j=q@i��@i�@i�^@i�7@i7L@h��@h�9@h�@h �@g�;@g�P@g;d@fȴ@f�+@f{@e�T@e��@e`B@e/@d��@d�@d�@d�j@dZ@dI�@d1@c��@c"�@b�!@b~�@bJ@a�@a�#@a��@ax�@aX@`��@`A�@`1'@_��@]��@\�/@\(�@[�m@[dZ@Z�\@Y��@Y�7@Yhs@YG�@Y�@X�u@X�@Xr�@XQ�@X �@Xb@X  @WK�@V��@VV@U�T@U`B@T��@T�/@T��@T9X@S��@S�F@St�@SdZ@SdZ@S@R�H@R��@R^5@R=q@R�@Q�@Qhs@P�u@P  @O�P@O\)@Ol�@OK�@N�R@NV@M��@M?}@L�@L�/@L�j@L�/@M�@Mp�@M�@L�D@K�
@Kt�@KdZ@KS�@KC�@Ko@J�H@J^5@I��@IX@I�@H�9@H�@H1'@G�;@G�;@G�w@G��@Gl�@G;d@G
=@F�R@FE�@E�-@E`B@EO�@E?}@E?}@Ep�@EO�@EO�@E/@E/@E�@D��@Dz�@Cƨ@C�@CdZ@CC�@C"�@C@B�@B��@B�\@B=q@A��@A��@A�7@@��@@r�@@ �@?�@?��@?|�@?K�@?;d@?;d@>��@>$�@=�T@=�-@=��@=p�@=O�@=V@<�j@<z�@<9X@<(�@;�m@;�F@;dZ@;S�@;33@;o@;@:��@:M�@:J@9�@9�7@9G�@9%@8�@81'@8 �@7�@7��@7l�@7+@6��@6ff@5�@5�-@5p�@5?}@5V@5V@4��@4�/@4�/@4��@4z�@41@3dZ@3o@2�H@2�H@2��@2~�@2-@2J@1�#@1hs@17L@1%@0�`@0��@0��@0r�@0A�@0 �@/�;@/|�@/;d@/
=@.�R@.ff@.E�@.5?@-@-`B@,�j@,Z@,9X@,(�@,1@+�m@+��@+"�@*�H@*M�@)�@)�7@)G�@(�`@(��@( �@(b@'�@'��@'|�@'+@&��@&�@&��@&v�@&5?@&@%�-@%p�@%/@$�j@$z�@$(�@#ƨ@#��@#��@#��@#S�@"�@"�!@"n�@"-@"J@!�#@!�^@!hs@!7L@!%@ ��@ �9@ �@ bN@ A�@ b@�;@�@\)@;d@;d@+@�@�@�@�@�+@ff@5?@{@@�T@�-@�h@O�@�@�j@�@��@j@��@�m@�F@t�@C�@"�@��@�!@�\@^5@-@-@�@�@��@�7@G�@�@�`@�9@��@�u@�@ �@�w@|�@l�@K�@�@��@�@�R@��@ff@@��@��@�h@�@`B@O�@��@��@Z@��@�F@S�@"�@�@��@��@n�@�@��@�^@hs@�`@��@bN@A�@ �@b@  @��@�@;d@�@��@@��@�@`B@/@V@��@�/@��@�@��@z�@Z@9X@�m@ƨ@�
@ƨ@��@�@t�@33@o@
�@
��@
�!@
�!@
�\@
^5@
�@
�@	��@	�@	��@	��@	x�@	G�@	&�@��@Ĝ@�@Q�@Q�@A�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A�O�A��yA�r�A�-A���A��wA��A�VA�hsA��A���A�|�A�C�A�  A��wA��9A���A�v�A�C�A��A�%A�x�A��yA�v�A��yA�\)A���A�jA��`A�5?A�A�A�ZA��#A�\)A���A�Q�A���A�5?A��`A�jA��RA��A�M�A�;dA�{A���A���A��uA�^5A�5?A��A��A�JA��`A��FA���A��7A��A�\)A�C�A�"�A�JA���A��A��A���A�ƨA��wA��9A���A��+A�C�A��A�33A��A��A�{A�I�A�/A�E�A�1A��uA�$�A�5?A�\)A���A���A��
A���A���A��/A��A��A���A�Q�A���A���A��A�=qA�S�A�ĜA�=qA���A?}A}G�A|�RA|E�A{O�Ay�wAv�yAu�At��At=qAs��Aq�mAo%AmAkl�Ai|�Ah�AhjAgAdQ�Aa��AaVA_��A]?}A[+AZ�DAYS�AV�AUƨAS�AQ��AP�AO�7AM�mAL��ALE�AK��AJjAIl�AHbAE|�AD��AC�
ACS�ABffAA7LA>�HA<�HA<��A:��A9�^A8r�A7|�A733A5�TA4��A4bNA3��A3��A3/A2~�A0�/A/l�A-x�A,ȴA,A+�A+�A)��A(�`A'A&�/A%�#A$I�A"�jA!�mA z�AoAn�A �AA��A"�AI�A�AĜA�A�-A��A��AVA�+A(�A��A^5A�A�uA-A��AVA�+AXA
�A
M�A	��A	�hA�AAt�A��At�A�yA�A�DA|�A �/A I�@��P@�$�@�A�@�@���@��u@��m@�v�@��7@��/@�@�-@���@�|�@��@��T@��`@��@�!@�1'@�
=@�5?@�@�1'@��@�C�@�ff@�I�@�5?@�z�@��@ە�@�S�@�$�@ّh@���@�S�@և+@���@�?}@ԋD@�ƨ@ҏ\@�p�@мj@Ͼw@���@�bN@���@�+@�~�@�/@�A�@��;@���@ź^@ě�@�9X@�ƨ@��y@§�@�J@���@�1'@��R@��T@�p�@��@�|�@�
=@��7@���@���@�Z@��m@�\)@��H@�E�@���@�j@�1@�o@�hs@���@��@�I�@���@���@��@�p�@�bN@��@���@�dZ@��\@���@��@��u@���@���@��P@�t�@��y@�{@�hs@���@�A�@��m@�ff@�@�`B@���@�dZ@��@�@�E�@���@�7L@��@�1'@�r�@��u@��u@��@��D@�z�@�1'@�A�@�r�@��/@��@�@�ȴ@�E�@���@��!@�J@���@��h@�hs@�O�@�&�@��@� �@�b@��;@���@��@�K�@�;d@�l�@�+@�o@��y@��!@��\@��@��^@���@��@�V@�%@�%@��u@��@�hs@�`B@�X@���@�j@�  @���@�;d@�"�@���@���@��+@�-@��#@���@�X@��@��j@���@���@�A�@�b@��F@�\)@��@��R@��\@�^5@�=q@��@��-@��h@��@�hs@��@�Ĝ@��u@�Z@� �@�1@��@��w@��P@�C�@��@���@�~�@�n�@�{@��@���@�@���@��@�p�@�`B@�G�@�V@���@���@�j@�Z@�(�@���@�dZ@�;d@�33@�+@��@���@��@���@�bN@�Q�@�A�@� �@�1@��;@��@��@�S�@�+@��@�@���@��@��y@��H@��@���@�$�@��T@���@��-@���@��h@�X@�7L@�V@��`@��j@��@���@��@�t�@�dZ@�S�@�+@���@��@��y@��@���@��@��#@���@�p�@�hs@�7L@��9@���@��u@�1'@��@~�@~v�@~5?@}��@}��@}p�@}`B@}?}@}?}@}�@|��@|Z@|I�@|�@{ƨ@{��@{��@{t�@z�@z��@z��@z��@z�\@z^5@zJ@yx�@y�@xĜ@xbN@xb@w�w@w\)@vv�@v@u�-@up�@t��@t�@s��@s�@sS�@r�H@r�!@q�#@q%@p��@p��@p�u@p1'@o��@o��@oK�@n�y@nȴ@n��@nff@n$�@n@m��@mO�@m�@l�D@l9X@kt�@j�H@j��@j=q@i��@i�@i�^@i�7@i7L@h��@h�9@h�@h �@g�;@g�P@g;d@fȴ@f�+@f{@e�T@e��@e`B@e/@d��@d�@d�@d�j@dZ@dI�@d1@c��@c"�@b�!@b~�@bJ@a�@a�#@a��@ax�@aX@`��@`A�@`1'@_��@]��@\�/@\(�@[�m@[dZ@Z�\@Y��@Y�7@Yhs@YG�@Y�@X�u@X�@Xr�@XQ�@X �@Xb@X  @WK�@V��@VV@U�T@U`B@T��@T�/@T��@T9X@S��@S�F@St�@SdZ@SdZ@S@R�H@R��@R^5@R=q@R�@Q�@Qhs@P�u@P  @O�P@O\)@Ol�@OK�@N�R@NV@M��@M?}@L�@L�/@L�j@L�/@M�@Mp�@M�@L�D@K�
@Kt�@KdZ@KS�@KC�@Ko@J�H@J^5@I��@IX@I�@H�9@H�@H1'@G�;@G�;@G�w@G��@Gl�@G;d@G
=@F�R@FE�@E�-@E`B@EO�@E?}@E?}@Ep�@EO�@EO�@E/@E/@E�@D��@Dz�@Cƨ@C�@CdZ@CC�@C"�@C@B�@B��@B�\@B=q@A��@A��@A�7@@��@@r�@@ �@?�@?��@?|�@?K�@?;d@?;d@>��@>$�@=�T@=�-@=��@=p�@=O�@=V@<�j@<z�@<9X@<(�@;�m@;�F@;dZ@;S�@;33@;o@;@:��@:M�@:J@9�@9�7@9G�@9%@8�@81'@8 �@7�@7��@7l�@7+@6��@6ff@5�@5�-@5p�@5?}@5V@5V@4��@4�/@4�/@4��@4z�@41@3dZ@3o@2�H@2�H@2��@2~�@2-@2J@1�#@1hs@17L@1%@0�`@0��@0��@0r�@0A�@0 �@/�;@/|�@/;d@/
=@.�R@.ff@.E�@.5?@-@-`B@,�j@,Z@,9X@,(�@,1@+�m@+��@+"�@*�H@*M�@)�@)�7@)G�@(�`@(��@( �@(b@'�@'��@'|�@'+@&��@&�@&��@&v�@&5?@&@%�-@%p�@%/@$�j@$z�@$(�@#ƨ@#��@#��@#��@#S�@"�@"�!@"n�@"-@"J@!�#@!�^@!hs@!7L@!%@ ��@ �9@ �@ bN@ A�@ b@�;@�@\)@;d@;d@+@�@�@�@�@�+@ff@5?@{@@�T@�-@�h@O�@�@�j@�@��@j@��@�m@�F@t�@C�@"�@��@�!@�\@^5@-@-@�@�@��@�7@G�@�@�`@�9@��@�u@�@ �@�w@|�@l�@K�@�@��@�@�R@��@ff@@��@��@�h@�@`B@O�@��@��@Z@��@�F@S�@"�@�@��@��@n�@�@��@�^@hs@�`@��@bN@A�@ �@b@  @��@�@;d@�@��@@��@�@`B@/@V@��@�/@��@�@��@z�@Z@9X@�m@ƨ@�
@ƨ@��@�@t�@33@o@
�@
��@
�!@
�!@
�\@
^5@
�@
�@	��@	�@	��@	��@	x�@	G�@	&�@��@Ĝ@�@Q�@Q�@A�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B'�B&�B%�B&�B �B�B�B#�B5?B9XB=qBA�BA�BB�BG�BJ�BL�BO�BP�BW
B^5BbNBdZBp�Bz�B~�B�B�DB��B��B��B��B�B��B��B��B��B��BĜB��B��B�B�#B�/B�)B�/B�BB�BB�;B�BB�NB�fB�fB�mB�mB�fB�fB�fB�sB�yB�sB�mB�fB�`B�`B�fB�mB�fB�mB�sB�yB�sB�fB�BB�B��B�-B�bB|�BaHB@�B�B�BbB��B�B�BȴB�?B��B�bB�Bq�Be`BT�BO�B=qB-B �BbBB
�sB
�^B
�B
��B
ffB
M�B
I�B
D�B
>wB
49B
"�B
�B
hB
PB
1B	��B	�5B	��B	��B	�B	�B	��B	��B	�oB	�B	w�B	p�B	gmB	YB	T�B	S�B	D�B	<jB	-B	 �B	�B	hB	uB	bB	PB	
=B	B��B�B�fB�;B�#B�B��B��BȴB�qB�^B�FB�!B�B��B��B��B�hB�VB�JB�7B�+B�%B�B~�By�Bu�Bs�Br�Bq�Bm�BiyBffBdZB`BB]/BZBW
BVBQ�BN�BM�BK�BJ�BF�BJ�BK�BH�BD�B@�B?}B;dB;dB9XB8RB6FB33B1'B/B/B.B-B,B-B)�B)�B(�B(�B(�B'�B'�B&�B%�B$�B$�B#�B#�B"�B!�B �B!�B"�B!�B!�B �B �B �B �B�B �B �B �B �B �B!�B"�B!�B"�B!�B�B�B�B�B�B�B�B!�B"�B%�B%�B&�B&�B(�B(�B)�B,B,B-B-B/B0!B1'B1'B2-B33B5?B6FB6FB7LB7LB:^B9XB:^B?}BA�BB�BD�BF�BI�BI�BJ�BK�BL�BP�BP�BR�BT�BS�BS�BXBZB^5BdZBdZBe`BffBgmBjBl�Bm�Br�Bw�Bv�Bv�Bv�Bx�B�B�B�%B�=B�PB�\B�bB��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�RB�XB�dB�jB��BÖBÖBÖBĜBŢB��B��B��B�)B�ZB�yB�B�B��B��B��B��B	B	JB	�B	�B	�B	�B	%�B	&�B	'�B	(�B	(�B	+B	-B	.B	.B	0!B	2-B	6FB	8RB	9XB	?}B	D�B	F�B	H�B	I�B	J�B	K�B	O�B	R�B	W
B	YB	[#B	]/B	`BB	dZB	hsB	n�B	o�B	s�B	t�B	r�B	r�B	q�B	r�B	r�B	u�B	w�B	w�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�JB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	B	ĜB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�5B	�;B	�BB	�HB	�NB	�ZB	�ZB	�`B	�`B	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
JB
JB
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
bB
bB
hB
hB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
%�B
%�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
/B
/B
/B
/B
/B
0!B
1'B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
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
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
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
P�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
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
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
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
iyB
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
l�B
l�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B'�B&�B%�B&�B �B�B�B#�B5B9$B=<BAUBAUBBuBG�BJ�BL�BO�BP�BV�B^Bb4Bd&Bp�Bz�B~�B��B�B�gB�kB��B��B��B��B��B��B��B��BāB͹BѷB��B��B��B��B��B�B�B�B�B�B�2B�2B�8B�8B�2B�2B�2B�>B�DB�>B�8B�2B�,B�,B�2B�8B�2B�RB�XB�DB�>B�2B�B��BʦB�B�.B|�Ba-B@OB�ByB.B��B�KB��BȚB�%B��B�.B��BqvBeFBT�BO�B=<B,�B �B.BB
�>B
�*B
��B
��B
f2B
M�B
I�B
DgB
>BB
4B
"�B
kB
4B
B
�B	��B	�B	ϫB	�OB	��B	��B	��B	��B	�:B	��B	w�B	poB	g8B	X�B	T�B	S�B	DgB	<6B	,�B	 �B	xB	4B	@B	.B	B	
	B	�B��B�B�2B�B��B��BөB͟BȀB�<B�*B�B��B��B��B��B�qB�4B�"B�B�B��B��B��B~�By�Bu�Bs�Br|BqvBm]Bi*Bf2BdB`B\�BY�BV�BU�BQ�BN�BM�BKxBJ�BFtBJ�BK�BH�BDgB@OB?HB;B;B9$B8B6B2�B0�B.�B.�B-�B,�B+�B,�B)�B)�B(�B(�B(�B'�B'�B&�B%�B$�B$�B#�B#�B"�B!|B �B!�B"�B!�B!�B vB �B �B �B�B vB �B �B �B �B!|B"�B!|B"�B!|B�B�BjB�B�B�BjB!�B"�B%�B%�B&�B&�B(�B(�B)�B+�B+�B,�B,�B.�B/�B0�B0�B1�B2�B5B6B6B7B6�B:B9$B:*B?HBAUBB[BDgBFtBI�BI�BJ�BKxBL~BP�BP�BR�BT�BS�BS�BW�BY�B^BdBdBeBfBgBjKBl=BmCBr|Bw�Bv�BvzBv�Bx�B��B��B��B��B�B�(B�.B�SB�eB�dB��B��B��B��B��B��B��B��B��B�B�B�	B�0B�6B�4B�aB�aB�aB�MB�mB�rB̈́BөB��B�&B�DB�cB�B�tB��B��B��B	�B	�B	MB	KB	kB	dB	%�B	&�B	'�B	(�B	(�B	*�B	,�B	-�B	-�B	/�B	1�B	5�B	8B	9$B	?HB	DgB	FtB	HfB	IlB	JrB	KxB	O�B	R�B	V�B	X�B	Z�B	\�B	_�B	dB	h$B	nIB	oiB	s�B	tnB	r|B	r|B	q[B	raB	r|B	u�B	w�B	w�B	z�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�(B	�B	� B	�@B	�FB	�EB	�kB	�qB	�xB	�~B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�$B	�*B	�B	�6B	�"B	�(B	�BB	�HB	�;B	�;B	�AB	�MB	�_B	�zB	�_B	ȀB	ȀB	˒B	�xB	͟B	ΥB	ΥB	ϫB	ϑB	ϑB	бB	ѷB	ѝB	ңB	��B	��B	ԯB	ԯB	��B	��B	ԯB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�&B	�B	�,B	�B	�2B	�$B	�*B	�DB	�DB	�*B	�DB	�QB	�QB	�WB	�]B	�CB	�]B	�IB	�IB	�iB	�UB	�vB	�aB	�B	�tB	�nB	�nB	�nB	��B	�tB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
�B
	B

	B

	B
B
B
�B
�B
B
B
B
B
"B
B
(B
B
(B
(B
(B
.B
.B
.B
B
4B
&B
&B
FB
,B
FB
,B
MB
MB
9B
?B
?B
_B
EB
EB
KB
eB
KB
KB
KB
KB
KB
eB
eB
eB
kB
kB
WB
qB
qB
qB
WB
xB
]B
qB
qB
WB
WB
kB
kB
]B
xB
WB
qB
WB
eB
?B
YB
?B
?B
_B
_B
_B
EB
EB
eB
xB
]B
xB
xB
xB
xB
~B
�B
pB
�B
�B
 �B
pB
 vB
 �B
!�B
!�B
!�B
!|B
!�B
!|B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
 �B
 �B
 �B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
%�B
%�B
'�B
(�B
(�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
0�B
4B
3�B
5B
5B
4�B
5B
4�B
6B
7B
6�B
7B
7B
7B
6�B
7B
7B
8B
8B
8B
9$B
8B
9$B
9$B
:*B
:*B
:B
:B
:*B
:B
:*B
;B
;0B
;0B
<B
<6B
<6B
<B
<6B
<B
<B
=<B
="B
=<B
=<B
="B
=<B
="B
="B
=<B
=<B
>(B
>(B
>(B
>BB
>(B
>(B
?HB
?.B
?HB
?HB
@OB
@OB
@OB
@4B
@4B
A;B
AUB
AUB
AUB
AUB
A;B
B[B
B[B
B[B
B[B
B[B
B[B
CaB
CGB
CGB
CaB
CGB
DgB
DMB
ESB
ESB
FtB
ESB
FtB
FtB
FtB
FtB
FtB
FtB
FtB
G_B
GzB
GzB
GzB
GzB
HfB
H�B
H�B
HfB
HfB
IlB
I�B
J�B
JrB
JrB
J�B
JrB
JrB
J�B
KxB
KxB
K�B
KxB
K�B
L~B
L�B
L~B
L�B
L~B
L~B
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
P�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
^B
]�B
^B
^B
^B
^�B
^�B
_B
_B
^�B
_�B
_�B
_�B
`B
aB
aB
`�B
aB
aB
`�B
a�B
a�B
a�B
bB
a�B
bB
c B
cB
c B
cB
c B
d&B
d&B
e,B
e,B
e,B
e,B
e,B
eB
e,B
e,B
f2B
fB
e,B
e,B
e,B
e,B
eB
f2B
fB
fB
f2B
f2B
f2B
f2B
f2B
f2B
fB
f2B
f2B
fB
f2B
g8B
g8B
g8B
g8B
h>B
h$B
h>B
h>B
h$B
h>B
i*B
i*B
iDB
iDB
jKB
j0B
jKB
j0B
jKB
jKB
iDB
i*B
iDB
iDB
i*B
iDB
j0B
j0B
jKB
jKB
jKB
jKB
kQB
kQB
kQB
lWB
lWG�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.48(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805230039272018052300392720180523003927201806042358152018060423581520180604235815JA  ARFMdecpA19c                                                                20180518153518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180518063533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180518063534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180518063534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180518063535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180518063535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180518063535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180518063535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180518063535  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180518063536                      G�O�G�O�G�O�                JA  ARUP                                                                        20180518070010                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180518154319  CV  JULD            G�O�G�O�F�@                JM  ARSQJMQC2.0                                                                 20180521000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20180522153927  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180522153927  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604145815  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                