CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:40Z creation;2016-06-24T09:48:42Z conversion to V3.1;2019-12-19T08:34:40Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160624094840  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_006                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @ײ��� 1   @ײ�I�@3�l�C���d�2�W��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dك3D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D^��D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg{�Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D���D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D��)D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dل)D���D� �D�@�Dڀ�D��)D� �D�@�Dۀ�D���D�)D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D��D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D�)D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��\D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�v�A�v�A�t�A�r�A�n�A�5?A���A֬Aև+A�n�A�Q�A�;dA�/A�$�A�&�A�(�A�(�A�(�A�(�A���A�bA�VA�bA�/AθRA�VA��HA�ĜA�+A��A̾wA̗�A�9XA˼jA���Aɝ�A�\)A��TA�r�A���A�\)A���A���A�&�A��A�-A���A��RA�{A��DA�ZA��+A��A���A��!A���A�{A���A�
=A�
=A��yA��RA���A��7A���A�C�A���A���A��9A�n�A��DA���A��\A��mA��A�G�A�VA�E�A�(�A�5?A��hA���A�7LA�p�A�-A�dZA��9A��jA�Q�A���A�oA���A�&�A��A�p�A�1A��A��A�=qA��+A���A��wA�^5A�p�A�(�A}hsAy\)AxbAv��Av�Av�At��Ar�/Aq��Ao�
AlAi��Ai;dAe�Aa�mAa
=A_|�A]�7AZ�AWC�AU�^AT�jAT$�ARM�AQ?}AM�hAK
=AJ�AI�-AG;dAE`BAC?}AA/A?%A=hsA<5?A;|�A:$�A8ĜA6��A6�A4�RA4�A3�A1S�A0A�A/�-A/\)A.v�A-�FA,��A+��A+t�A+O�A+�A*^5A({A'�7A&�A#`BA!�FA!/A A�A��A`BA�A��A�mAl�AO�A&�Az�Al�A�A�-AE�A�yA�A�DAO�A1'A;dA�TAl�A+A�A�9A �A��A��A9XAhsA
��A
r�A	��A�uAp�A�AE�A�`A�A;dA�A�A ȴA E�@�\)@���@���@���@�x�@�9X@��@�-@�?}@�u@�1'@�F@�|�@�@���@��@�"�@�n�@�@���@��@��#@�X@��@�(�@�t�@��H@ᙚ@�%@�Z@߮@�33@�
=@ާ�@�^5@݉7@�;d@�n�@�-@ى7@ج@�/@��@���@׍P@�ȴ@�E�@��T@ղ-@�/@ҏ\@Ͼw@̣�@�`B@̼j@�(�@�(�@ˍP@�5?@�%@��`@��/@ȣ�@��H@�J@�+@��@�bN@�x�@��#@�@�|�@Ĵ9@�1'@�@��y@ÍP@���@�{@�
=@�(�@�7L@őh@�@š�@���@� �@þw@�"�@�$�@�{@��@���@��@��P@���@��@�G�@�?}@���@�1@��;@��
@�K�@��R@��@��-@�%@�Q�@���@���@�$�@���@�?}@��@��D@��@��@�|�@�dZ@��@��R@�$�@�p�@��`@�r�@�Z@�A�@�  @���@�l�@�+@�o@���@���@�$�@��^@���@��7@�p�@�O�@��@��/@���@�j@�9X@�1'@�(�@� �@��@��m@���@��@�^5@�E�@�E�@�V@�J@��@���@��7@�?}@��`@��@�A�@�C�@���@��R@���@�n�@�J@�@�x�@��/@�r�@�b@��@�;d@�o@�@��H@���@�n�@��@��7@�r�@�C�@��@��y@�@��@���@���@��`@���@��@�?}@�p�@�x�@�`B@�`B@�G�@�x�@�V@�Z@�Z@�j@�  @��@�\)@��
@�ƨ@��;@�  @�b@�b@�1@��;@��w@���@���@��@��@���@�ff@�ff@�-@���@�hs@�O�@�/@�j@��@���@�\)@�@�V@��@��j@��D@��u@���@�r�@�j@��@�M�@�J@��T@��^@�?}@���@�/@�G�@�G�@�%@�1'@��@�@�v�@��^@��h@��@�`B@��@��@��m@�l�@��@��y@���@��R@�~�@��T@�p�@�7L@��@���@�z�@�Q�@��@�  @���@��@��m@��;@���@��@��y@���@�n�@�=q@�5?@�$�@�{@�@��T@��^@���@���@�`B@��/@�Ĝ@��j@��u@�bN@� �@���@�\)@�K�@�C�@�o@��@�ȴ@���@�~�@�V@�M�@�{@���@��7@��7@�x�@�O�@���@��@��@�I�@�b@
=@~��@}�@}�h@}`B@}�@{t�@zJ@y�@y�@y�@y��@y�7@yhs@y%@xĜ@x�@xr�@xA�@x �@w�@v�@vff@v{@u�@u�@t��@tj@s��@s@r�!@r~�@q��@q�7@q&�@p��@pĜ@p1'@o�P@o;d@n�y@n��@nV@n$�@n@m`B@lZ@k�
@k�@ko@j�\@j^5@i��@h��@h�9@h1'@gl�@f��@f��@f@e��@ep�@d��@d��@dj@c�F@c33@b�H@b��@b�!@bn�@a�7@`Ĝ@`bN@`Q�@`1'@_�@_�@_l�@_\)@_;d@^�y@^��@^$�@]`B@\�@\��@\I�@\�@[�F@[S�@["�@Z�@Z~�@Y�^@Y�@X�u@Xr�@XA�@W�@Wl�@V��@V�@Vȴ@V�R@V�R@V��@V�+@V$�@U��@U`B@T��@TZ@T9X@T(�@S��@S�@S33@R�@R��@R��@R��@R^5@R=q@R-@RJ@Q��@Q��@Qhs@Q7L@P��@PQ�@O�@O��@O\)@O;d@Nȴ@Nff@N$�@M��@M`B@M?}@M�@L�@L�@Lz�@L�@K�
@KdZ@J��@Jn�@I�#@I��@I�@H��@Hr�@H �@H  @G�@G��@G|�@G+@F�@F��@FV@E�@E�@EV@D�@D�@C��@Ct�@C33@B��@B�\@A��@A��@A��@A�@Ax�@@�`@@�9@@�u@@Q�@@  @?��@?�w@?�w@?�@?��@?l�@?�@>�@>$�@=@=p�@=?}@<�@<Z@<�@<�@<�@<�@;��@;ƨ@;��@;33@:~�@:�@9G�@8�`@8r�@8Q�@81'@8 �@8b@8  @8  @8  @8  @8  @8  @7�@7��@7+@6�+@6@5@5�h@5`B@5�@4��@4j@4�@3�m@3��@3dZ@3o@2�H@2M�@1�#@1�7@1X@1�@0��@0Q�@0 �@0  @/�P@/K�@/+@/�@/
=@/
=@.��@.ȴ@.��@.v�@.E�@.$�@.$�@.@-�@-@-�@-`B@-`B@-O�@-/@-/@-V@,��@,z�@,9X@,�@+�F@+�@+dZ@+S�@+@*�!@*~�@*^5@*�@)�^@)x�@)�@(��@(�@(bN@(1'@'K�@&v�@&$�@&@%��@%�h@%O�@%V@$�@$�D@$I�@$�@$1@#�
@#�F@#��@#t�@#33@#o@"�H@"M�@"J@!��@!X@ �`@ �@ �u@ ��@ �9@ �9@ �9@ ��@ �@ 1'@�@+@�@�@�+@ff@V@E�@$�@@��@�-@�@�@��@�@z�@Z@�@ƨ@�@�@t�@C�@33@33@33@33@33@33@"�@o@�@�\@J@��@�7@x�@hs@X@��@��@1'@�@|�@|�@|�@|�@|�@|�@|�@l�@+@v�@E�@{@@��@@@��@�h@�@p�@`B@?}@�@�j@�D@I�@9X@(�@1@�
@��@dZ@33@o@�@�!@��@~�@^5@M�@=q@�@�@�^@��@&�@Ĝ@��@Q�@1'@1'@  @�;@�w@�w@�w@�P@;d@��@��@�R@��@��@v�@V@$�@�T@�-@�h@?}@�@��@�j@�@�D@z�@�D@�D@Z@�@��@t�@dZ@33@@
��@
�!@
~�@
=q@	�@	�^@	�7@	x�@	hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�x�A�v�A�v�A�t�A�r�A�n�A�5?A���A֬Aև+A�n�A�Q�A�;dA�/A�$�A�&�A�(�A�(�A�(�A�(�A���A�bA�VA�bA�/AθRA�VA��HA�ĜA�+A��A̾wA̗�A�9XA˼jA���Aɝ�A�\)A��TA�r�A���A�\)A���A���A�&�A��A�-A���A��RA�{A��DA�ZA��+A��A���A��!A���A�{A���A�
=A�
=A��yA��RA���A��7A���A�C�A���A���A��9A�n�A��DA���A��\A��mA��A�G�A�VA�E�A�(�A�5?A��hA���A�7LA�p�A�-A�dZA��9A��jA�Q�A���A�oA���A�&�A��A�p�A�1A��A��A�=qA��+A���A��wA�^5A�p�A�(�A}hsAy\)AxbAv��Av�Av�At��Ar�/Aq��Ao�
AlAi��Ai;dAe�Aa�mAa
=A_|�A]�7AZ�AWC�AU�^AT�jAT$�ARM�AQ?}AM�hAK
=AJ�AI�-AG;dAE`BAC?}AA/A?%A=hsA<5?A;|�A:$�A8ĜA6��A6�A4�RA4�A3�A1S�A0A�A/�-A/\)A.v�A-�FA,��A+��A+t�A+O�A+�A*^5A({A'�7A&�A#`BA!�FA!/A A�A��A`BA�A��A�mAl�AO�A&�Az�Al�A�A�-AE�A�yA�A�DAO�A1'A;dA�TAl�A+A�A�9A �A��A��A9XAhsA
��A
r�A	��A�uAp�A�AE�A�`A�A;dA�A�A ȴA E�@�\)@���@���@���@�x�@�9X@��@�-@�?}@�u@�1'@�F@�|�@�@���@��@�"�@�n�@�@���@��@��#@�X@��@�(�@�t�@��H@ᙚ@�%@�Z@߮@�33@�
=@ާ�@�^5@݉7@�;d@�n�@�-@ى7@ج@�/@��@���@׍P@�ȴ@�E�@��T@ղ-@�/@ҏ\@Ͼw@̣�@�`B@̼j@�(�@�(�@ˍP@�5?@�%@��`@��/@ȣ�@��H@�J@�+@��@�bN@�x�@��#@�@�|�@Ĵ9@�1'@�@��y@ÍP@���@�{@�
=@�(�@�7L@őh@�@š�@���@� �@þw@�"�@�$�@�{@��@���@��@��P@���@��@�G�@�?}@���@�1@��;@��
@�K�@��R@��@��-@�%@�Q�@���@���@�$�@���@�?}@��@��D@��@��@�|�@�dZ@��@��R@�$�@�p�@��`@�r�@�Z@�A�@�  @���@�l�@�+@�o@���@���@�$�@��^@���@��7@�p�@�O�@��@��/@���@�j@�9X@�1'@�(�@� �@��@��m@���@��@�^5@�E�@�E�@�V@�J@��@���@��7@�?}@��`@��@�A�@�C�@���@��R@���@�n�@�J@�@�x�@��/@�r�@�b@��@�;d@�o@�@��H@���@�n�@��@��7@�r�@�C�@��@��y@�@��@���@���@��`@���@��@�?}@�p�@�x�@�`B@�`B@�G�@�x�@�V@�Z@�Z@�j@�  @��@�\)@��
@�ƨ@��;@�  @�b@�b@�1@��;@��w@���@���@��@��@���@�ff@�ff@�-@���@�hs@�O�@�/@�j@��@���@�\)@�@�V@��@��j@��D@��u@���@�r�@�j@��@�M�@�J@��T@��^@�?}@���@�/@�G�@�G�@�%@�1'@��@�@�v�@��^@��h@��@�`B@��@��@��m@�l�@��@��y@���@��R@�~�@��T@�p�@�7L@��@���@�z�@�Q�@��@�  @���@��@��m@��;@���@��@��y@���@�n�@�=q@�5?@�$�@�{@�@��T@��^@���@���@�`B@��/@�Ĝ@��j@��u@�bN@� �@���@�\)@�K�@�C�@�o@��@�ȴ@���@�~�@�V@�M�@�{@���@��7@��7@�x�@�O�@���@��@��@�I�@�b@
=@~��@}�@}�h@}`B@}�@{t�@zJ@y�@y�@y�@y��@y�7@yhs@y%@xĜ@x�@xr�@xA�@x �@w�@v�@vff@v{@u�@u�@t��@tj@s��@s@r�!@r~�@q��@q�7@q&�@p��@pĜ@p1'@o�P@o;d@n�y@n��@nV@n$�@n@m`B@lZ@k�
@k�@ko@j�\@j^5@i��@h��@h�9@h1'@gl�@f��@f��@f@e��@ep�@d��@d��@dj@c�F@c33@b�H@b��@b�!@bn�@a�7@`Ĝ@`bN@`Q�@`1'@_�@_�@_l�@_\)@_;d@^�y@^��@^$�@]`B@\�@\��@\I�@\�@[�F@[S�@["�@Z�@Z~�@Y�^@Y�@X�u@Xr�@XA�@W�@Wl�@V��@V�@Vȴ@V�R@V�R@V��@V�+@V$�@U��@U`B@T��@TZ@T9X@T(�@S��@S�@S33@R�@R��@R��@R��@R^5@R=q@R-@RJ@Q��@Q��@Qhs@Q7L@P��@PQ�@O�@O��@O\)@O;d@Nȴ@Nff@N$�@M��@M`B@M?}@M�@L�@L�@Lz�@L�@K�
@KdZ@J��@Jn�@I�#@I��@I�@H��@Hr�@H �@H  @G�@G��@G|�@G+@F�@F��@FV@E�@E�@EV@D�@D�@C��@Ct�@C33@B��@B�\@A��@A��@A��@A�@Ax�@@�`@@�9@@�u@@Q�@@  @?��@?�w@?�w@?�@?��@?l�@?�@>�@>$�@=@=p�@=?}@<�@<Z@<�@<�@<�@<�@;��@;ƨ@;��@;33@:~�@:�@9G�@8�`@8r�@8Q�@81'@8 �@8b@8  @8  @8  @8  @8  @8  @7�@7��@7+@6�+@6@5@5�h@5`B@5�@4��@4j@4�@3�m@3��@3dZ@3o@2�H@2M�@1�#@1�7@1X@1�@0��@0Q�@0 �@0  @/�P@/K�@/+@/�@/
=@/
=@.��@.ȴ@.��@.v�@.E�@.$�@.$�@.@-�@-@-�@-`B@-`B@-O�@-/@-/@-V@,��@,z�@,9X@,�@+�F@+�@+dZ@+S�@+@*�!@*~�@*^5@*�@)�^@)x�@)�@(��@(�@(bN@(1'@'K�@&v�@&$�@&@%��@%�h@%O�@%V@$�@$�D@$I�@$�@$1@#�
@#�F@#��@#t�@#33@#o@"�H@"M�@"J@!��@!X@ �`@ �@ �u@ ��@ �9@ �9@ �9@ ��@ �@ 1'@�@+@�@�@�+@ff@V@E�@$�@@��@�-@�@�@��@�@z�@Z@�@ƨ@�@�@t�@C�@33@33@33@33@33@33@"�@o@�@�\@J@��@�7@x�@hs@X@��@��@1'@�@|�@|�@|�@|�@|�@|�@|�@l�@+@v�@E�@{@@��@@@��@�h@�@p�@`B@?}@�@�j@�D@I�@9X@(�@1@�
@��@dZ@33@o@�@�!@��@~�@^5@M�@=q@�@�@�^@��@&�@Ĝ@��@Q�@1'@1'@  @�;@�w@�w@�w@�P@;d@��@��@�R@��@��@v�@V@$�@�T@�-@�h@?}@�@��@�j@�@�D@z�@�D@�D@Z@�@��@t�@dZ@33@@
��@
�!@
~�@
=q@	�@	�^@	�7@	x�@	hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBe`Be`BffBffBhsBz�B�B� B}�B{�By�Bx�Bw�Bv�Bw�Bx�By�Bz�B{�Bx�Bu�B� B�DB�{B��B�bB�oB��B��B��B��B��B��B��B��B�BhB�B'�B:^BC�BL�Bn�B�\B��B�!B�B��B��BhB�B�B�B�B�B!�B#�B)�B+B(�B33B�B�)B�B��B�B��BǮB�B�B��B��Bt�BF�BhB��B�B��B��B��B�%Bk�B]/B8RB/B�BbB
=B
�B
�B
�/B
�NB
�B
�B
�NB
��B
�qB
�B
��B
��B
�uB
�+B
�%B
x�B
e`B
YB
5?B
-B
#�B
%�B
%�B
�B
DB
B
B	�`B	�B	��B	�^B	��B	�oB	�7B	� B	o�B	]/B	O�B	H�B	E�B	:^B	49B	%�B	�B	hB	JB	%B��B�B�B�TB�5B�#B�
B��B��B��B��B��B��B��B��B��B��B��B��BɺBȴBȴB��B��B��B��B��B��B��B��BÖBĜBĜBBB��B��BBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��BɺBƨBĜB��B�}B�qB�jB�^B�FB�3B�-B�!B�B�B��B��B�B�'B�B�B��B��B�B��B��B��B��B��B��B��B��B��B��B�uB�oB�oB�oB�hB�hB�bB�bB�bB�\B�bB�uB�{B�uB�uB��B��B��B��B��B��B��B�'B�FB�qB�jB�dB�wB�jB��B��B�wBBƨBɺBȴBƨBƨBɺB��B��B�B��B��B�
B�#B�)B�5B�5B�)B�B�B�#B�5B�/B�/B�TB�B�B��B	B	B	B�B�B�B�B��B	B��B	hB	&�B	B�B	J�B	K�B	Q�B	YB	cTB	ffB	k�B	o�B	o�B	n�B	s�B	u�B	u�B	w�B	{�B	� B	�B	�%B	�%B	�B	�B	�B	�%B	�7B	�JB	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�FB	�LB	�^B	�jB	�jB	�jB	�qB	�qB	�wB	�}B	��B	��B	B	B	B	B	B	B	B	��B	��B	��B	B	ŢB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	��B	�
B	�
B	��B	��B	��B	��B	�B	�B	�5B	�BB	�HB	�NB	�ZB	�ZB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
PB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
bB
hB
hB
hB
hB
hB
hB
hB
hB
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
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
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
-B
.B
-B
-B
-B
,B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
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
@�B
@�B
@�B
A�B
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
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
H�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
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
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
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
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
dZB
dZB
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
iyB
iyB
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
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
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
v�B
w�B
w�B
w�B
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
x�B
x�B
x�B
x�B
x�B
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
{�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBe`Be`Bf�Bf�BiB{dB��B�OB~(B|BzBx�Bw�Bv�Bw�Bx�By�B{0B}B{�B{B��B��B�gB�SB�NB��B��B�TB�RB�sB��B�LB�=B�MB�B:B�B)DB;dBEBN�Bp;B�HB��B��BٚB��B 4B�BWB5B �B�B �B$@B'8B-CB-�B,WB7�BdBߊB��B�B�B��B�KB�sB�KBרB�eBx�BJ#B�B��B��B� B��B��B�1Bm�Ba-B;�B2�B�BB�B
�B
�B
�OB
�B
�ZB
�B
�`B
�<B
�B
��B
��B
��B
��B
��B
��B
|B
i*B
\�B
7B
.IB
$�B
&�B
'�B
�B
B
EB
B	��B	��B	��B	�(B	�OB	��B	�B	��B	s3B	_B	QNB	J	B	G�B	<�B	8B	(�B	�B	&B	\B	�B��B�zB�!B�`B߾B�CB��B��B�B�9BԕB��B�vB��B�BΥBΊB��B��B�B��B��B�B�~B�PB�:B��BѷB҉BΥBĜBżB�mB��B�B�ABB�B��B�1B��B�4B�MB�SBּB�SB�oBѝBуB�\B�\B�VBʌBǔBƎBªB�OB�]B�<B�B�LB�B��B�vB��B�cB��B�eB��B�aB�B�B��B��B��B�B�>B�B��B��B��B��B�xB�B�B��B��B��B�B�oB��B� B� B��B�HB��B�,B��B��B�,B�$B�B��B�;B�-B�fB�eB�[B��B��B�<B��B��B��B�B��B�wB��B�_B�#B�7B�B�B�#BбB��B��B�aB��B�sBیB�]B��B�!B��B�KB�QBۦB�!B�dBܒB�B�IB�AB��B	�B	�B	uB�TB�B�B�B��B	�B�]B	�B	&fB	BAB	J�B	LB	RoB	Y�B	c�B	f�B	l"B	o�B	o�B	o�B	s�B	u�B	v`B	xlB	|PB	�B	�uB	��B	�YB	�SB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	�'B	�NB	�ZB	�DB	�WB	�/B	�B	�OB	�oB	�aB	�hB	�ZB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	ªB	ªB	��B	��B	�B	��B	��B	��B	B	żB	��B	��B	��B	�B	�B	�B	�<B	�vB	�@B	�B	�B	�,B	�FB	�&B	�@B	�TB	�:B	�B	�:B	�:B	�B	�B	�B	�,B	�MB	�aB	ՁB	��B	֡B	�B	��B	��B	�MB	רB	׍B	�B	��B	��B	��B	�B	�B	�OB	�'B	�bB	�4B	�B	�B	�4B	�nB	�B	�B	�@B	�B	�sB	�qB	�}B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�LB	��B	��B	��B	��B	�B	�B	�ZB	��B	��B	��B	��B	�B	��B	�0B	��B	��B	�B	�B	��B	��B	��B
 B
UB
�B
[B
�B
 iB	�cB	�B	�B	�(B	�]B	�HB
 iB
[B
'B
-B
-B
B
mB
�B
_B
fB
fB
fB
	7B

XB
^B
DB
)B
^B
^B
DB
~B
�B
jB
pB
�B
vB
vB
vB
}B
}B
}B
}B
}B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
}B
}B
�B
hB
�B
�B
�B
oB
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
7B
B
�B
�B
�B
�B
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
"B
#B
#�B
#�B
$B
%B
$�B
$�B
$�B
&B
&B
%�B
&�B
'B
'B
(
B
(
B
($B
)DB
*0B
*B
*0B
*B
*0B
+QB
,"B
,"B
,=B
,WB
-CB
-)B
-CB
.B
-)B
-CB
-)B
,"B
-CB
-CB
-)B
./B
/5B
/OB
0oB
0UB
0;B
0!B
0;B
0;B
0;B
1AB
1AB
1'B
1AB
1AB
1[B
1vB
1AB
2-B
2GB
2GB
2aB
3MB
3MB
3MB
3hB
4�B
4nB
5ZB
5ZB
5ZB
5tB
5ZB
6`B
6`B
6+B
6FB
6FB
6`B
6`B
6`B
7fB
7LB
7�B
8lB
8lB
8lB
8RB
9�B
9XB
9rB
9rB
9rB
:^B
:xB
:xB
:xB
:xB
:xB
:xB
:xB
:xB
:�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=qB
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>wB
?�B
?�B
?�B
@�B
@�B
@�B
A�B
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
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
H�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
MB
MB
NB
NB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
PB
O�B
Q B
Q B
Q B
Q B
Q B
RB
RB
RB
RB
SB
SB
R�B
SB
T,B
TB
TB
TB
UB
UB
UB
UB
UB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W$B
W$B
W
B
W$B
W$B
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
X+B
X+B
X+B
X+B
Y1B
Y1B
Y1B
YB
Y1B
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[WB
\CB
\CB
\CB
\]B
\xB
]~B
^5B
^OB
^OB
^OB
^OB
^OB
^5B
_VB
_VB
_VB
_VB
_VB
`\B
`\B
`\B
`\B
`\B
`BB
a|B
abB
abB
bhB
bhB
cnB
dZB
dZB
ffB
ffB
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
iyB
iyB
i_B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
jB
j�B
j�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
lqB
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
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
v�B
w�B
w�B
w�B
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
x�B
x�B
x�B
x�B
x�B
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
zB
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
|B
|B
|B
{�B
{�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606221351002016062213510020160622135100201806221257472018062212574720180622125747201804050656072018040506560720180405065607  JA  ARFMdecpA19c                                                                20160624183522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094840  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094840  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094840  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094841  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094841  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094841  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094841  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160624094842                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102544                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160611153633  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160622045100  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160622045100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215607  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035747  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                