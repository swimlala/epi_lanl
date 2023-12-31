CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:43Z creation;2016-06-24T09:48:45Z conversion to V3.1;2019-12-19T08:34:26Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20160624094843  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_007                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׳�$8!�1   @׳�β@�@3ش9Xb�d���Fs�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C!HC �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�=�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D���D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D��D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D���D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D��)D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�G�A�G�A�I�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�C�A�9XA�7LA�1'A��A�%A��A��HA���A־wA�XA�p�A���A�bNA�%A��AΕ�A�&�A���A�ffA�1A̴9A��
A�z�A�5?A�O�A�Q�A��
A�~�A�(�A�XA�M�A�x�ADA�A�A�bNA���A�^5A���A��\A��A�1'A�A��A�ZA�ȴA��#A�;dA��A�-A�?}A�dZA�/A��A�M�A��HA���A��A�VA�%A�"�A��jA�VA�|�A�ƨA�$�A��RA�?}A��A�l�A�hsA�;dA��A���A��`A��A��A��A���A�JA��RA��A���A��PA�|�A�VA��wA��
A�bA�ƨA�ȴA�VA�M�A��A�z�A?}A}A{�
A{%Ay�Aw�^Au�#As�mAr��Aq�Ap�+AnĜAmO�Al�`Ak��AjQ�Aip�Ai
=Ae�Aa�A`�HA`A]G�A[�AY
=AVr�AT��AS��AQ�APz�APVAN�RAL-AJ��AI�AHJAF�AD��AB��AAS�A@�/A@ZA?33A=�TA<��A;��A:��A:n�A9��A7�A6bNA5+A4��A4�9A4bNA3�
A3S�A3+A2�yA1�A0Q�A/C�A.~�A.�A-"�A+�
A*��A)��A)XA(�!A(5?A'p�A%��A$�HA$z�A$bA#��A#O�A"r�A ~�A A��A~�A�jA��A"�A�AA�A�A��AbAS�A��AjA�TA33A�mA33AI�AjA�;AJA`BA�A/A
VA	oAjA\)A�A��A�TA��A�TA��AdZA ��@�t�@��\@�-@�G�@���@��@�dZ@�p�@��@�9X@�  @�1@�-@�r�@� �@�{@�Ĝ@�(�@�;d@�J@�1@��@㕁@�-@�%@���@���@ޗ�@���@ޏ\@އ+@�ff@�@��T@�X@���@܃@�ƨ@�ȴ@���@�Z@�33@��@�/@���@ҧ�@�O�@Ь@�z�@�9X@��;@�dZ@Χ�@�M�@�ȴ@� �@�1@�9X@��@�ƨ@�Q�@�`B@���@���@�@�~�@�@Ų-@�z�@��;@�1'@��@��@�Ĝ@�9X@��;@Ý�@Õ�@�V@�O�@��w@�&�@�X@��#@�~�@°!@�"�@�t�@���@î@�|�@�S�@��@��@\@�{@��7@�X@���@��w@�M�@�x�@�Ĝ@���@��9@�I�@�1@�o@�ff@���@�7L@��j@�ƨ@�33@���@�ȴ@��!@���@��\@�=q@���@�p�@�`B@�7L@���@�(�@��w@�S�@��@��@��@��@���@�@�hs@��/@�Ĝ@���@�r�@�A�@��
@���@�V@�5?@�{@���@��T@��7@�%@��D@�Z@��@�33@��y@��R@��+@�ff@�ff@�V@�J@��#@��-@�?}@�/@�V@��j@�z�@�bN@�A�@��@�  @���@�ƨ@��P@�\)@���@���@�~�@�V@�$�@���@���@�Q�@�(�@�  @���@��w@��@�\)@�&�@���@��@�1@��@��w@���@�1@�1'@�1'@� �@���@���@�@��y@��\@�V@�E�@�$�@�@��^@��7@�G�@�hs@��7@�7L@�%@�j@��@�\)@�;d@�
=@��@��R@��\@�ff@�$�@���@��T@��^@�`B@��D@�(�@���@�dZ@�@�@�5?@�X@��/@�Ĝ@�Z@�1'@�ƨ@�K�@��@��!@��\@�@�G�@��@��7@���@��D@� �@��F@���@�|�@�\)@�+@�@�
=@��H@���@�M�@�5?@�$�@�@��@��#@���@�x�@��@��`@��j@��D@�Q�@���@�C�@�33@��@��@�"�@�
=@���@�~�@�M�@���@��@��T@�@�X@���@�Ĝ@��u@�A�@�  @�33@���@�=q@�{@��@���@��7@�/@�9X@�dZ@��@���@��y@��H@���@���@���@�E�@���@���@�/@��@��@�%@�Ĝ@�j@\)@~��@~��@~��@~v�@}��@|��@|�@{dZ@z�H@zn�@zJ@y�#@y�^@y�7@yX@x��@w�@w�@v�R@vv�@vE�@v5?@v{@u�@u�T@u@u`B@t��@t�j@t�D@t9X@s�m@sS�@r��@r�\@r�\@r=q@q��@q��@qG�@q�@p��@p��@p�u@ol�@n��@nv�@nE�@n@m�T@m?}@l�j@lz�@l(�@k��@k��@k�m@k�m@k��@j~�@i&�@h��@hr�@hbN@hA�@h  @gl�@g
=@f�+@e��@e�-@ep�@e?}@eV@d�j@d(�@ct�@c"�@b��@b�@a��@a7L@`Ĝ@`Q�@_|�@_�@^��@^$�@^$�@^{@\��@\9X@\(�@\�@\1@[�m@[�
@[��@[C�@Z^5@Yhs@Xr�@W��@W�w@W|�@W;d@V�@V5?@T��@TZ@S��@S�F@S��@S��@SC�@R��@R�\@R^5@R-@Q7L@P��@Pr�@P �@O�@P  @O�@O�;@O��@Ol�@O+@N�R@N$�@N{@M��@Mp�@MV@L��@L�D@LZ@L(�@K�m@K�@KdZ@J�@J�\@J=q@I�#@I��@I��@I��@I�@H�`@H��@H��@HĜ@H�9@H�u@H �@G|�@G;d@G�@F��@Fȴ@F�+@F$�@F$�@F{@F@E��@E/@D(�@C��@C��@C�@C"�@C@B��@B=q@A��@Ax�@A7L@@�`@@��@@�u@@bN@?��@?K�@>ȴ@>5?@>@=�@<�D@<Z@<�@<1@;�
@;��@;@:~�@9��@9�7@9%@8��@8Ĝ@8�9@8r�@8Q�@8 �@7�w@7+@6�@6�+@6{@5@5��@5�@5O�@4��@4Z@49X@4�@4�@41@3��@3��@2��@2=q@1�#@1��@1�7@1x�@1hs@1X@1G�@1%@0��@0Ĝ@0��@0��@0�@0 �@/�@/��@/�P@.�y@.��@.ff@.E�@.{@-@-�@-O�@-?}@-�@-�@,�@,9X@+�
@+�@+33@+@*��@*��@*n�@*n�@*-@*J@)�@)��@)�7@)&�@(�`@(��@(Ĝ@(�9@(r�@( �@'�w@'�P@&�@&v�@&V@&E�@&{@%��@%@%�-@%�-@%�h@%�h@%�@%`B@%?}@%/@$�/@$�@$�D@$j@$�@#��@#�@#t�@#"�@"��@"�!@"�\@"�@!�#@!�^@!��@!�7@!hs@!7L@!%@ Ĝ@ bN@ b@�@��@�P@l�@l�@l�@\)@
=@�y@�R@�+@v�@E�@@@�-@��@�@�@Z@I�@(�@1@1@��@�m@ƨ@�
@ƨ@�@t�@S�@"�@o@�H@�!@n�@-@��@��@�^@��@X@&�@�@%@Ĝ@bN@A�@1'@1'@1'@1'@1'@1'@ �@b@  @�@  @  @�@�;@��@��@|�@
=@�y@�@ȴ@ȴ@�R@��@�+@V@�@��@@�-@�-@�h@p�@O�@�j@Z@ƨ@�F@dZ@"�@��@��@~�@=q@�#@�7@7L@&�@�`@�9@�9@�u@Q�@A�@ �@b@�;@�;@��@�w@�@�@�@�@�@�@|�@K�@+@�y@v�@ff@E�@E�@5?@{@�T@��@?}@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�G�A�G�A�I�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�C�A�9XA�7LA�1'A��A�%A��A��HA���A־wA�XA�p�A���A�bNA�%A��AΕ�A�&�A���A�ffA�1A̴9A��
A�z�A�5?A�O�A�Q�A��
A�~�A�(�A�XA�M�A�x�ADA�A�A�bNA���A�^5A���A��\A��A�1'A�A��A�ZA�ȴA��#A�;dA��A�-A�?}A�dZA�/A��A�M�A��HA���A��A�VA�%A�"�A��jA�VA�|�A�ƨA�$�A��RA�?}A��A�l�A�hsA�;dA��A���A��`A��A��A��A���A�JA��RA��A���A��PA�|�A�VA��wA��
A�bA�ƨA�ȴA�VA�M�A��A�z�A?}A}A{�
A{%Ay�Aw�^Au�#As�mAr��Aq�Ap�+AnĜAmO�Al�`Ak��AjQ�Aip�Ai
=Ae�Aa�A`�HA`A]G�A[�AY
=AVr�AT��AS��AQ�APz�APVAN�RAL-AJ��AI�AHJAF�AD��AB��AAS�A@�/A@ZA?33A=�TA<��A;��A:��A:n�A9��A7�A6bNA5+A4��A4�9A4bNA3�
A3S�A3+A2�yA1�A0Q�A/C�A.~�A.�A-"�A+�
A*��A)��A)XA(�!A(5?A'p�A%��A$�HA$z�A$bA#��A#O�A"r�A ~�A A��A~�A�jA��A"�A�AA�A�A��AbAS�A��AjA�TA33A�mA33AI�AjA�;AJA`BA�A/A
VA	oAjA\)A�A��A�TA��A�TA��AdZA ��@�t�@��\@�-@�G�@���@��@�dZ@�p�@��@�9X@�  @�1@�-@�r�@� �@�{@�Ĝ@�(�@�;d@�J@�1@��@㕁@�-@�%@���@���@ޗ�@���@ޏ\@އ+@�ff@�@��T@�X@���@܃@�ƨ@�ȴ@���@�Z@�33@��@�/@���@ҧ�@�O�@Ь@�z�@�9X@��;@�dZ@Χ�@�M�@�ȴ@� �@�1@�9X@��@�ƨ@�Q�@�`B@���@���@�@�~�@�@Ų-@�z�@��;@�1'@��@��@�Ĝ@�9X@��;@Ý�@Õ�@�V@�O�@��w@�&�@�X@��#@�~�@°!@�"�@�t�@���@î@�|�@�S�@��@��@\@�{@��7@�X@���@��w@�M�@�x�@�Ĝ@���@��9@�I�@�1@�o@�ff@���@�7L@��j@�ƨ@�33@���@�ȴ@��!@���@��\@�=q@���@�p�@�`B@�7L@���@�(�@��w@�S�@��@��@��@��@���@�@�hs@��/@�Ĝ@���@�r�@�A�@��
@���@�V@�5?@�{@���@��T@��7@�%@��D@�Z@��@�33@��y@��R@��+@�ff@�ff@�V@�J@��#@��-@�?}@�/@�V@��j@�z�@�bN@�A�@��@�  @���@�ƨ@��P@�\)@���@���@�~�@�V@�$�@���@���@�Q�@�(�@�  @���@��w@��@�\)@�&�@���@��@�1@��@��w@���@�1@�1'@�1'@� �@���@���@�@��y@��\@�V@�E�@�$�@�@��^@��7@�G�@�hs@��7@�7L@�%@�j@��@�\)@�;d@�
=@��@��R@��\@�ff@�$�@���@��T@��^@�`B@��D@�(�@���@�dZ@�@�@�5?@�X@��/@�Ĝ@�Z@�1'@�ƨ@�K�@��@��!@��\@�@�G�@��@��7@���@��D@� �@��F@���@�|�@�\)@�+@�@�
=@��H@���@�M�@�5?@�$�@�@��@��#@���@�x�@��@��`@��j@��D@�Q�@���@�C�@�33@��@��@�"�@�
=@���@�~�@�M�@���@��@��T@�@�X@���@�Ĝ@��u@�A�@�  @�33@���@�=q@�{@��@���@��7@�/@�9X@�dZ@��@���@��y@��H@���@���@���@�E�@���@���@�/@��@��@�%@�Ĝ@�j@\)@~��@~��@~��@~v�@}��@|��@|�@{dZ@z�H@zn�@zJ@y�#@y�^@y�7@yX@x��@w�@w�@v�R@vv�@vE�@v5?@v{@u�@u�T@u@u`B@t��@t�j@t�D@t9X@s�m@sS�@r��@r�\@r�\@r=q@q��@q��@qG�@q�@p��@p��@p�u@ol�@n��@nv�@nE�@n@m�T@m?}@l�j@lz�@l(�@k��@k��@k�m@k�m@k��@j~�@i&�@h��@hr�@hbN@hA�@h  @gl�@g
=@f�+@e��@e�-@ep�@e?}@eV@d�j@d(�@ct�@c"�@b��@b�@a��@a7L@`Ĝ@`Q�@_|�@_�@^��@^$�@^$�@^{@\��@\9X@\(�@\�@\1@[�m@[�
@[��@[C�@Z^5@Yhs@Xr�@W��@W�w@W|�@W;d@V�@V5?@T��@TZ@S��@S�F@S��@S��@SC�@R��@R�\@R^5@R-@Q7L@P��@Pr�@P �@O�@P  @O�@O�;@O��@Ol�@O+@N�R@N$�@N{@M��@Mp�@MV@L��@L�D@LZ@L(�@K�m@K�@KdZ@J�@J�\@J=q@I�#@I��@I��@I��@I�@H�`@H��@H��@HĜ@H�9@H�u@H �@G|�@G;d@G�@F��@Fȴ@F�+@F$�@F$�@F{@F@E��@E/@D(�@C��@C��@C�@C"�@C@B��@B=q@A��@Ax�@A7L@@�`@@��@@�u@@bN@?��@?K�@>ȴ@>5?@>@=�@<�D@<Z@<�@<1@;�
@;��@;@:~�@9��@9�7@9%@8��@8Ĝ@8�9@8r�@8Q�@8 �@7�w@7+@6�@6�+@6{@5@5��@5�@5O�@4��@4Z@49X@4�@4�@41@3��@3��@2��@2=q@1�#@1��@1�7@1x�@1hs@1X@1G�@1%@0��@0Ĝ@0��@0��@0�@0 �@/�@/��@/�P@.�y@.��@.ff@.E�@.{@-@-�@-O�@-?}@-�@-�@,�@,9X@+�
@+�@+33@+@*��@*��@*n�@*n�@*-@*J@)�@)��@)�7@)&�@(�`@(��@(Ĝ@(�9@(r�@( �@'�w@'�P@&�@&v�@&V@&E�@&{@%��@%@%�-@%�-@%�h@%�h@%�@%`B@%?}@%/@$�/@$�@$�D@$j@$�@#��@#�@#t�@#"�@"��@"�!@"�\@"�@!�#@!�^@!��@!�7@!hs@!7L@!%@ Ĝ@ bN@ b@�@��@�P@l�@l�@l�@\)@
=@�y@�R@�+@v�@E�@@@�-@��@�@�@Z@I�@(�@1@1@��@�m@ƨ@�
@ƨ@�@t�@S�@"�@o@�H@�!@n�@-@��@��@�^@��@X@&�@�@%@Ĝ@bN@A�@1'@1'@1'@1'@1'@1'@ �@b@  @�@  @  @�@�;@��@��@|�@
=@�y@�@ȴ@ȴ@�R@��@�+@V@�@��@@�-@�-@�h@p�@O�@�j@Z@ƨ@�F@dZ@"�@��@��@~�@=q@�#@�7@7L@&�@�`@�9@�9@�u@Q�@A�@ �@b@�;@�;@��@�w@�@�@�@�@�@�@|�@K�@+@�y@v�@ff@E�@E�@5?@{@�T@��@?}@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BF�BD�BC�BA�BA�B@�B>wB:^BN�BdZBl�Bs�Bx�B�\B�B�FB��BŢBɺB��B��BȴB�NB��B\B�B-BG�BdZBdZBu�B�PB��B�LBŢB�
B�)BÖB��B�fB�B�B  B�B�B�B �B�B�B�B�B{B+B+BBB��B�B�fB�5B��B�wB��B��B��B��B�VBo�BP�B1'B�B��B�ZB�B�3B�uB�DBu�BbNBVBP�BK�B>wB�B{B1B
��B
��B
�fB
�;B
��B
ĜB
�FB
��B
�1B
�B
v�B
jB
]/B
Q�B
B�B
:^B
33B
'�B
�B
bB
JB
+B	��B	�B	�yB	��B	��B	��B	��B	�7B	v�B	gmB	M�B	B�B	9XB	-B	�B	�B	{B	1B	B��B��B�B�B�fB�NB�NB�HB�5B�)B�B�B�
B�B��B��B��B��B��B��BȴBƨBŢBĜBŢBǮBƨBĜBŢBȴB��BŢB��B��B��BÖBŢB��B��B��B��B��B��B��B��B�B�B�B�5B�fB�yB�B�B�B�B�B�B�B�B�B�B�sB�mB�TB�HB�;B�B��BƨB�!B�!B�-B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB�uB�uB�oB�PB�1B�JB�PB�bB��B��B��B��B��B��B��B��B��B�uB�hB�{B��B��B��B��B�9B��B��B��B��BÖBÖBĜBĜBŢBŢBB�}B�}B��BŢBƨB��B�/B�B�B�B�
B��B��B��B��B��BǮBɺB��B��B�/B�yB��B	B	+B	�B	�B	�B	PB��B��B	B	DB	hB	49B	9XB	9XB	9XB	7LB	33B	.B	+B	�B	%�B	I�B	R�B	VB	YB	^5B	_;B	gmB	hsB	iyB	l�B	m�B	n�B	p�B	s�B	u�B	u�B	r�B	y�B	|�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�VB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�XB	�^B	�dB	�dB	�dB	�dB	�jB	��B	B	ÖB	ĜB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�
B	�
B	�B	�B	�B	�
B	�#B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�mB	�yB	�sB	�sB	�mB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
+B
+B
+B
%B
%B
%B
B
B
%B
+B
+B
+B
+B
	7B
+B
+B
1B
1B
1B
	7B

=B
JB
JB
PB
VB
VB
VB
VB
\B
bB
bB
bB
hB
oB
oB
uB
uB
uB
{B
{B
{B
uB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
33B
2-B
33B
2-B
33B
33B
33B
33B
5?B
7LB
6FB
6FB
6FB
6FB
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
9XB
9XB
9XB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
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
A�B
A�B
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
C�B
D�B
E�B
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
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
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
O�B
O�B
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
YB
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
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
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
aHB
aHB
bNB
bNB
bNB
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
e`B
ffB
ffB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
k�B
k�B
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
q�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
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
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
w�B
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
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BF�BD�BC�BA�BA�BAoBA BA�BR�Be`BmCBt9By�B�HB��B�B�UBƨB�^B��B�6B�dB�&B��B.B�B/ BI�BfLBg�Bu�B�jB�B�XBȚB�7B��B�YB��B��B��B�aBAB�B#nB!-B#�B!HB#B�B�BBKBB�BB�(B�|B�B�\B�_B��B�NB��B�5B�B�[Bt�BVB6B�B��B��B��B�LB��B��BxlBc�BW
BR:BN�BBuB�B�B	�B
�"B
��B
�
B
�B
�@B
ǔB
�B
�B
��B
�uB
y	B
lqB
_pB
TB
C�B
;�B
4�B
)�B
;B
NB
�B
�B	�0B	�GB	�]B	�{B	�:B	�qB	��B	��B	y�B	jeB	PB	DB	;JB	.�B	�B	�B	YB	
#B	�B�B�0B�B�B��B�B�TB��B��BݲB�7B�B�B�sB�BԯB�4B�PB�B�DB�lB�EB��B�SB�EB�RB��BŢBƎB�	B�~B�+BªB�AB�[BāB��B�~B�B�vBуBңB��B�{B�$B��B��B�B�\B�B�0B�QB�B�WB��B�B�qB�wB�IB�CB�kB�B�B�B��B�bB�eB�mB�7B�oB��B��B��B�B�FB��B�OB��B�B��B��B�_B��B��B��B��B�B�,B��B��B��B��B��B��B��B��B��B�\B�B��B�\B��B��B��B��B�[B��B�B�	B��B��B�B��B��B��B��B��B��B��B�B�YB�YBðB�B�OB�BƨB�_B�0B�B�kB�KB�_B�YBՁBӏB��B��B�0B��BɠB�B��B��B�B��B	 �B	�B	B	�B	QB	�B�0B��B	UB	
XB	.B	49B	9�B	9�B	9�B	88B	4B	/5B	,B	B	#�B	IB	R�B	U�B	X�B	^5B	_B	gmB	h�B	i�B	l�B	m�B	o B	qB	tB	vFB	v�B	s�B	z^B	}<B	B	�;B	�AB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	��B	�B	��B	��B	��B	��B	�B	�`B	�_B	�=B	�5B	�OB	�AB	�|B	��B	��B	��B	�xB	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	ȴB	ȴB	ɺB	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�,B	�2B	�MB	�2B	�B	�9B	�9B	�mB	֡B	�_B	�+B	�?B	�?B	�$B	�EB	ؓB	�+B	�sB	�SB	�B	�B	�$B	�	B	�B	�B	�;B	�\B	�|B	�B	�B	�tB	�B	�zB	�B	�B	�sB	�B	�B	�B	�LB	�RB	�B	��B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�B	�B	��B	�B	�B	�rB	�DB	�$B	�B	�$B	�B	�6B	�JB	�B	�"B	�"B	�VB	�<B	��B
 B
uB
UB
;B
[B
'B
'B
B
B
-B
9B
_B
fB
fB
EB
EB
+B
?B
?B
YB
SB
9B
YB
EB
_B
zB
�B
	lB
+B
EB
1B
1B
KB
	lB

rB
~B
~B
PB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
oB
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
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
B
B
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
#B
#B
$B
$�B
$�B
$�B
%B
$�B
%�B
%�B
%�B
%�B
&B
&LB
'B
'B
&�B
&�B
'B
'B
($B
(
B
(�B
)B
(�B
)B
)B
)*B
)DB
)_B
+6B
+B
,B
,"B
,"B
,"B
-CB
-)B
-CB
-)B
-B
-)B
./B
.IB
.IB
.IB
.IB
.IB
.IB
/OB
/OB
/OB
/OB
/iB
0UB
0UB
1[B
1AB
1AB
1�B
3hB
2B
3B
2GB
3MB
33B
3MB
3hB
5�B
7�B
6�B
6`B
6`B
6FB
7fB
7�B
8�B
8�B
8�B
8lB
9rB
9rB
9rB
9rB
9�B
:xB
9rB
9�B
9�B
;�B
;B
;B
<�B
<jB
<�B
=�B
=qB
=�B
=�B
=�B
=�B
>�B
=qB
=�B
>�B
>�B
>wB
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@iB
@�B
@�B
A�B
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
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
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
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
N�B
N�B
OB
O�B
O�B
PB
O�B
P�B
Q B
Q B
QB
Q B
Q�B
RB
Q�B
RB
RB
R B
S@B
S&B
TB
TB
S�B
S�B
S�B
S�B
TB
TB
UB
T�B
T�B
UB
UB
T�B
UB
VB
VB
VB
VB
W$B
W$B
W
B
W
B
X+B
XB
W�B
X+B
X+B
X+B
XEB
YKB
Z7B
Z7B
[#B
[#B
[=B
\CB
\)B
\CB
\CB
\CB
\B
\CB
]IB
]/B
]/B
]/B
]IB
]IB
^OB
^jB
_VB
_pB
`\B
`\B
`\B
`\B
`\B
aHB
aHB
aHB
abB
aHB
aHB
abB
bhB
bhB
bNB
cnB
cnB
cnB
dtB
dtB
dtB
ezB
ezB
ezB
ezB
ezB
e�B
f�B
ffB
ezB
e`B
ezB
ezB
ezB
ezB
e�B
f�B
ffB
ffB
f�B
gmB
gmB
gmB
g�B
g�B
g�B
g�B
h�B
h�B
hsB
hsB
i�B
iyB
i�B
i�B
i�B
j�B
k�B
k�B
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
mwB
m�B
m�B
m�B
n}B
n�B
n}B
n�B
n�B
n�B
n�B
n�B
n}B
n�B
n�B
o�B
o�B
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
q�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
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
w�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
w�B
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
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606221350332016062213503320160622135033201806221257552018062212575520180622125755201804050656172018040506561720180405065617  JA  ARFMdecpA19c                                                                20160624183508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094843  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094843  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094844  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094844  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094844  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094844  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094844  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160624094845                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102544                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160622045021  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160622045033  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160622045033  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215617  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035755  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                