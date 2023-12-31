CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-13T21:35:12Z creation;2018-01-13T21:35:16Z conversion to V3.1;2019-12-19T07:51:52Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180113213512  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_199                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Ds 1   @�Ds��� @;)�Y��}�dT��q�j1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B�RB(�B0�B8�B@�BH�BP�BX�B`�Bg�RBp�Bx�B�\B�\B�\B�\B�\B�\B��)B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B��)B�\B�\B�\B�\B�\B�\B��)B�\B�\B�\C �C!HC�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Ci�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�RD>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�RDE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�D)D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�DŽ�D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�M�D�d)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A���A��
A��A��#A��#A��/A��/A��;A��;A��;A��HA��HA��HA��TA��HA���A�XA��A�ĜA�`BA��/A��A���A���A���A���A��uA�z�A�t�A�hsA�VA�"�A�  A��`A���A���A�ƨA��^A���A�r�A�K�A��A���A�ZA�oA���A�1'A���A�ƨA�33A�ffA�JA���A�bA�-A��A��7A�n�A�(�A���A�C�A�+A�?}A��A�(�A���A�7LA���A���A�ZA���A�?}A�jA��A�"�A���A��A��A�C�A�E�A}��A{+Az1'Ay?}Aw�AwK�Av$�As�
Ar�uAq�Ao��An�HAn=qAm�Am�^Al�Ak�;Aj��Ajn�Aj-Aj  Ai�^Ait�AhĜAg|�Af��Ae�
AcXAbz�Aa��Aa�;Aa��Aal�A`bA^~�A]�A\�A\^5A[ƨAZz�AX��AW�AV �AU��AU|�AU`BAU&�AT  AQ\)AN�\ANJAMAMC�ALĜAL��AL��ALbNAK;dAJ�AI�FAH��AG\)AGO�AF�AF{AEdZAD�RAChsABJA@=qA>�A>ZA=��A=�7A=+A=oA<�A<�!A<v�A<=qA;�A;�PA:��A8��A7K�A6��A5��A5t�A3�hA29XA1A0z�A0�A/�^A.�yA-�^A-+A-
=A,�/A,�A+l�A*$�A(��A&��A%�TA%��A%t�A%�A#
=A!|�A �DA��AQ�A�A��A5?AƨAXAoA��AVA�A�hA�wA��A�HA��A1'A�hA7LA�!A=qA�
A/A��A1A��A1AJA�A7LA��A�A�DA�;A
�A	�
A�AĜAn�A�^A�hA��AdZA��A A�@���@�+@��@��!@��^@��-@��-@��@��D@�A�@��@��@�/@���@��@���@�K�@�!@�x�@� �@@�ȴ@�@��@웦@�9X@��@�R@���@�\)@���@�~�@�@���@���@�{@�-@�  @ݲ-@ܛ�@��
@�K�@�=q@ش9@�  @�ƨ@��#@��@��/@��/@��/@�Ĝ@�o@�ff@��@Ѓ@�{@�?}@̼j@��;@ʸR@�$�@���@ɉ7@�?}@ȴ9@���@Ų-@�Ĝ@Ý�@�n�@���@���@�ƨ@�\)@��@�~�@�E�@��@�;d@�x�@���@�J@�7L@��u@��@��;@���@���@�C�@��7@�(�@��P@�ȴ@�$�@�@��-@�G�@��@��`@�9X@�@�ȴ@�-@�7L@��@��u@�bN@��m@���@��H@��@���@��#@��@���@�~�@���@��@�l�@�S�@�;d@���@��\@��@�@��@�G�@��j@�dZ@���@���@�$�@���@��9@�z�@�(�@��F@���@�S�@�;d@��@��H@��+@�M�@��T@�hs@�&�@���@��9@�r�@��@�K�@��H@�=q@�x�@�hs@�X@�/@��/@��@��m@���@�S�@�;d@�+@��@��!@�5?@�hs@���@��j@�ƨ@�C�@�ȴ@�v�@�^5@�{@��h@�X@��@��`@��@��u@�bN@���@��
@��w@��w@��@�;d@���@��@��@���@���@�hs@�&�@�j@� �@��@�dZ@��@�"�@�;d@�
=@���@��R@���@���@��H@��H@���@�J@���@��7@�O�@�%@���@��D@��@�I�@�;@|�@~�y@~$�@|��@{��@z�@z��@z^5@y��@yx�@yx�@yX@y&�@y�@y%@y%@x��@y%@y�@y�@y&�@y%@x�u@v�R@u�-@u�@t�D@s�m@s�F@s��@sdZ@s"�@r��@r^5@q��@q��@qhs@qG�@q�@q%@p��@p��@p��@p�u@p �@n��@n�y@nv�@n5?@n5?@n$�@n{@m�@m`B@m�@mp�@m/@l�/@l1@k��@k��@l1@k�
@kS�@k"�@j�H@j�!@j^5@i��@i��@ix�@ihs@i�@h �@g��@g��@g+@f�@f��@f5?@e�T@e?}@dZ@d1@c��@c�
@c�F@c��@cdZ@c33@b��@b^5@a��@a�^@a��@`�`@`bN@`1'@_�;@_\)@^ȴ@^��@^ff@^$�@]�T@]�T@]�T@]�-@\��@\9X@\1@[��@[�
@[ƨ@[��@[dZ@["�@[@Z��@Zn�@ZJ@Y�@XĜ@X �@W�;@W�w@W\)@V�R@V5?@U�-@U�@T��@Tj@TZ@T(�@S�F@S�@SdZ@SC�@So@So@R�@R=q@RJ@Q��@Qhs@P�`@PbN@Pb@O�w@Ol�@N�R@N�+@N5?@N$�@M�T@M��@MO�@M/@L�@L�@L�D@LI�@K�
@KdZ@KS�@KC�@Ko@J�!@J~�@J^5@JM�@JM�@J�@I��@I&�@H��@H��@H�`@H�9@H�9@H�9@H��@G�;@G�P@G\)@G;d@G+@F��@F�R@F��@F5?@F@E��@E/@D��@D�D@DZ@D�@C�F@C"�@B�H@B��@B��@B��@B��@B�@A�^@AG�@@A�@@b@?�;@?�w@?��@?�@>E�@>@=�h@=`B@=�@=V@=V@<��@<�/@<�@<j@<1@;�@;C�@;o@:��@:=q@9��@9��@9��@9��@9�7@8��@8�u@7�@7�P@7\)@7K�@7K�@7+@6v�@65?@5��@5@5�h@5/@5/@5?}@5O�@5`B@5`B@5/@4��@4�j@4�D@4j@49X@4�@3�F@3t�@2~�@2=q@2-@1�#@1�^@1�7@1x�@17L@17L@17L@17L@17L@1&�@1�@0��@0��@0��@0�u@0bN@/��@.V@.$�@.@-�@-��@-@-�-@-�h@-�@-`B@-?}@-�@,��@,j@,1@+��@+�@+S�@+33@+o@+o@*�H@*n�@)�@)��@)G�@(��@(�`@(1'@(  @'��@'�@'�@'�P@&��@&��@&V@&$�@%�@%`B@%?}@$�/@$z�@$j@#��@#C�@#dZ@#33@#S�@#dZ@#C�@#33@#33@#@"�H@"��@"��@"��@"��@"�\@"~�@"M�@"�@!�@!�^@!�7@!G�@!7L@!7L@!%@ ��@ ��@ Q�@ A�@ 1'@  �@�;@�w@�P@l�@+@ȴ@v�@E�@{@�@�-@V@��@z�@Z@�@�
@ƨ@�F@��@�@t�@dZ@dZ@dZ@C�@33@"�@@�!@�\@-@J@�#@�^@��@hs@hs@G�@%@�`@�9@Q�@b@��@��@�w@�w@�w@��@�P@\)@;d@�@��@�y@�y@�R@ff@{@�h@?}@V@�/@�@Z@��@�m@�m@��@"�@�H@�\@n�@-@�@�@�@x�@&�@��@��@r�@b@�@�w@+@�y@�@ȴ@�+@�@@�h@?}@/@V@�@�/@��@��@�@Z@ƨ@�F@��@��@��@dZ@C�@33@
�@
��@
�!@
�!@
��@
�\@
~�@
n�@
n�@
n�@
n�@
^5@
-@	��@	�7@	hs@	X@	G�@	7L@	�@��@�u@bN@A�@A�@1'@  @�;@�w@K�@�y@ȴ@�R@�R@�R@��@ff@E�@@��@��@p�@`B@�@�j@�D@j@(�@�F@��@��@��@dZ@C�@C�@"�@@�!@~�@^5@-@�@J@��@�@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A���A��
A��A��#A��#A��/A��/A��;A��;A��;A��HA��HA��HA��TA��HA���A�XA��A�ĜA�`BA��/A��A���A���A���A���A��uA�z�A�t�A�hsA�VA�"�A�  A��`A���A���A�ƨA��^A���A�r�A�K�A��A���A�ZA�oA���A�1'A���A�ƨA�33A�ffA�JA���A�bA�-A��A��7A�n�A�(�A���A�C�A�+A�?}A��A�(�A���A�7LA���A���A�ZA���A�?}A�jA��A�"�A���A��A��A�C�A�E�A}��A{+Az1'Ay?}Aw�AwK�Av$�As�
Ar�uAq�Ao��An�HAn=qAm�Am�^Al�Ak�;Aj��Ajn�Aj-Aj  Ai�^Ait�AhĜAg|�Af��Ae�
AcXAbz�Aa��Aa�;Aa��Aal�A`bA^~�A]�A\�A\^5A[ƨAZz�AX��AW�AV �AU��AU|�AU`BAU&�AT  AQ\)AN�\ANJAMAMC�ALĜAL��AL��ALbNAK;dAJ�AI�FAH��AG\)AGO�AF�AF{AEdZAD�RAChsABJA@=qA>�A>ZA=��A=�7A=+A=oA<�A<�!A<v�A<=qA;�A;�PA:��A8��A7K�A6��A5��A5t�A3�hA29XA1A0z�A0�A/�^A.�yA-�^A-+A-
=A,�/A,�A+l�A*$�A(��A&��A%�TA%��A%t�A%�A#
=A!|�A �DA��AQ�A�A��A5?AƨAXAoA��AVA�A�hA�wA��A�HA��A1'A�hA7LA�!A=qA�
A/A��A1A��A1AJA�A7LA��A�A�DA�;A
�A	�
A�AĜAn�A�^A�hA��AdZA��A A�@���@�+@��@��!@��^@��-@��-@��@��D@�A�@��@��@�/@���@��@���@�K�@�!@�x�@� �@@�ȴ@�@��@웦@�9X@��@�R@���@�\)@���@�~�@�@���@���@�{@�-@�  @ݲ-@ܛ�@��
@�K�@�=q@ش9@�  @�ƨ@��#@��@��/@��/@��/@�Ĝ@�o@�ff@��@Ѓ@�{@�?}@̼j@��;@ʸR@�$�@���@ɉ7@�?}@ȴ9@���@Ų-@�Ĝ@Ý�@�n�@���@���@�ƨ@�\)@��@�~�@�E�@��@�;d@�x�@���@�J@�7L@��u@��@��;@���@���@�C�@��7@�(�@��P@�ȴ@�$�@�@��-@�G�@��@��`@�9X@�@�ȴ@�-@�7L@��@��u@�bN@��m@���@��H@��@���@��#@��@���@�~�@���@��@�l�@�S�@�;d@���@��\@��@�@��@�G�@��j@�dZ@���@���@�$�@���@��9@�z�@�(�@��F@���@�S�@�;d@��@��H@��+@�M�@��T@�hs@�&�@���@��9@�r�@��@�K�@��H@�=q@�x�@�hs@�X@�/@��/@��@��m@���@�S�@�;d@�+@��@��!@�5?@�hs@���@��j@�ƨ@�C�@�ȴ@�v�@�^5@�{@��h@�X@��@��`@��@��u@�bN@���@��
@��w@��w@��@�;d@���@��@��@���@���@�hs@�&�@�j@� �@��@�dZ@��@�"�@�;d@�
=@���@��R@���@���@��H@��H@���@�J@���@��7@�O�@�%@���@��D@��@�I�@�;@|�@~�y@~$�@|��@{��@z�@z��@z^5@y��@yx�@yx�@yX@y&�@y�@y%@y%@x��@y%@y�@y�@y&�@y%@x�u@v�R@u�-@u�@t�D@s�m@s�F@s��@sdZ@s"�@r��@r^5@q��@q��@qhs@qG�@q�@q%@p��@p��@p��@p�u@p �@n��@n�y@nv�@n5?@n5?@n$�@n{@m�@m`B@m�@mp�@m/@l�/@l1@k��@k��@l1@k�
@kS�@k"�@j�H@j�!@j^5@i��@i��@ix�@ihs@i�@h �@g��@g��@g+@f�@f��@f5?@e�T@e?}@dZ@d1@c��@c�
@c�F@c��@cdZ@c33@b��@b^5@a��@a�^@a��@`�`@`bN@`1'@_�;@_\)@^ȴ@^��@^ff@^$�@]�T@]�T@]�T@]�-@\��@\9X@\1@[��@[�
@[ƨ@[��@[dZ@["�@[@Z��@Zn�@ZJ@Y�@XĜ@X �@W�;@W�w@W\)@V�R@V5?@U�-@U�@T��@Tj@TZ@T(�@S�F@S�@SdZ@SC�@So@So@R�@R=q@RJ@Q��@Qhs@P�`@PbN@Pb@O�w@Ol�@N�R@N�+@N5?@N$�@M�T@M��@MO�@M/@L�@L�@L�D@LI�@K�
@KdZ@KS�@KC�@Ko@J�!@J~�@J^5@JM�@JM�@J�@I��@I&�@H��@H��@H�`@H�9@H�9@H�9@H��@G�;@G�P@G\)@G;d@G+@F��@F�R@F��@F5?@F@E��@E/@D��@D�D@DZ@D�@C�F@C"�@B�H@B��@B��@B��@B��@B�@A�^@AG�@@A�@@b@?�;@?�w@?��@?�@>E�@>@=�h@=`B@=�@=V@=V@<��@<�/@<�@<j@<1@;�@;C�@;o@:��@:=q@9��@9��@9��@9��@9�7@8��@8�u@7�@7�P@7\)@7K�@7K�@7+@6v�@65?@5��@5@5�h@5/@5/@5?}@5O�@5`B@5`B@5/@4��@4�j@4�D@4j@49X@4�@3�F@3t�@2~�@2=q@2-@1�#@1�^@1�7@1x�@17L@17L@17L@17L@17L@1&�@1�@0��@0��@0��@0�u@0bN@/��@.V@.$�@.@-�@-��@-@-�-@-�h@-�@-`B@-?}@-�@,��@,j@,1@+��@+�@+S�@+33@+o@+o@*�H@*n�@)�@)��@)G�@(��@(�`@(1'@(  @'��@'�@'�@'�P@&��@&��@&V@&$�@%�@%`B@%?}@$�/@$z�@$j@#��@#C�@#dZ@#33@#S�@#dZ@#C�@#33@#33@#@"�H@"��@"��@"��@"��@"�\@"~�@"M�@"�@!�@!�^@!�7@!G�@!7L@!7L@!%@ ��@ ��@ Q�@ A�@ 1'@  �@�;@�w@�P@l�@+@ȴ@v�@E�@{@�@�-@V@��@z�@Z@�@�
@ƨ@�F@��@�@t�@dZ@dZ@dZ@C�@33@"�@@�!@�\@-@J@�#@�^@��@hs@hs@G�@%@�`@�9@Q�@b@��@��@�w@�w@�w@��@�P@\)@;d@�@��@�y@�y@�R@ff@{@�h@?}@V@�/@�@Z@��@�m@�m@��@"�@�H@�\@n�@-@�@�@�@x�@&�@��@��@r�@b@�@�w@+@�y@�@ȴ@�+@�@@�h@?}@/@V@�@�/@��@��@�@Z@ƨ@�F@��@��@��@dZ@C�@33@
�@
��@
�!@
�!@
��@
�\@
~�@
n�@
n�@
n�@
n�@
^5@
-@	��@	�7@	hs@	X@	G�@	7L@	�@��@�u@bN@A�@A�@1'@  @�;@�w@K�@�y@ȴ@�R@�R@�R@��@ff@E�@@��@��@p�@`B@�@�j@�D@j@(�@�F@��@��@��@dZ@C�@C�@"�@@�!@~�@^5@-@�@J@��@�@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BhBbBbB\BJBDB\BoBoBoBoBhBbBbB\BVBPBDBJB
=B
=B
=B	7B%BB  B��B��B�B�fB�/B��B�FB�=BaHBN�BD�B8RB,BuB��BŢB�B`BBVB33B-B �B{BVBPB	7B1BB  B
��B
�B
�HB
��B
ȴB
ƨB
�jB
�9B
�{B
�%B
z�B
jB
k�B
dZB
]/B
XB
N�B
=qB
8RB
/B
'�B
&�B
"�B
!�B
�B
�B
uB
VB
JB
DB

=B
+B
B	��B	�B	�B	�yB	�
B	�B	�
B	�
B	��B	��B	B	�^B	�9B	�'B	�B	��B	��B	�\B	�1B	�%B	�+B	�%B	�B	|�B	o�B	^5B	R�B	`BB	aHB	]/B	[#B	YB	XB	R�B	I�B	D�B	A�B	;dB	49B	9XB	49B	-B	'�B	#�B	�B	\B	DB	B		7B	1B	1B	%B	+B	%B	B	B��B��B��B�B�BB�B�5B�B��BǮBŢBĜBƨBƨBĜB�qB�^B�^B�qB�dB�LB�B��B��B��B��B��B��B��B�+B�B�B� Bz�B� Bz�B}�B{�By�Bx�Bu�Bp�BffBZBZBO�B[#B`BB]/BYB[#BXBW
BW
BR�BT�BT�B[#B[#BYBVBS�BW
BT�BS�BK�BG�BD�BH�BM�BK�BF�BI�BB�B6FB �B/B7LB6FB0!B1'B<jBA�B@�B>wB<jB;dB7LB6FB7LB8RB6FB7LB7LB6FB5?B2-B6FB49B33B5?B5?B6FB33B1'B.B/B49B49B1'B-B&�B(�B(�B!�B�B%�B)�B+B+B(�B.B-B'�B-B49B33B2-B0!B(�B)�B+B$�B�B(�B(�B&�B#�B'�B)�B)�B(�B%�B"�B�B%�B%�B$�B,B(�B,B-B.B.B.B%�B$�B(�B,B49B:^B>wB?}BD�BC�BA�B?}B:^B>wBE�BG�BK�BQ�BQ�BS�BW
BW
BT�BS�B_;B^5BbNBl�Bm�Br�Bv�Bx�B�B�B�B|�B|�B~�B�DB�=B�=B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�FB�FB�XB��BBÖBBBĜBǮB��B��B�B�B�B�B�B�B�5B�;B�NB�TB�NB�HB�BB�HB�`B�sB�fB�B��B��B��B��B	B	B		7B	PB	\B	oB	oB	oB	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	$�B	%�B	%�B	%�B	,B	-B	,B	.B	2-B	49B	49B	5?B	8RB	9XB	:^B	:^B	:^B	:^B	:^B	=qB	=qB	>wB	?}B	A�B	F�B	G�B	F�B	F�B	I�B	I�B	I�B	H�B	O�B	R�B	VB	VB	T�B	W
B	XB	YB	ZB	\)B	_;B	bNB	e`B	jB	m�B	n�B	p�B	p�B	p�B	p�B	s�B	w�B	y�B	{�B	}�B	}�B	~�B	~�B	� B	�B	�B	�B	�+B	�DB	�DB	�JB	�JB	�PB	�VB	�VB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�RB	�qB	��B	B	B	B	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�5B	�HB	�HB	�HB	�TB	�ZB	�ZB	�TB	�NB	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
	7B
	7B
	7B
1B
1B
	7B
DB
PB
JB
JB
PB
PB
PB
JB
VB
\B
\B
\B
\B
\B
bB
\B
bB
hB
hB
uB
{B
{B
uB
uB
oB
uB
{B
{B
{B
{B
uB
uB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
%�B
$�B
#�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
)�B
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
,B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
33B
33B
33B
33B
49B
33B
33B
33B
1'B
0!B
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
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
9XB
;dB
<jB
<jB
=qB
<jB
?}B
?}B
?}B
@�B
?}B
>wB
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
C�B
B�B
C�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
G�B
I�B
I�B
I�B
H�B
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
I�B
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
O�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
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
S�B
S�B
S�B
S�B
T�B
W
B
VB
W
B
W
B
W
B
XB
XB
XB
W
B
YB
YB
ZB
ZB
[#B
[#B
ZB
YB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
[#B
[#B
\)B
\)B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
^5B
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
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
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBB B.B6BJB�B�BoBoBoB�B�B}B�B�B�B�B~B
rB
=B
XB	lBtB�B iB�qB��B�B�8BބBбB�^B��BfLBPbBE�B9�B-�B9B��B�JB��Bc�BY�B8RB/�B#:B?BBpB
=B�B�B �B
��B
�}B
�B
��B
��B
��B
�(B
��B
�KB
�B
~(B
l�B
l�B
e�B
^�B
YB
P�B
@B
9�B
0�B
)_B
'�B
#�B
"4B
 \B
�B
{B
\B
�B
�B

�B
�B
�B	��B	�9B	�B	�B	ٴB	�	B	׍B	�?B	�MB	ϫB	�MB	�B	�tB	��B	��B	��B	�~B	��B	��B	�EB	��B	�tB	�{B	}�B	q�B	a|B	U�B	`�B	a�B	]�B	[�B	YeB	XEB	S�B	KB	E�B	B�B	<�B	5�B	9�B	4�B	.B	(�B	$�B	qB	NB	jB	�B		�B	�B	�B	�B	_B	tB	{B	uB�cB�jB��B��B��B��B�B�7B�2B�	B�_B�B�_B�EB�SB��B��B�B��B��B�B��B��B��B��B��B�B�/B��B��B��B�tB�oB|�B� B{�B~�B|�BzxByXBv`Bq�Bh>B\xB\xBRoB[�B`�B]�BZB[�BX�BW�BW�BS�BU�BU�B[=B[=BYKBV�BT�BWsBU�BTaBL�BIBFBI�BN<BLdBG�BJXBC�B9	B$�B0�B7�B7B1�B2|B<�BA�B@�B>�B<�B;�B8B6�B7�B8�B6�B7�B7�B6�B6+B3B6�B4�B3�B5�B5�B6�B3�B2B/iB/�B4�B4�B1�B-�B($B)�B)�B# B;B&�B*�B+�B+�B)�B.�B-�B)B-�B4TB33B2aB0�B*B*�B+�B%�B 'B)�B)yB'�B$�B(>B*KB*KB)DB&�B#�B5B&�B&�B%�B,�B)�B,�B-wB.cB.cB.}B'B&LB*0B-]B5%B:�B>�B@ BD�BC�BA�B@ B;�B?cBFBHKBLJBRBR:BTFBW?BWYBU�BT�B_�B^�BcBl�Bm�Br�Bw2ByrB�'B�'B�;B}�B}�B�B��B��B�)B�"B��B��B��B��B��B��B��B��B�7B�sB�B�B�BB�|B�$B�B�WB�6B�IB�CB�B�OB�OB�oB�zB��B��B��B��B��B��B��B�B�B�DB�6B�B�B�9B�SB�EB�B�OBߊB�hB�nB�B�B��B��B��B�B�8B��B�2B�B�B�HB	oB	SB		lB	�B	�B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%B	&B	&B	&LB	,=B	-CB	,qB	.IB	2-B	49B	4nB	5tB	8RB	9XB	:^B	:^B	:^B	:�B	:�B	=�B	=�B	>�B	?�B	A�B	F�B	G�B	F�B	F�B	I�B	J	B	J#B	I7B	P.B	S@B	VB	V9B	UB	W$B	X+B	Y1B	Z7B	\CB	_;B	bNB	ezB	jB	mwB	n�B	p�B	p�B	p�B	qAB	tB	xB	zB	|B	~B	~B	~�B	~�B	�4B	�;B	�GB	�9B	�EB	�^B	�^B	�JB	�JB	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	��B	�B	�B	�"B	�CB	�'B	�TB	�?B	�zB	�lB	��B	��B	ªB	��B	��B	żB	��B	��B	��B	ɺB	��B	�B	�"B	�.B	��B	��B	��B	�B	�B	�B	�B	�9B	�?B	�EB	�1B	�7B	�KB	�WB	�IB	�dB	�IB	�jB	�HB	�bB	�bB	�nB	�ZB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�"B	�B	�(B	�B	�(B
 B
B
'B
'B
'B
-B
3B
-B
-B
3B
3B
3B
SB
B
EB
EB
_B
KB
	RB
	7B
	7B
KB
fB
	lB
^B
PB
dB
dB
PB
PB
jB
�B
pB
vB
vB
vB
vB
\B
}B
�B
}B
�B
�B
uB
{B
�B
�B
�B
�B
�B
{B
aB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 �B
 �B
"�B
#�B
$�B
%�B
$�B
$B
$�B
%B
&�B
(
B
(
B
(�B
(�B
)�B
)�B
)�B
*B
*B
*B
+B
,"B
,B
,B
,"B
-B
,=B
0;B
1AB
1AB
1AB
1AB
2-B
2GB
33B
33B
4B
3B
33B
3B
3MB
49B
3MB
3MB
3hB
1�G�O�B
6`B
7fB
7LB
7fB
7LB
7LB
7fB
7fB
7fB
8lB
7fB
7fB
7fB
8lB
8RB
9XB
:^B
:xB
:^B
:^B
:^B
9�B
9rB
;B
<�B
<jB
=qB
<�B
?}B
?}B
?�B
@�B
?�B
>�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
@�B
A�B
C�B
B�B
C�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
G�B
I�B
I�B
I�B
H�B
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
J	B
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
O�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
Q B
Q B
Q B
P�B
Q B
Q�B
P�B
Q B
RB
RB
RB
RB
SB
S�B
S�B
S�B
S�B
TB
TB
TB
TB
TB
T�B
T�B
T�B
TB
TB
TB
T,B
UB
W$B
VB
W$B
W$B
W$B
XB
X+B
X+B
W?B
Y1B
Y1B
Z7B
ZB
[#B
[	B
ZB
YKB
ZB
[=B
[=B
[=B
[=B
\CB
\)B
[=B
[=B
\)B
\CB
[=B
[=B
\)B
]IB
]IB
^5B
^OB
^5B
^5B
^B
_;B
_VB
^jB
^jB
aHB
aHB
a-B
aHB
abB
abB
aHB
abB
bNB
bhB
cTB
cTB
c:B
cTB
cTB
c:B
cTB
cTB
cnB
cnB
b�B
cnB
dZB
dZB
d@B
dtB
dtB
d�B
e`B
ezB
f�B
fLB
ffB
f�B
f�B
ffB
f�B
f�B
hXB
hsB
hXB
hsB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801180033562018011800335620180118003356201806221236162018062212361620180622123616201804050432452018040504324520180405043245  JA  ARFMdecpA19c                                                                20180114063511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180113213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180113213514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180113213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180113213515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180113213515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180113213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180113213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180113213516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180113213516                      G�O�G�O�G�O�                JA  ARUP                                                                        20180113215455                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180114153542  CV  JULD            G�O�G�O�F�#�                JM  ARSQJMQC2.0                                                                 20180115000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20180117153356  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180117153356  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193245  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                