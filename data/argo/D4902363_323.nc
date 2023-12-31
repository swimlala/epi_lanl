CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-21T00:37:02Z creation;2019-01-21T00:37:08Z conversion to V3.1;2019-12-19T07:22:52Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190121003702  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              CA   JA  I2_0576_323                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ءr�K 1   @ءs��J @9ə�����dPw�kP�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dу3D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@���A z�A"{A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�RDN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dф)D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՄ)D���D� �D�@�Dր�D���D� �D�D)Dׄ)D���D�)D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�\D�=�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�C�A�G�A�I�A�G�A�G�A�5?A�1'A�;dA�33A�1'A��A���A��TA��#A���A��FA�z�A�dZA�XA�XA�XA�VA�Q�A�Q�A�S�A�S�A�VA�XA�VA�M�A�$�A�ȴA��A���A��A�t�A�dZA��DA���A���A��A�dZA�A�A�K�A�O�A���A�G�A��jA�VA���A�5?A��\A���A��A�&�A�A�VA�jA��A�JA�bA��;A�;dA���A��A��A�r�A��A��HA���A�E�A�?}A� �A��yA���A��A��9A�n�A�1'A���A��mA���A�p�A�~�A�Q�A�ĜA��A�z�A���A�E�A�+A�hsA���A�5?A��`A��!A���A��FA���A�{A��9A�M�A�r�A��A��`A�hsA�K�A��hAO�A~��A~��A~A�A}�wA|�uA{��A{C�Az~�Ay/Aw�^Av�+Au/AqƨAm|�Ai�
Ahz�Ag�FAfA�AeVAdAcG�Ab�Aa�Aa;dA`�yA`(�A_�A_�7A^M�A\=qAZ�yAZ$�AWAV�yAVjAUK�ATz�AS�mARȴAQ/APAO�7AOdZAOC�AN��AN�uAM�AMhsALjAK��AI�
AHr�AH1AGp�AF�AFȴAF�\AE�wADAC��AC;dAC�AB�jAB  AA�A?\)A=�7A<��A;�
A:�+A9l�A8ffA7��A6�\A6(�A4z�A49XA3ƨA3�7A2�A1��A1p�A0v�A0-A/�PA.VA-&�A+�A+
=A)��A)x�A)XA)C�A)7LA)A'�A'%A&-A%O�A$��A$�DA$r�A#�A"��A"bNA"  A!ƨA!O�A"�A�A��Al�A�A�mA�hA�HA�A5?A��A�AAn�A�A�\AVAp�A\)A\)AXA;dA�A�Ar�A�TAp�AO�A7LA/A��Av�A�;AK�A�A
�/A
E�Av�A9XA�^Al�AC�A�yAr�A�AhsA �!A  �@�ƨ@�l�@��R@�E�@�-@��@��-@��h@��@��+@���@��@��!@���@��+@��+@�v�@�{@�1@���@�~�@�@�K�@�`B@�u@��@�`B@蛦@�-@��`@���@�v�@�=q@��@�7@�j@��#@�7L@���@���@�
=@ٙ�@��/@׍P@�V@��`@�1@ӕ�@�o@�$�@�j@�\)@�@���@��/@�z�@��m@�l�@��@���@�n�@ɡ�@�&�@�|�@�$�@ř�@�&�@��/@��@��@� �@��H@�A�@�^5@���@�z�@��F@��R@���@��D@�b@�@�ff@�$�@��@��R@��h@���@�j@��@���@��@�\)@��@��-@�?}@��@���@�z�@�r�@�Z@�Z@�bN@�Q�@�1'@��w@��P@�+@�v�@���@�X@�/@�p�@�@�hs@�b@�\)@��R@�5?@���@�x�@�V@��`@�(�@��F@�
=@��@��\@��@���@���@�9X@���@�C�@��@�@���@�&�@�Z@���@�\)@�@���@���@���@�G�@�1'@�;d@��@���@���@��R@���@�^5@�@�p�@���@�A�@���@�t�@���@�E�@�J@��^@�x�@���@��/@�j@��w@�l�@�"�@��@���@���@��+@��+@��\@���@�v�@�$�@�@���@��@���@���@�G�@��@��@��+@��@��^@��^@��-@���@�x�@�X@�G�@��@���@��@��
@�\)@��@�
=@��@��R@���@�K�@�C�@�o@��R@��+@�^5@��@��-@�X@�7L@��@��`@�z�@�  @�;@��@��@l�@~�R@~ff@~{@}@}�@}p�@}`B@}?}@|��@|Z@|9X@|(�@|(�@|�@{��@{t�@{C�@z��@z��@z�@y�#@yG�@xA�@w��@wl�@wK�@vȴ@v{@v$�@v$�@v$�@uV@t(�@sƨ@s�@s33@r��@r��@r~�@r-@qG�@p �@o�P@nȴ@n@mO�@l��@l�j@l�D@l�@k33@j��@j�!@jn�@jM�@j-@j�@i��@i��@i7L@i%@h��@h�u@h1'@h  @gK�@f�+@fV@f$�@f{@f@e�T@e��@eO�@d�D@c��@b�\@b=q@a�@a��@`��@`��@`�@`Q�@` �@_��@_\)@_
=@^�+@^ff@^ff@^$�@]@]p�@]?}@]�@\��@\�/@\�j@\�@\�@\�D@\9X@[�m@\9X@\(�@[S�@Z�!@ZM�@Y��@Y��@Y�^@Y��@Y�#@Y�^@Yx�@X�`@XbN@W�@W\)@W�@V�+@Vff@VE�@U@U`B@UV@T��@T�@T��@Tj@TZ@T1@R�@Q�#@Q7L@PĜ@P�9@P��@P�u@Pr�@P �@O|�@O�@N�@N5?@L�/@L��@LZ@L(�@L1@K��@K�
@K�F@Kƨ@K��@KS�@J��@JM�@I�^@I��@Ihs@I7L@I�@HĜ@H��@Hr�@H  @G�@G�P@Gl�@G;d@F��@Fff@F$�@E�h@D�@Dz�@C�m@C�F@C"�@B-@A��@A%@@Ĝ@@�9@@��@@�u@@r�@@Q�@@ �@@  @?�w@?�P@?;d@>�+@>5?@=�@=�T@=@=��@=p�@=�@<�@<�/@<�/@<�/@<�/@<�/@<�j@<�@<Z@;ƨ@;��@;t�@;dZ@;S�@;33@:��@:~�@:~�@:^5@:=q@:=q@:J@9�^@9x�@9&�@8Ĝ@8�9@8Q�@8b@7��@7l�@7;d@7�@6��@6E�@65?@5@5�@4�D@4�@3�m@3�
@3�F@3�F@3�F@3�@3S�@333@3o@2�H@2�\@1��@1x�@1G�@1�@0��@0�9@0 �@/�w@/��@/|�@/K�@/
=@.��@.V@.5?@.@-p�@-V@,�@,I�@+�
@+��@+C�@+33@+o@*�!@*^5@*=q@*-@*J@)�@)�^@)�7@)G�@)%@(Ĝ@(r�@(A�@(b@'�@'l�@';d@'�@&�R@&ff@&5?@%��@%��@%�@%O�@%/@%V@$�@$��@$9X@#��@#dZ@#S�@#33@#"�@#@"��@"^5@"-@"-@"-@"J@"J@!�@!��@!hs@ ��@ �@ �@ bN@ bN@ Q�@ Q�@  �@�;@�@�P@�@�@ȴ@ff@{@�T@��@��@�-@��@�@O�@/@�@�j@z�@Z@9X@ƨ@ƨ@��@dZ@33@�@�\@n�@n�@M�@J@��@�#@��@��@��@��@��@��@�^@�^@��@�7@X@��@�`@r�@b@�w@�@�@��@�P@|�@|�@|�@l�@+@�@V@$�@$�@@��@�-@��@�h@p�@?}@�@�@�j@��@�D@I�@9X@��@t�@�H@��@�\@M�@=q@��@��@��@��@hs@X@X@7L@%@��@��@Ĝ@�@A�@1'@ �@ �@  @�@��@�@��@
=@�@ȴ@��@v�@V@@��@`B@�@�@z�@I�@9X@(�@�@ƨ@�@dZ@33@o@o@@
�H@
�\@
^5@
-@
J@	�#@	�^@	&�@��@Ĝ@��@r�@�@��@�P@|�@|�@\)@��@�@�R@ff@5?@5?@5?@5?@{@�T@@�@/@��@�D@Z@1@ƨ@��@�@t�@dZ@S�@33@"�@o@o@�!@n�@^5@M�@-@J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�C�A�C�A�G�A�I�A�G�A�G�A�5?A�1'A�;dA�33A�1'A��A���A��TA��#A���A��FA�z�A�dZA�XA�XA�XA�VA�Q�A�Q�A�S�A�S�A�VA�XA�VA�M�A�$�A�ȴA��A���A��A�t�A�dZA��DA���A���A��A�dZA�A�A�K�A�O�A���A�G�A��jA�VA���A�5?A��\A���A��A�&�A�A�VA�jA��A�JA�bA��;A�;dA���A��A��A�r�A��A��HA���A�E�A�?}A� �A��yA���A��A��9A�n�A�1'A���A��mA���A�p�A�~�A�Q�A�ĜA��A�z�A���A�E�A�+A�hsA���A�5?A��`A��!A���A��FA���A�{A��9A�M�A�r�A��A��`A�hsG�O�G�O�AO�A~��A~��A~A�A}�wA|�uA{��A{C�Az~�Ay/Aw�^Av�+Au/AqƨAm|�Ai�
Ahz�Ag�FAfA�AeVAdAcG�Ab�Aa�Aa;dA`�yA`(�A_�A_�7A^M�A\=qAZ�yAZ$�AWAV�yAVjAUK�ATz�AS�mARȴAQ/APAO�7AOdZAOC�AN��AN�uAM�AMhsALjAK��AI�
AHr�AH1AGp�AF�AFȴAF�\AE�wADAC��AC;dAC�AB�jAB  AA�A?\)A=�7A<��A;�
A:�+A9l�A8ffA7��A6�\A6(�A4z�A49XA3ƨA3�7A2�A1��A1p�A0v�A0-A/�PA.VA-&�A+�A+
=A)��A)x�A)XA)C�A)7LA)A'�A'%A&-A%O�A$��A$�DA$r�A#�A"��A"bNA"  A!ƨA!O�A"�A�A��Al�A�A�mA�hA�HA�A5?A��A�AAn�A�A�\AVAp�A\)A\)AXA;dA�A�Ar�A�TAp�AO�A7LA/A��Av�A�;AK�A�A
�/A
E�Av�A9XA�^Al�AC�A�yAr�A�AhsA �!A  �@�ƨ@�l�@��R@�E�@�-@��@��-@��h@��@��+@���@��@��!@���@��+@��+@�v�@�{@�1@���@�~�@�@�K�@�`B@�u@��@�`B@蛦@�-@��`@���@�v�@�=q@��@�7@�j@��#@�7L@���@���@�
=@ٙ�@��/@׍P@�V@��`@�1@ӕ�@�o@�$�@�j@�\)@�@���@��/@�z�@��m@�l�@��@���@�n�@ɡ�@�&�@�|�@�$�@ř�@�&�@��/@��@��@� �@��H@�A�@�^5@���@�z�@��F@��R@���@��D@�b@�@�ff@�$�@��@��R@��h@���@�j@��@���@��@�\)@��@��-@�?}@��@���@�z�@�r�@�Z@�Z@�bN@�Q�@�1'@��w@��P@�+@�v�@���@�X@�/@�p�@�@�hs@�b@�\)@��R@�5?@���@�x�@�V@��`@�(�@��F@�
=@��@��\@��@���@���@�9X@���@�C�@��@�@���@�&�@�Z@���@�\)@�@���@���@���@�G�@�1'@�;d@��@���@���@��R@���@�^5@�@�p�@���@�A�@���@�t�@���@�E�@�J@��^@�x�@���@��/@�j@��w@�l�@�"�@��@���@���@��+@��+@��\@���@�v�@�$�@�@���@��@���@���@�G�@��@��@��+@��@��^@��^@��-@���@�x�@�X@�G�@��@���@��@��
@�\)@��@�
=@��@��R@���@�K�@�C�@�o@��R@��+@�^5@��@��-@�X@�7L@��@��`@�z�@�  @�;@��@��@l�@~�R@~ff@~{@}@}�@}p�@}`B@}?}@|��@|Z@|9X@|(�@|(�@|�@{��@{t�@{C�@z��@z��@z�@y�#@yG�@xA�@w��@wl�@wK�@vȴ@v{@v$�@v$�@v$�@uV@t(�@sƨ@s�@s33@r��@r��@r~�@r-@qG�@p �@o�P@nȴ@n@mO�@l��@l�j@l�D@l�@k33@j��@j�!@jn�@jM�@j-@j�@i��@i��@i7L@i%@h��@h�u@h1'@h  @gK�@f�+@fV@f$�@f{@f@e�T@e��@eO�@d�D@c��@b�\@b=q@a�@a��@`��@`��@`�@`Q�@` �@_��@_\)@_
=@^�+@^ff@^ff@^$�@]@]p�@]?}@]�@\��@\�/@\�j@\�@\�@\�D@\9X@[�m@\9X@\(�@[S�@Z�!@ZM�@Y��@Y��@Y�^@Y��@Y�#@Y�^@Yx�@X�`@XbN@W�@W\)@W�@V�+@Vff@VE�@U@U`B@UV@T��@T�@T��@Tj@TZ@T1@R�@Q�#@Q7L@PĜ@P�9@P��@P�u@Pr�@P �@O|�@O�@N�@N5?@L�/@L��@LZ@L(�@L1@K��@K�
@K�F@Kƨ@K��@KS�@J��@JM�@I�^@I��@Ihs@I7L@I�@HĜ@H��@Hr�@H  @G�@G�P@Gl�@G;d@F��@Fff@F$�@E�h@D�@Dz�@C�m@C�F@C"�@B-@A��@A%@@Ĝ@@�9@@��@@�u@@r�@@Q�@@ �@@  @?�w@?�P@?;d@>�+@>5?@=�@=�T@=@=��@=p�@=�@<�@<�/@<�/@<�/@<�/@<�/@<�j@<�@<Z@;ƨ@;��@;t�@;dZ@;S�@;33@:��@:~�@:~�@:^5@:=q@:=q@:J@9�^@9x�@9&�@8Ĝ@8�9@8Q�@8b@7��@7l�@7;d@7�@6��@6E�@65?@5@5�@4�D@4�@3�m@3�
@3�F@3�F@3�F@3�@3S�@333@3o@2�H@2�\@1��@1x�@1G�@1�@0��@0�9@0 �@/�w@/��@/|�@/K�@/
=@.��@.V@.5?@.@-p�@-V@,�@,I�@+�
@+��@+C�@+33@+o@*�!@*^5@*=q@*-@*J@)�@)�^@)�7@)G�@)%@(Ĝ@(r�@(A�@(b@'�@'l�@';d@'�@&�R@&ff@&5?@%��@%��@%�@%O�@%/@%V@$�@$��@$9X@#��@#dZ@#S�@#33@#"�@#@"��@"^5@"-@"-@"-@"J@"J@!�@!��@!hs@ ��@ �@ �@ bN@ bN@ Q�@ Q�@  �@�;@�@�P@�@�@ȴ@ff@{@�T@��@��@�-@��@�@O�@/@�@�j@z�@Z@9X@ƨ@ƨ@��@dZ@33@�@�\@n�@n�@M�@J@��@�#@��@��@��@��@��@��@�^@�^@��@�7@X@��@�`@r�@b@�w@�@�@��@�P@|�@|�@|�@l�@+@�@V@$�@$�@@��@�-@��@�h@p�@?}@�@�@�j@��@�D@I�@9X@��@t�@�H@��@�\@M�@=q@��@��@��@��@hs@X@X@7L@%@��@��@Ĝ@�@A�@1'@ �@ �@  @�@��@�@��@
=@�@ȴ@��@v�@V@@��@`B@�@�@z�@I�@9X@(�@�@ƨ@�@dZ@33@o@o@@
�H@
�\@
^5@
-@
J@	�#@	�^@	&�@��@Ĝ@��@r�@�@��@�P@|�@|�@\)@��@�@�R@ff@5?@5?@5?@5?@{@�T@@�@/@��@�D@Z@1@ƨ@��@�@t�@dZ@S�@33@"�@o@o@�!@n�@^5@M�@-@J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BE�BE�BD�BD�BC�BB�BF�BG�BD�BE�BD�BE�BJ�BM�BM�BL�BM�B[#B`BBdZBe`BffBffBhsBiyBiyBiyBhsBhsBgmBe`BffBx�B|�B� B�B~�Bu�B��B��B�qB�XB�!B�{B{�B�1B�+B�=B�JB�B�Br�Be`BI�Bt�B�Bl�B\)BE�B=qBG�BR�B)�B�B  B{BhB�B-B"�BVB�BŢB��B��B��B�B��B��B��B�'B�FB�3B��B��B�bBq�B]/Bk�BaHBffBL�B+B/B2-BoB
�sB
�yB
�B
�/B
�TB
�B
�}B
�!B
��B
|�B
n�B
jB
"�B
hsB
o�B
hsB
\)B
Q�B
F�B
M�B
=qB
.B
�B
JB	��B	��B	��B	��B	�dB	ŢB	�9B	�3B	�!B	�3B	�?B	�B	��B	�B	��B	��B	��B	�+B	p�B	n�B	r�B	VB	dZB	iyB	XB	XB	T�B	D�B	?}B	<jB	I�B	M�B	J�B	D�B	?}B	49B	1'B	 �B	�B	JB	
=B	�B	�B	�B	�B	oB	%B�B		7B		7B	
=B	B��B�B�BB�/B��B�}B�}B�qB�wB�3B�dB��B�^B�?B�?B�B��B��B��B��B��B�oB�=B�1B�JB�B�bB��B�{B�hB�7Bx�Bv�Bv�Bw�B{�B� B|�Bo�BiyBu�Bo�BjB_;B>wBP�Be`BdZB]/BO�B]/BYB]/BVBB�B@�BK�BG�BC�B49B�B;dBO�BP�BO�BM�BI�BF�BA�B@�BA�BG�BG�BE�B>wB33B/B(�B�B�B�BB�B��B)�B/B)�B%�B"�B$�B�B&�B.B/B-B/B33B0!B.B)�B�B�B�B�B2-B33B2-B1'B.B%�B�B�B&�B�BDBoB�BoB�B�BPB �B$�B�B0!B.B'�B�B�B"�B'�B�B!�B�B!�B�B!�B%�B/B33B1'B)�B$�B+B.B49B?}B=qB<jBB�BC�BB�BA�B<jB<jB2-B5?BB�BC�BB�B:^B33B%�B5?B+B7LBD�BB�BF�BF�BG�BE�BO�BL�BP�BT�BI�BK�B\)BgmBiyBp�Br�Bt�Bv�Bt�Bo�B~�B�B�7B�=B�DB�DB�JB�PB�DB�=B�7B�JB�=B�1B�PB��B��B��B��B��B��B��B�B�B�'B�!B�B�'B�B�!B�3B�dB�qB�}BÖB��B�?B�}BB��B�qB�}BÖBB��B��B�B�/B�/B�B�#B�)B�HB�B�B��B��B�B�B�B�B�B��B��B��B��B��B	B	B	+B	+B	VB	VB	hB	�B	�B	!�B	$�B	'�B	)�B	-B	.B	.B	-B	-B	-B	2-B	49B	8RB	7LB	49B	,B	5?B	>wB	@�B	E�B	K�B	L�B	L�B	L�B	L�B	M�B	L�B	K�B	K�B	VB	W
B	\)B	aHB	aHB	dZB	gmB	m�B	k�B	k�B	jB	k�B	l�B	k�B	l�B	o�B	r�B	q�B	u�B	t�B	u�B	|�B	}�B	|�B	}�B	{�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�+B	�7B	�7B	�7B	�7B	�+B	�7B	�=B	�=B	�JB	�=B	�JB	�JB	�DB	�bB	�{B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�3B	�3B	�LB	�^B	�^B	�dB	�dB	�dB	�^B	�dB	�dB	�jB	�wB	�jB	�qB	�wB	�qB	�wB	ĜB	ĜB	ŢB	ŢB	ŢB	ĜB	B	��B	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�#B	�)B	�#B	�B	�)B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�HB	�HB	�`B	�ZB	�NB	�ZB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B
  B	��B	��B
%B
1B

=B
DB
JB
JB
JB
PB
JB
JB
DB
DB
PB
bB
hB
bB
hB
hB
oB
uB
oB
uB
�B
�B
{B
uB
oB
uB
oB
oB
uB
{B
�B
{B
uB
�B
�B
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
�B
�B
�B
!�B
 �B
 �B
 �B
�B
!�B
"�B
#�B
#�B
#�B
"�B
"�B
!�B
 �B
 �B
#�B
#�B
$�B
$�B
#�B
#�B
%�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
)�B
(�B
'�B
(�B
+B
(�B
'�B
)�B
,B
.B
/B
0!B
0!B
0!B
/B
.B
/B
/B
.B
-B
,B
0!B
1'B
2-B
1'B
1'B
0!B
33B
49B
49B
49B
49B
49B
49B
6FB
5?B
33B
5?B
6FB
6FB
7LB
9XB
9XB
;dB
:^B
:^B
:^B
=qB
=qB
=qB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
?}B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
C�B
C�B
C�B
A�B
B�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
G�B
I�B
I�B
H�B
H�B
G�B
F�B
G�B
E�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
L�B
J�B
K�B
M�B
N�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
N�B
P�B
P�B
O�B
P�B
O�B
O�B
R�B
R�B
R�B
Q�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
R�B
R�B
Q�B
S�B
Q�B
R�B
T�B
W
B
XB
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
VB
T�B
T�B
T�B
XB
YB
XB
XB
YB
YB
ZB
YB
YB
YB
YB
YB
ZB
ZB
YB
ZB
YB
W
B
XB
\)B
]/B
\)B
]/B
]/B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
_;B
`BB
_;B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
`BB
aHB
_;B
aHB
bNB
bNB
bNB
bNB
aHB
aHB
cTB
cTB
cTB
cTB
e`B
ffB
ffB
e`B
dZB
e`B
ffB
gmB
gmB
hsB
hsB
gmB
ffB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
jB
jB
iyB
iyB
jB
m�B
m�B
m�B
l�B
k�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
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
r�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
q�B
r�B
t�B
t�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BE�BE�BD�BD�BC�BB�BF�BG�BD�BE�BD�BE�BJ�BM�BM�BMBNVB[WB`\BdZBe`BffBffBhsBiyBiyBiyBhsBh�Bg�Be�BgBx�B}"B�4B�UB}BxB��B��B�B��B��B��B}�B�RB�fB�xB�PB��B�Bt�Bg�BM6Bu�B�oBn}B^�BIRBA BI�BS�B-�B�B�B
B�B �B-)B#nBB��B�XB̈́B�jB�BٴBյB�pB��B��B��B��B��B�pB��BtB`\Bl�BbhBgBN�B.IB0�B3�B�B
�B
�=B
�oB
�;B
�@B
�7B
��B
�-B
��B
��B
qvG�O�G�O�B
hXB
o�B
h�B
]B
S@B
G�B
NVB
>�B
/�B
�B
"B	�]B	�BB	�*B	��B	��B	��B	�B	��B	�vB	�B	��B	�=B	��B	�wB	��B	�FB	�OB	�B	sB	pUB	s�B	X�B	eFB	jKB	Y�B	YB	U�B	F?B	AUB	=�B	J	B	NB	KB	EB	@ B	5?B	1�B	"4B	�B	�B	�B	CB	KB	+B	�B	B	_B��B		�B		�B	
�B	�B��B�BۦB��B��B�jB�UB��B��B�}B��B�6B��B��B��B��B�B��B�tB��B�nB��B�,B��B��B��B��B��B��B��B��B��Bz^BxBw�Bx�B|�B�iB}qBp�Bj�BvBp;BkB`\BA�BRTBe�Bd�B^BQhB]�BZB]�BV�BD�BBBL�BH�BD�B6FB B<PBO�BQ BPBN"BJ	BG+BBABAUBB'BG�BG�BE�B>�B4B0B)�BCB+B�B�B��B��B*B/OB*�B&�B#�B%�B �B'�B.}B/�B-�B/iB3MB0oB.cB*eB�B�B�B�B2-B3hB2GB1AB.IB&�B�BdB'BdB�B�BOB�BmB~BBB!HB%zB�B0;B.cB(�B�B�B#TB($B �B"�B�B"hB�B"�B&�B/�B3�B1�B*�B&B+�B/ B4�B?�B=�B<�BB�BC�BB�BA�B="B=B3hB6FBB�BC�BB�B;0B4TB(
B6FB,�B8�BEBC{BG_BGzBHfBF�BPbBM�BQhBUgBKBMB\�Bg�Bi�Bp�Br�BuBv�Bu?BpoBHB�oB�RB�=B�^B�^B�dB�jB�xB�rB��B�~B��B��B��B��B��B��B��B�BB��B�yB��B�}B�vB�UB�iB�AB��B��B��B�B��B��B��B��B�`B��B��B��B�B��B��B�-B�0B�TB�KB�IB�~BںB��B��B��B��B��B��B��B��B��B�B�'B�AB�*B�B�XB�dB�]B	MB	mB	_B	zB	�B	�B	�B	�B	�B	!�B	$�B	(
B	)�B	-B	.B	./B	-)B	-]B	-]B	2GB	4TB	88B	7�B	4�B	-)B	5�B	>�B	@�B	E�B	K�B	L�B	L�B	L�B	L�B	M�B	MB	K�B	LdB	VB	WsB	\]B	abB	a|B	dZB	gRB	m]B	k�B	k�B	j�B	k�B	l�B	k�B	l�B	o�B	r�B	q�B	u�B	uB	u�B	|�B	~B	}B	}�B	|B	�B	�'B	�3B	�3B	�%B	�?B	�B	�-B	�EB	�RB	�RB	�7B	�RB	�_B	�RB	�XB	�rB	�dB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�:B	�B	�DB	�6B	�cB	�3B	�9B	�TB	�hB	��B	�fB	�xB	�xB	�B	�B	�dB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	ĶB	ňB	żB	żB	ĶB	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�2B	�$B	�+B	�?B	�#B	�CB	�=B	�QB	�CB	�5B	�OB	�VB	�\B	�\B	�HB	�bB	�hB	�bB	�bB	�FB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�B	��B	��B
  B	��B	�B	�"B	�"B	�(B
 4B	�BB	�wB
?B
KB

XB
DB
0B
dB
dB
PB
dB
dB
xB
xB
�B
bB
�B
bB
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
!�B
 �B
 �B
 �B
�B
!�B
"�B
#�B
#�B
#�B
"�B
"�B
!�B
 �B
 �B
#�B
#�B
$�B
$�B
#�B
$B
%�B
'�B
(
B
(
B
'�B
'B
'B
%�B
&�B
'B
(
B
'B
'�B
(
B
($B
)�B
)B
($B
)B
+B
)*B
(>B
*B
,"B
./B
/ B
0!B
0!B
0B
/B
./B
/B
/5B
./B
-CB
,WB
0;B
1AB
2-B
1AB
1AB
0UB
3MB
49B
4TB
4TB
4TB
4TB
4TB
6`B
5ZB
3hB
5tB
6`B
6zB
7�B
9rB
9rB
;dB
:^B
:xB
:xB
=VB
=qB
=�B
<�B
<�B
<�B
<�B
<jB
=�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
?�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
C�B
C�B
C�B
A�B
B�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
G�B
I�B
I�B
H�B
H�B
G�B
F�B
G�B
E�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
L�B
J�B
K�B
M�B
N�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
OB
P�B
Q B
O�B
Q B
O�B
O�B
R�B
R�B
SB
RB
S�B
TB
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
SB
SB
Q�B
S�B
R B
S&B
UB
W
B
XB
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
VB
T�B
UB
U2B
X+B
X�B
XB
X+B
YB
Y1B
ZB
Y1B
YB
YB
Y1B
Y1B
ZB
ZB
Y1B
Z7B
Y1B
W?B
X+B
\CB
]B
\CB
]/B
]/B
^OB
_;B
^OB
^OB
_;B
_;B
_VB
_;B
`BB
_VB
`BB
_VB
_VB
aHB
aHB
a-B
abB
aHB
abB
`\B
abB
_pB
abB
bNB
bhB
bhB
bNB
abB
abB
cnB
cnB
cnB
c�B
e`B
ffB
ffB
ezB
dZB
ezB
f�B
gmB
gmB
hsB
hsB
gmB
f�B
h�B
h�B
h�B
hsB
hsB
g�B
i�B
j�B
j�B
i�B
i�B
j�B
m�B
mwB
m�B
l�B
k�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
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
r�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
q�B
r�B
t�B
t�B
t�B
t�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901250033572019012500335720190125003357201901250200162019012502001620190125020016201901260021292019012600212920190126002129  JA  ARFMdecpA19c                                                                20190121093700  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190121003702  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190121003706  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190121003707  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190121003707  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190121003707  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190121003707  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20190121003707  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20190121003707  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190121003707  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20190121003708  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190121003708                      G�O�G�O�G�O�                JA  ARUP                                                                        20190121005732                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190121153238  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20190122000000  CF  PSAL_ADJUSTED_QCC\  C\  G�O�                JM  ARSQJMQC2.0                                                                 20190122000000  CF  TEMP_ADJUSTED_QCC\  C\  G�O�                JM  ARCAJMQC2.0                                                                 20190124153357  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190124153357  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190124170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190125152129  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                