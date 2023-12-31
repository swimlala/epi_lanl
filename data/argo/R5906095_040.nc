CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-10T23:33:00Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �p   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ̰   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220310233300  20220310233300  5906095 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               (A   AO  7901                            2B  A   NAVIS_A                         1009                            170425                          863 @��Y1   @��"".�@&��G�{�c͙����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         (A   A   F   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�33BǙ�B�  BΙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�
>B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B��)B�B�BǨ�B�\BΨ�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG{�DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx�RDyRDy��Dz�Dz��D{�D{��D|�D|��D}�D}��D}��D~��D�D��D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�=�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�D)D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A� �A�"�A�&�A�&�A�-A�/A�/A�-A�-A�+A�+A�1'A�/A�33A�33A�;dA�E�A�E�A�VA�dZA�l�A�n�A�l�A�^5A�XA�M�A�?}A��A�A���A��HA���A�ƨAӺ^AӸRAӴ9AӴ9AӰ!AӮAӥ�Aӣ�Aӟ�AӓuA�v�A�?}A��A��A�G�A�+A���A�E�A���A��HA���A��A��yA���A�1'A���A�7LA�r�A��A��A�G�A���AƨAx�jAm�#Aj~�Ad�A_C�A]�A\{AZ��AZI�AY�PAYS�AXĜAWG�AUC�AS�AR��AR(�AQ`BAP5?AO�-AO|�AO%AM��AL�/AK��AI��AH��AE�AB�A?�A?/A?�A>��A>��A?oA?�hA?�wA?x�A>�`A>ȴA>JA=XA<bNA;��A;
=A:��A9��A8ĜA7�A6v�A6bA5A5dZA5�A4��A4�A4 �A3�#A3hsA2ȴA2ZA1��A17LA0��A05?A/��A/�;A/\)A/�A.�9A.^5A-��A-��A-hsA-;dA,�A,v�A+��A++A*��A*ĜA*bA)�A)��A)C�A(��A(ZA(�A'��A'�PA'dZA'7LA&��A&v�A& �A%�^A%�7A%hsA%C�A%;dA%
=A$��A$�yA$��A$I�A#�A#�-A#�A#XA"�yA"��A"ffA"ZA"JA!��A!��A!C�A �A �A ~�A ffA 5?A JA�A�
A��A7LAoA�A~�A�A�A�A;dA"�A�A�RA��Ar�A-A��A;dA��AZA{A�^A"�A��A�+A1'A�FA|�A&�A�jAZA(�A�mA��A`BA�A�A=qA�A�PAhsA"�A�RA{AƨA��A33Ar�A$�AJAt�A�9A�AjAJA�^Ax�A�A��Ap�AVA
��A
z�A
A�A	��A	t�A	A��A  A�PA�yA�9A�\AI�A��A`BA%A^5A�A�PA/A�A~�A��A�A ��A �\A ffA �@��@�5?@�x�@���@���@�  @�ȴ@�?}@��@�ƨ@��!@���@�@�@�?}@��@��@�|�@��@��@�`B@�j@�(�@�~�@�p�@�bN@���@��@�$�@�^@�V@�@�1@�@�ȴ@�h@�G�@���@߾w@�33@ް!@�n�@��@݁@�%@���@�9X@�l�@���@�=q@١�@ج@�b@ץ�@֟�@�-@ղ-@�x�@��/@ԓu@�z�@�(�@ӍP@�\)@�
=@ҟ�@�=q@Ѻ^@щ7@�p�@�%@���@�  @Ͼw@�S�@Η�@�~�@�V@�$�@��@�O�@�bN@ˮ@�\)@�;d@��@ʰ!@�M�@��@ɺ^@�`B@ȣ�@�bN@�ƨ@ǍP@���@��@�r�@öF@�
=@���@\@�^5@�@�`B@��@�r�@�b@��w@�l�@��@�5?@���@��@��/@��@��+@�@���@�`B@��@�V@���@�j@��F@�33@���@��#@�x�@�X@��@��
@�C�@���@���@���@���@��h@��7@�x�@�O�@��/@�Q�@��F@�@�~�@�{@��@���@�9X@��m@�\)@�~�@�J@�@���@��@�&�@��@�33@��H@�M�@���@�`B@��@��9@�(�@��m@���@�;d@�ȴ@���@���@��@��D@�1'@��;@�dZ@�"�@��R@�v�@��@�?}@��u@�Z@�(�@�1@���@���@�@���@���@�=q@�J@���@��h@�G�@��@���@�Ĝ@��u@�r�@�j@�bN@�1'@�  @�ƨ@�S�@�o@�ȴ@���@�v�@�$�@���@�G�@�Ĝ@�b@���@��F@���@��@�l�@�S�@�C�@�C�@�33@��@���@���@�~�@�V@�@��^@��7@�&�@���@�Ĝ@��D@�j@�I�@� �@���@�ƨ@�S�@��H@�~�@�ff@�5?@�{@��@�x�@���@���@��u@�z�@�Q�@��@��F@��P@�;d@�@���@���@�M�@�{@�@��-@�hs@��@��j@��D@�9X@���@�
=@�n�@�{@��7@�?}@�/@�V@���@��u@�I�@��m@���@�|�@�33@���@��H@���@�^5@���@�/@�Ĝ@�9X@�@\)@+@~�y@~�+@}�@}V@|��@|(�@{�@{"�@z��@z~�@y�7@x�9@x�u@x1'@w�@vȴ@vff@v$�@u��@u�-@u�h@u`B@u�@t��@t��@t��@t9X@s�F@s"�@r��@r-@q�^@qx�@p��@pA�@o�@o��@o|�@oK�@n��@n$�@m��@mO�@m�@l�/@lj@l1@k��@j��@j-@j�@i��@i��@ix�@iG�@i&�@h��@h��@h �@g�P@g+@f�y@f�+@f$�@ep�@e�@d��@d��@dZ@c��@c��@c33@b��@b~�@b�@a�#@ax�@aX@a%@`�9@`Q�@_��@_K�@^�@^ff@]��@]�h@]p�@]`B@]/@]�@\��@\�D@\I�@\1@[ƨ@[t�@Z�H@Z��@Z��@Z^5@ZJ@Y��@Y7L@X�`@XA�@W�@W�@V�R@V5?@U�T@U�h@U?}@U�@T��@Tj@T�@S�
@S��@St�@SS�@So@R�@R�H@R��@R�@Q�^@Q��@QG�@P�`@P��@P�9@P�@P1'@O�@N�y@N�@N�R@Nv�@M�T@M�@L�j@Lj@L�@K�m@K��@KdZ@Ko@J��@J~�@J=q@J-@JJ@I��@I&�@HĜ@H�u@H�@H�@Hr�@HA�@Hb@G�;@G�w@G�P@GK�@F��@F�+@F5?@E�@E�@E�-@EO�@E�@D��@D��@D��@D�D@D�D@Dz�@D9X@C�m@Ct�@CS�@Co@B��@B�!@B�!@B�\@B^5@B=q@B-@B�@A�#@A��@A�7@A7L@@��@@�9@@�u@@bN@@ �@?�;@?�w@?\)@>�y@>�@>�R@>5?@=�h@=�@<�j@<z�@<I�@<1@;ƨ@;�@;C�@:�@:��@:�\@9��@9hs@8Ĝ@8 �@7�;@7�P@7+@6ȴ@6��@6�+@6�+@6V@6E�@6$�@5��@5`B@4�@4��@4j@3��@3�F@3��@3t�@3C�@3"�@3@2�@2��@2��@2�\@2n�@2-@1�#@1G�@0�`@0��@0�@0r�@0bN@0b@/��@/+@.��@.�R@.E�@-�-@-?}@,�@,��@,9X@+�
@+t�@+33@*��@*^5@)X@(��@(Ĝ@(r�@(1'@(  @'�@'�P@'\)@&�y@&��@&E�@&$�@%��@%?}@$�@$�/@$��@$�D@$I�@#�m@#t�@#"�@#@"��@"��@"~�@!�@!��@!hs@!7L@ Ĝ@ �u@ Q�@�@�@��@\)@;d@�@�@�+@{@��@/@�@Z@��@ƨ@�F@dZ@33@@��@��@�@�@��@�^@�^@��@hs@&�@�`@��@�@bN@bN@A�@��@\)@�@�y@��@v�@E�@@�T@�-@�@?}@�@�j@��@z�@Z@I�@I�@(�@1@1@��@ƨ@�F@��@S�@"�@o@o@o@�H@�!@��@~�@M�@J@�#@��@��@X@7L@��@��@Ĝ@��@Q�@b@�;@�w@��@�P@K�@��@ȴ@�R@��@��@�+@v�@V@E�@5?@5?@{@@@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�"�A� �A�"�A�&�A�&�A�-A�/A�/A�-A�-A�+A�+A�1'A�/A�33A�33A�;dA�E�A�E�A�VA�dZA�l�A�n�A�l�A�^5A�XA�M�A�?}A��A�A���A��HA���A�ƨAӺ^AӸRAӴ9AӴ9AӰ!AӮAӥ�Aӣ�Aӟ�AӓuA�v�A�?}A��A��A�G�A�+A���A�E�A���A��HA���A��A��yA���A�1'A���A�7LA�r�A��A��A�G�A���AƨAx�jAm�#Aj~�Ad�A_C�A]�A\{AZ��AZI�AY�PAYS�AXĜAWG�AUC�AS�AR��AR(�AQ`BAP5?AO�-AO|�AO%AM��AL�/AK��AI��AH��AE�AB�A?�A?/A?�A>��A>��A?oA?�hA?�wA?x�A>�`A>ȴA>JA=XA<bNA;��A;
=A:��A9��A8ĜA7�A6v�A6bA5A5dZA5�A4��A4�A4 �A3�#A3hsA2ȴA2ZA1��A17LA0��A05?A/��A/�;A/\)A/�A.�9A.^5A-��A-��A-hsA-;dA,�A,v�A+��A++A*��A*ĜA*bA)�A)��A)C�A(��A(ZA(�A'��A'�PA'dZA'7LA&��A&v�A& �A%�^A%�7A%hsA%C�A%;dA%
=A$��A$�yA$��A$I�A#�A#�-A#�A#XA"�yA"��A"ffA"ZA"JA!��A!��A!C�A �A �A ~�A ffA 5?A JA�A�
A��A7LAoA�A~�A�A�A�A;dA"�A�A�RA��Ar�A-A��A;dA��AZA{A�^A"�A��A�+A1'A�FA|�A&�A�jAZA(�A�mA��A`BA�A�A=qA�A�PAhsA"�A�RA{AƨA��A33Ar�A$�AJAt�A�9A�AjAJA�^Ax�A�A��Ap�AVA
��A
z�A
A�A	��A	t�A	A��A  A�PA�yA�9A�\AI�A��A`BA%A^5A�A�PA/A�A~�A��A�A ��A �\A ffA �@��@�5?@�x�@���@���@�  @�ȴ@�?}@��@�ƨ@��!@���@�@�@�?}@��@��@�|�@��@��@�`B@�j@�(�@�~�@�p�@�bN@���@��@�$�@�^@�V@�@�1@�@�ȴ@�h@�G�@���@߾w@�33@ް!@�n�@��@݁@�%@���@�9X@�l�@���@�=q@١�@ج@�b@ץ�@֟�@�-@ղ-@�x�@��/@ԓu@�z�@�(�@ӍP@�\)@�
=@ҟ�@�=q@Ѻ^@щ7@�p�@�%@���@�  @Ͼw@�S�@Η�@�~�@�V@�$�@��@�O�@�bN@ˮ@�\)@�;d@��@ʰ!@�M�@��@ɺ^@�`B@ȣ�@�bN@�ƨ@ǍP@���@��@�r�@öF@�
=@���@\@�^5@�@�`B@��@�r�@�b@��w@�l�@��@�5?@���@��@��/@��@��+@�@���@�`B@��@�V@���@�j@��F@�33@���@��#@�x�@�X@��@��
@�C�@���@���@���@���@��h@��7@�x�@�O�@��/@�Q�@��F@�@�~�@�{@��@���@�9X@��m@�\)@�~�@�J@�@���@��@�&�@��@�33@��H@�M�@���@�`B@��@��9@�(�@��m@���@�;d@�ȴ@���@���@��@��D@�1'@��;@�dZ@�"�@��R@�v�@��@�?}@��u@�Z@�(�@�1@���@���@�@���@���@�=q@�J@���@��h@�G�@��@���@�Ĝ@��u@�r�@�j@�bN@�1'@�  @�ƨ@�S�@�o@�ȴ@���@�v�@�$�@���@�G�@�Ĝ@�b@���@��F@���@��@�l�@�S�@�C�@�C�@�33@��@���@���@�~�@�V@�@��^@��7@�&�@���@�Ĝ@��D@�j@�I�@� �@���@�ƨ@�S�@��H@�~�@�ff@�5?@�{@��@�x�@���@���@��u@�z�@�Q�@��@��F@��P@�;d@�@���@���@�M�@�{@�@��-@�hs@��@��j@��D@�9X@���@�
=@�n�@�{@��7@�?}@�/@�V@���@��u@�I�@��m@���@�|�@�33@���@��H@���@�^5@���@�/@�Ĝ@�9X@�@\)@+@~�y@~�+@}�@}V@|��@|(�@{�@{"�@z��@z~�@y�7@x�9@x�u@x1'@w�@vȴ@vff@v$�@u��@u�-@u�h@u`B@u�@t��@t��@t��@t9X@s�F@s"�@r��@r-@q�^@qx�@p��@pA�@o�@o��@o|�@oK�@n��@n$�@m��@mO�@m�@l�/@lj@l1@k��@j��@j-@j�@i��@i��@ix�@iG�@i&�@h��@h��@h �@g�P@g+@f�y@f�+@f$�@ep�@e�@d��@d��@dZ@c��@c��@c33@b��@b~�@b�@a�#@ax�@aX@a%@`�9@`Q�@_��@_K�@^�@^ff@]��@]�h@]p�@]`B@]/@]�@\��@\�D@\I�@\1@[ƨ@[t�@Z�H@Z��@Z��@Z^5@ZJ@Y��@Y7L@X�`@XA�@W�@W�@V�R@V5?@U�T@U�h@U?}@U�@T��@Tj@T�@S�
@S��@St�@SS�@So@R�@R�H@R��@R�@Q�^@Q��@QG�@P�`@P��@P�9@P�@P1'@O�@N�y@N�@N�R@Nv�@M�T@M�@L�j@Lj@L�@K�m@K��@KdZ@Ko@J��@J~�@J=q@J-@JJ@I��@I&�@HĜ@H�u@H�@H�@Hr�@HA�@Hb@G�;@G�w@G�P@GK�@F��@F�+@F5?@E�@E�@E�-@EO�@E�@D��@D��@D��@D�D@D�D@Dz�@D9X@C�m@Ct�@CS�@Co@B��@B�!@B�!@B�\@B^5@B=q@B-@B�@A�#@A��@A�7@A7L@@��@@�9@@�u@@bN@@ �@?�;@?�w@?\)@>�y@>�@>�R@>5?@=�h@=�@<�j@<z�@<I�@<1@;ƨ@;�@;C�@:�@:��@:�\@9��@9hs@8Ĝ@8 �@7�;@7�P@7+@6ȴ@6��@6�+@6�+@6V@6E�@6$�@5��@5`B@4�@4��@4j@3��@3�F@3��@3t�@3C�@3"�@3@2�@2��@2��@2�\@2n�@2-@1�#@1G�@0�`@0��@0�@0r�@0bN@0b@/��@/+@.��@.�R@.E�@-�-@-?}@,�@,��@,9X@+�
@+t�@+33@*��@*^5@)X@(��@(Ĝ@(r�@(1'@(  @'�@'�P@'\)@&�y@&��@&E�@&$�@%��@%?}@$�@$�/@$��@$�D@$I�@#�m@#t�@#"�@#@"��@"��@"~�@!�@!��@!hs@!7L@ Ĝ@ �u@ Q�@�@�@��@\)@;d@�@�@�+@{@��@/@�@Z@��@ƨ@�F@dZ@33@@��@��@�@�@��@�^@�^@��@hs@&�@�`@��@�@bN@bN@A�@��@\)@�@�y@��@v�@E�@@�T@�-@�@?}@�@�j@��@z�@Z@I�@I�@(�@1@1@��@ƨ@�F@��@S�@"�@o@o@o@�H@�!@��@~�@M�@J@�#@��@��@X@7L@��@��@Ĝ@��@Q�@b@�;@�w@��@�P@K�@��@ȴ@�R@��@��@�+@v�@V@E�@5?@5?@{@@@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	K�B	K�B	K�B	K�B	L�B	L�B	L�B	K�B	K�B	K�B	K�B	K�B	L�B	L�B	L�B	L�B	Q�B	W
B	T�B	aHB	o�B	w�B	|�B	�%B	�=B	�=B	�7B	�1B	�B	�B	� B	}�B	{�B	z�B	y�B	z�B	z�B	z�B	z�B	z�B	y�B	y�B	y�B	z�B	�B	�%B	�%B	�1B	�PB	��B
.B
�BB
�ZBhBD�B\B
�B
��B
�{B
�B
�=B
��B
�%B
iyB
Q�B
-B
 �B
hB	��B	��B
�B
0!B
J�B
T�B
cTB
m�B
z�B
|�B
�B
�oB
��B
�FB
�^B
�wB
B
ɺB
��B
��B
�B
�B
��B
��B
�B
��B
�sB
��B
ÖB
ȴB
��B
�B
�BB
��B{B/B33B0!BE�BM�BN�BK�BN�BN�BM�BM�BN�BO�BM�BM�BL�BK�BJ�BI�BI�BI�BG�BI�BH�BG�BG�BD�BD�BC�BA�BB�BA�BA�B?}B?}B?}B>wB>wB=qB<jB:^B8RB6FB6FB5?B5?B33B33B1'B1'B0!B/B.B.B-B-B,B+B)�B'�B'�B&�B&�B%�B%�B%�B%�B$�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBuBhBhBbB\BPBJBDB
=B
=B	7B1B1B+B%BBBB  B
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
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�fB
�`B
�ZB
�TB
�BB
�5B
�/B
�)B
�B
�
B
�B
�B
��B
��B
��B
��B
��B
��B
ȴB
ƨB
ÖB
ÖB
B
��B
��B
�}B
�jB
�^B
�XB
�FB
�?B
�-B
�'B
�!B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�oB
�oB
�bB
�\B
�DB
�=B
�1B
�+B
�%B
�B
�B
�B
�B
�B
� B
~�B
|�B
{�B
z�B
z�B
y�B
x�B
x�B
w�B
w�B
v�B
v�B
v�B
u�B
u�B
t�B
t�B
t�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
r�B
s�B
s�B
t�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
q�B
q�B
q�B
p�B
p�B
o�B
o�B
o�B
p�B
q�B
p�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
o�B
n�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
l�B
m�B
k�B
l�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
k�B
l�B
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
m�B
m�B
m�B
n�B
n�B
n�B
m�B
l�B
m�B
m�B
m�B
m�B
o�B
p�B
p�B
p�B
p�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
s�B
s�B
t�B
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
w�B
x�B
y�B
y�B
x�B
x�B
w�B
w�B
w�B
w�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
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
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�9B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�}B
�}B
��B
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
B
B
B
B
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B	*$B	*$B	*$B	*$B	+*B	+*B	+*B	*$B	*$B	*$B	*$B	*$B	+*B	+*B	+*B	+*B	0IB	5gB	3[B	?�B	M�B	V,B	[KB	d�B	h�B	h�B	g�B	f�B	bvB	_cB	^]B	\QB	ZDB	Y>B	X8B	Y>B	Y>B	Y>B	Y>B	Y>B	X8B	X8B	X8B	Y>B	_cB	d�B	d�B	f�B	k�B	�2B
qB
��B
·B
��B"�B
��B
�zB
��B
r�B
aoB
h�B
s�B
d�B
G�B
0IB
kB	�"B	��B	�8B	�,B	��B
~B
)B
3[B
A�B
K�B
Y>B
[KB
aoB
p�B
�.B
��B
��B
��B
��B
�B
�$B
�<B
�tB
��B
�B
�2B
��B
�DB
��B
�UB
��B
�B
�OB
�mB
��B
�>B
��BxB�B~B#�B,0B-6B*$B-6B-6B,0B,0B-6B.<B,0B,0B+*B*$B)B(B(B(B&B(B'B&B&B"�B"�B!�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~BxBqBqBkBkB
eB	_BYBMBMBFBFB@B@B@B@B:B4B.B (B
�"B
�B
�B
�B
�	B
�B
�B
��B
��B
��B
��B
��B
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
��B
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
�|B
�oB
�cB
�]B
�QB
�KB
�>B
�8B
�,B
�,B
� B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
ýB
·B
��B
��B
��B
��B
��B
�tB
�gB
�aB
�aB
�IB
�<B
�<B
�6B
�*B
�$B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�~B
�xB
�xB
�qB
�kB
�_B
�SB
�FB
�FB
�@B
�:B
�4B
�.B
"B
~B
}B
}B
{	B
zB
x�B
w�B
v�B
v�B
t�B
r�B
p�B
p�B
n�B
m�B
i�B
h�B
f�B
e�B
d�B
c|B
bvB
`iB
`iB
_cB
^]B
]WB
[KB
ZDB
Y>B
Y>B
X8B
W2B
W2B
V,B
V,B
U&B
U&B
U&B
T B
T B
SB
SB
SB
RB
RB
RB
RB
RB
SB
SB
RB
RB
QB
QB
RB
RB
SB
RB
RB
RB
RB
RB
QB
QB
QB
PB
PB
PB
OB
OB
M�B
M�B
M�B
OB
PB
OB
PB
PB
OB
OB
OB
OB
OB
OB
OB
M�B
M�B
M�B
M�B
L�B
K�B
K�B
L�B
L�B
K�B
K�B
K�B
J�B
K�B
I�B
J�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
I�B
I�B
I�B
J�B
I�B
J�B
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
K�B
K�B
L�B
L�B
L�B
K�B
J�B
K�B
K�B
K�B
K�B
M�B
OB
OB
OB
OB
QB
QB
QB
RB
RB
RB
RB
SB
RB
RB
SB
SB
SB
SB
SB
SB
SB
T B
T B
T B
T B
U&B
U&B
U&B
V,B
V,B
V,B
V,B
V,B
V,B
W2B
V,B
W2B
X8B
X8B
W2B
W2B
V,B
V,B
V,B
V,B
X8B
X8B
X8B
X8B
X8B
Y>B
Y>B
ZDB
ZDB
[KB
[KB
[KB
\QB
\QB
\QB
\QB
\QB
]WB
]WB
]WB
]WB
\QB
]WB
]WB
]WB
^]B
^]B
^]B
^]B
^]B
_cB
_cB
_cB
_cB
`iB
`iB
`iB
`iB
`iB
`iB
`iB
`iB
`iB
`iB
aoB
aoB
aoB
aoB
aoB
bvB
bvB
bvB
aoB
aoB
bvB
bvB
bvB
c|B
c|B
c|B
c|B
c|B
d�B
c|B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
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
m�B
m�B
m�B
m�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
s�B
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
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
zB
zB
zB
zB
zB
zB
{	B
{	B
|B
|B
|B
|B
|B
}B
}B
}B
}B
}B
}B
~B
~B
~B
~B
"B
"B
"B
"B
�(B
�(B
�(B
�(B
�(B
�(B
�(B
�.B
�.B
�.B
�.B
�.B
�4B
�4B
�4B
�4B
�:B
�:B
�:B
�:B
�:B
�:B
�:B
�@B
�:B
�@B
�@B
�@B
�@B
�@B
�@B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�MB
�MB
�MB
�SB
�SB
�SB
�SB
�SB
�YB
�YB
�YB
�YB
�YB
�YB
�_B
�_B
�_B
�_B
�_B
�_B
�_B
�_B
�eB
�_B
�eB
�eB
�eB
�eB
�eB
�eB
�kB
�kB
�kB
�kB
�kB
�qB
�qB
�xB
�xB
�xB
�xB
�xB
�xB
�xB
�~B
�~B
�~B
�~B
�~B
�~B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B
�$B
�$B
�$B
�*B
�*B
�*B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�6B
�6B
�6B
�6B
�6B
�6B
�6B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�<B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�IB
�IB
�IB
�IB
�IB
�IB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�OB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�aB
�aB
�aB
�aB
�aB
�gB
�gB
�gB
�gB
�gB
�gB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�tB
�tB
�tB
�tB
�t33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0328498                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 26 02 2020 030 -0.0328498 0.0004 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20220310233300              20220310233300  AO  ARCAADJP                                                                    20220310233300    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20220310233300    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220310233300  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220310233300  QCF$                G�O�G�O�G�O�8000            