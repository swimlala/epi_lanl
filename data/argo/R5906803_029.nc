CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-03-15T14:02:27Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230315140227  20230315140227  5906803 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  9276                            2B  A   NAVIS_A                         1437                            170425                          863 @�!N�61   @�!��`P@;B3����dH�Fs��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��DgD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da�3Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp3Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD�əD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�	�D�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�3D�FfDfD��fD�fD�FfDÆfD��fD�fD�FfDĆfD��fD�fD�FfDņfD��fD�fD�FfDƆfD��fD�fD�FfDǆfD��fD�fD�FfDȆfD��fD�fD�FfDɆfD��fD�fD�FfDʆfD��fD�fD�FfDˆfD��fD�fD�FfD̆fD��fD�fD�FfD͆fD��fD�fD�FfDΆfD��fD�fD�FfDφfD��fD�fD�FfDІfD��fD�fD�FfDцfD��fD�fD�FfD҆fD��fD�fD�FfDӆfD��fD�fD�FfDԆfD��fD�fD�FfDՆfD��fD�fD�FfDֆfD��fD�fD�FfD׆fD��fD�fD�FfD؆fD��fD�fD�FfDنfD��fD�fD�FfDچfD��fD�fD�FfDۆfD��fD�fD�FfD܆fD��fD�3D�FfD݆fD��fD�fD�FfDކfD��fD�fD�FfD߆fD��fD�fD�FfD��fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD�əD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD��fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD���D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�
=A�-A�t�A�t�A�hsA�\)A�ZA�ZA�XA�VA�XA�VA�S�A�Q�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�XA�XA�XA�ZA�ZA�\)A�\)A�XA�XA�VA�Q�A�E�A�33A�-A�+A�&�A��A���A�Q�A�A���A�`BA��A�{A��/A�bNA���A�K�A��A�M�A�S�A���A�M�A��HA��A�C�A�  A��FA�ffA��A�bNA���A��A�\)A��A�v�A�
=A��9A���A��A�v�A�JA�XA�hsA�S�A�ZA�t�A�C�A�VA���A�O�A��A�A�bNA��+A���A�1'A�jA��A��yA���A��A���A��A��+A�%A��A�ĜA��#A���A��A�{A��A|�AyG�AyAx~�AwAu�#AsVAr�uArQ�AqƨAp�/Ao��An�HAmAi��AhI�Ag�;Ag�Af1'AcA`��A]
=A[�AYƨAX��AXjAW��AV��AU��AU�FAT��AQ�FAOƨAM��AL=qAK
=AJz�AI33AG\)AE�-AC��ABĜAA��A@�A?�wA>��A>jA>E�A>  A=XA<�yA<��A<z�A<bNA;`BA:ffA9��A9S�A9?}A9"�A8�A7hsA7oA6�A6��A6�9A6�DA6bNA6�A5�mA4��A333A2  A1p�A0�jA0$�A/�A-��A-oA,z�A+/A)�PA(��A'�wA&��A&�uA&  A%�7A#��A#33A"�+A!VA�^AoA�A�mA�AK�AȴA�uA�A~�A��AhsA��AVA�hA�A��AA�A|�AQ�A|�A��A~�A�hA/A�`A1'A�AA$�AJA�mAG�A
(�A~�A�A�AVA��AM�A1'A��A��AoA ��A 5?A 1@��@���@��-@�x�@�O�@��9@��\@���@��+@�@�l�@�+@�;d@��@���@�E�@�?}@���@�u@�w@�7@�"�@�ȴ@�ff@���@�@�J@�u@�-@��@� �@��@ߥ�@ߕ�@�dZ@�~�@��@ݩ�@��@�(�@�=q@��#@ՙ�@Չ7@�?}@�b@�v�@�hs@���@���@��@ɡ�@ȃ@�|�@��@ě�@��@��m@���@��/@�K�@��@�^5@���@���@��@�G�@�j@���@�|�@��H@��@�p�@�r�@�K�@�^5@��@��-@���@��/@�1'@���@��j@�dZ@�v�@��T@��7@�7L@�r�@� �@��@��@�v�@�{@�`B@�&�@�9X@��@�33@�M�@�/@�%@���@�I�@���@�+@�ff@��-@��@��@�Z@���@�+@���@��\@�E�@��^@�&�@���@���@��;@�|�@�"�@��@���@���@�E�@�5?@�5?@��@�@��^@�hs@�V@���@��j@�I�@�1@��
@��@���@���@�dZ@�"�@�@��H@��H@���@�$�@���@���@���@��@�r�@�j@�I�@�9X@�1@��@���@���@�\)@�C�@���@��H@�5?@���@���@���@��h@���@�r�@�bN@�Z@�9X@��w@�t�@�l�@�dZ@�dZ@�\)@�\)@�dZ@�\)@�C�@�;d@��y@�V@��T@�O�@���@�9X@���@��u@�I�@�(�@���@���@���@���@���@���@���@���@���@�t�@��@�ȴ@��\@��T@��-@���@�`B@��@��9@�j@�Z@�1'@�1@�  @��F@��P@�S�@�~�@��-@�%@��j@�j@�Q�@�1'@�1@�@��@K�@+@}��@}?}@}?}@}�@|�/@|�/@|��@|��@|��@{ƨ@{33@z�@z��@z��@zn�@y�7@x��@w��@w��@w|�@wK�@v�y@v��@v��@v�+@vE�@u�@t(�@s�F@s"�@q��@q�7@q7L@q�@p��@pĜ@pĜ@p�9@p�9@o|�@n@m��@m��@m`B@l�@l�D@m�@m`B@l��@l9X@k�
@k�@ko@j�!@j~�@j~�@j~�@j=q@j�@i��@i&�@h��@h �@fȴ@f�R@f�R@fv�@e��@eV@dI�@d(�@d�@cdZ@b�@b��@b�!@b��@b�!@b�!@b�!@b��@bn�@a��@a�@`��@`�@`1'@` �@`b@_��@_K�@^ȴ@^v�@^V@]��@\��@\��@\(�@[��@[t�@["�@[o@Z�H@Z^5@Y��@Y�^@Y%@XQ�@X �@Xb@W�;@W|�@W|�@W\)@W�@Vȴ@V��@Vv�@V{@V@U��@U@U��@U�@Up�@Up�@UO�@U?}@U/@T�/@TZ@S�m@St�@Q��@PQ�@Pb@P  @O��@O�w@O�@O�@O��@O��@O�P@O|�@Ol�@O;d@N�@Nv�@NE�@M�@M@M��@L�@Lz�@LZ@LI�@L(�@K�m@K��@KC�@Ko@K@J��@J�\@J-@I�7@H�`@HQ�@HQ�@HA�@H �@GK�@F�y@Fȴ@F�R@E�@Ep�@EO�@EV@D��@C��@Cƨ@C@B-@A��@A��@Ahs@A�@@��@@r�@@ �@@b@?��@?K�@>��@>v�@>E�@>@=�T@=@=�@<�@<�/@<�j@<�D@<j@<Z@;ƨ@;��@;C�@:=q@9��@9��@9X@9�@8��@8r�@81'@8  @7�@7�P@7�@6ȴ@6�R@6��@65?@5�-@5O�@5V@4�/@4�D@4�@3ƨ@3��@3t�@3@2�!@2��@2�\@2M�@2J@1��@1X@0��@0��@0��@0Ĝ@0�@0bN@0 �@/��@/l�@/;d@/
=@.��@.��@.{@-@-�h@-`B@-�@,�@,1@+�@+dZ@+"�@*�@*�H@*�H@*��@*��@*=q@*J@*J@)��@)��@*J@*�@*�@*�@*J@)��@)�#@)��@)hs@)%@)%@(��@(�9@(�@(A�@( �@(  @'�;@'��@'�w@'|�@'+@'
=@&��@&�R@&�+@&v�@&ff@&ff@&V@&5?@%�@%�@$�@$j@#�
@#�@#"�@#o@"�@"��@"��@"��@"��@"^5@"�@!�#@!x�@!7L@ �`@ bN@�w@�@�@��@�P@K�@�@��@��@E�@{@@�@�-@�@?}@V@�@�/@��@�j@�@��@��@�@�@��@��@9X@��@�m@ƨ@��@dZ@C�@"�@��@�!@~�@��@��@��@&�@��@�9@�u@�u@�@r�@bN@bN@Q�@A�@1'@�@��@�w@|�@
=@ȴ@�R@��@��@�+@V@@�@�T@��@@�@V@��@�D@�D@z�@Z@9X@�@�m@��@�@��@��@C�@"�@"�@"�@@��@��@��@hs@7L@%@�@Q�@A�@A�@A�@1'@1'@1'@ �@ �@ �@b@�@�w@|�@\)@;d@�@��@��@��@E�@5?@@�T@�-@O�@��@Z@�@ƨ@��@33@o@
�!@
n�@
^5@
^5@
=q@	��@	��@	��@	G�@	�@��@�9@�@r�@r�@bN@Q�@1'@ �@�@�;@�;@��@�@�P@K�@+@
=@�@ȴ@�R@�+@V@E�@$�@�T@@�h@O�@?}@V@�@�j@��@I�@�@��@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�
=A�-A�t�A�t�A�hsA�\)A�ZA�ZA�XA�VA�XA�VA�S�A�Q�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�XA�XA�XA�ZA�ZA�\)A�\)A�XA�XA�VA�Q�A�E�A�33A�-A�+A�&�A��A���A�Q�A�A���A�`BA��A�{A��/A�bNA���A�K�A��A�M�A�S�A���A�M�A��HA��A�C�A�  A��FA�ffA��A�bNA���A��A�\)A��A�v�A�
=A��9A���A��A�v�A�JA�XA�hsA�S�A�ZA�t�A�C�A�VA���A�O�A��A�A�bNA��+A���A�1'A�jA��A��yA���A��A���A��A��+A�%A��A�ĜA��#A���A��A�{A��A|�AyG�AyAx~�AwAu�#AsVAr�uArQ�AqƨAp�/Ao��An�HAmAi��AhI�Ag�;Ag�Af1'AcA`��A]
=A[�AYƨAX��AXjAW��AV��AU��AU�FAT��AQ�FAOƨAM��AL=qAK
=AJz�AI33AG\)AE�-AC��ABĜAA��A@�A?�wA>��A>jA>E�A>  A=XA<�yA<��A<z�A<bNA;`BA:ffA9��A9S�A9?}A9"�A8�A7hsA7oA6�A6��A6�9A6�DA6bNA6�A5�mA4��A333A2  A1p�A0�jA0$�A/�A-��A-oA,z�A+/A)�PA(��A'�wA&��A&�uA&  A%�7A#��A#33A"�+A!VA�^AoA�A�mA�AK�AȴA�uA�A~�A��AhsA��AVA�hA�A��AA�A|�AQ�A|�A��A~�A�hA/A�`A1'A�AA$�AJA�mAG�A
(�A~�A�A�AVA��AM�A1'A��A��AoA ��A 5?A 1@��@���@��-@�x�@�O�@��9@��\@���@��+@�@�l�@�+@�;d@��@���@�E�@�?}@���@�u@�w@�7@�"�@�ȴ@�ff@���@�@�J@�u@�-@��@� �@��@ߥ�@ߕ�@�dZ@�~�@��@ݩ�@��@�(�@�=q@��#@ՙ�@Չ7@�?}@�b@�v�@�hs@���@���@��@ɡ�@ȃ@�|�@��@ě�@��@��m@���@��/@�K�@��@�^5@���@���@��@�G�@�j@���@�|�@��H@��@�p�@�r�@�K�@�^5@��@��-@���@��/@�1'@���@��j@�dZ@�v�@��T@��7@�7L@�r�@� �@��@��@�v�@�{@�`B@�&�@�9X@��@�33@�M�@�/@�%@���@�I�@���@�+@�ff@��-@��@��@�Z@���@�+@���@��\@�E�@��^@�&�@���@���@��;@�|�@�"�@��@���@���@�E�@�5?@�5?@��@�@��^@�hs@�V@���@��j@�I�@�1@��
@��@���@���@�dZ@�"�@�@��H@��H@���@�$�@���@���@���@��@�r�@�j@�I�@�9X@�1@��@���@���@�\)@�C�@���@��H@�5?@���@���@���@��h@���@�r�@�bN@�Z@�9X@��w@�t�@�l�@�dZ@�dZ@�\)@�\)@�dZ@�\)@�C�@�;d@��y@�V@��T@�O�@���@�9X@���@��u@�I�@�(�@���@���@���@���@���@���@���@���@���@�t�@��@�ȴ@��\@��T@��-@���@�`B@��@��9@�j@�Z@�1'@�1@�  @��F@��P@�S�@�~�@��-@�%@��j@�j@�Q�@�1'@�1@�@��@K�@+@}��@}?}@}?}@}�@|�/@|�/@|��@|��@|��@{ƨ@{33@z�@z��@z��@zn�@y�7@x��@w��@w��@w|�@wK�@v�y@v��@v��@v�+@vE�@u�@t(�@s�F@s"�@q��@q�7@q7L@q�@p��@pĜ@pĜ@p�9@p�9@o|�@n@m��@m��@m`B@l�@l�D@m�@m`B@l��@l9X@k�
@k�@ko@j�!@j~�@j~�@j~�@j=q@j�@i��@i&�@h��@h �@fȴ@f�R@f�R@fv�@e��@eV@dI�@d(�@d�@cdZ@b�@b��@b�!@b��@b�!@b�!@b�!@b��@bn�@a��@a�@`��@`�@`1'@` �@`b@_��@_K�@^ȴ@^v�@^V@]��@\��@\��@\(�@[��@[t�@["�@[o@Z�H@Z^5@Y��@Y�^@Y%@XQ�@X �@Xb@W�;@W|�@W|�@W\)@W�@Vȴ@V��@Vv�@V{@V@U��@U@U��@U�@Up�@Up�@UO�@U?}@U/@T�/@TZ@S�m@St�@Q��@PQ�@Pb@P  @O��@O�w@O�@O�@O��@O��@O�P@O|�@Ol�@O;d@N�@Nv�@NE�@M�@M@M��@L�@Lz�@LZ@LI�@L(�@K�m@K��@KC�@Ko@K@J��@J�\@J-@I�7@H�`@HQ�@HQ�@HA�@H �@GK�@F�y@Fȴ@F�R@E�@Ep�@EO�@EV@D��@C��@Cƨ@C@B-@A��@A��@Ahs@A�@@��@@r�@@ �@@b@?��@?K�@>��@>v�@>E�@>@=�T@=@=�@<�@<�/@<�j@<�D@<j@<Z@;ƨ@;��@;C�@:=q@9��@9��@9X@9�@8��@8r�@81'@8  @7�@7�P@7�@6ȴ@6�R@6��@65?@5�-@5O�@5V@4�/@4�D@4�@3ƨ@3��@3t�@3@2�!@2��@2�\@2M�@2J@1��@1X@0��@0��@0��@0Ĝ@0�@0bN@0 �@/��@/l�@/;d@/
=@.��@.��@.{@-@-�h@-`B@-�@,�@,1@+�@+dZ@+"�@*�@*�H@*�H@*��@*��@*=q@*J@*J@)��@)��@*J@*�@*�@*�@*J@)��@)�#@)��@)hs@)%@)%@(��@(�9@(�@(A�@( �@(  @'�;@'��@'�w@'|�@'+@'
=@&��@&�R@&�+@&v�@&ff@&ff@&V@&5?@%�@%�@$�@$j@#�
@#�@#"�@#o@"�@"��@"��@"��@"��@"^5@"�@!�#@!x�@!7L@ �`@ bN@�w@�@�@��@�P@K�@�@��@��@E�@{@@�@�-@�@?}@V@�@�/@��@�j@�@��@��@�@�@��@��@9X@��@�m@ƨ@��@dZ@C�@"�@��@�!@~�@��@��@��@&�@��@�9@�u@�u@�@r�@bN@bN@Q�@A�@1'@�@��@�w@|�@
=@ȴ@�R@��@��@�+@V@@�@�T@��@@�@V@��@�D@�D@z�@Z@9X@�@�m@��@�@��@��@C�@"�@"�@"�@@��@��@��@hs@7L@%@�@Q�@A�@A�@A�@1'@1'@1'@ �@ �@ �@b@�@�w@|�@\)@;d@�@��@��@��@E�@5?@@�T@�-@O�@��@Z@�@ƨ@��@33@o@
�!@
n�@
^5@
^5@
=q@	��@	��@	��@	G�@	�@��@�9@�@r�@r�@bN@Q�@1'@ �@�@�;@�;@��@�@�P@K�@+@
=@�@ȴ@�R@�+@V@E�@$�@�T@@�h@O�@?}@V@�@�j@��@I�@�@��@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBDB�B6FB;dB<jB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB<jB<jB<jB<jB<jB<jB<jB<jB=qB=qB=qB=qB=qB=qBA�BA�B@�B?}B?}B?}B?}B:^B0!B)�B�B	7B��B�B�B�fB��B��B�}B��B�7B}�Br�B^5BL�B@�B9XB%�BJBB�mB�#B�B�B��B��B��B��BǮBŢB�-B��Bk�BW
BS�BI�B:^B7LB5?B/B+B$�B!�B�BhB1B
��B
�B
�B
�;B
��B
B
�9B
�!B
�B
��B
��B
�=B
|�B
w�B
p�B
bNB
K�B
9XB
�B
�B
�B
uB
JB	��B	��B	�B	�B	�B	�`B	�/B	��B	��B	�FB	�3B	�B	��B	��B	�B	p�B	e`B	_;B	YB	W
B	R�B	N�B	I�B	G�B	A�B	6FB	,B	$�B	�B	�B	oB	PB	B��B��B�B�B�mB�ZB�HB�5B�/B�)B�B�B�B��B��B��B��BɺBȴBǮBƨBŢB��B�}B�wB�qB�qB�jB�jB�dB�XB�LB�!B�B��B��B��B��B��B��B�{B�bB�DB�+B�B�B� B}�Bz�Bw�Bs�Bq�Bl�BhsBe`BcTB_;B^5B]/B[#BYBXBT�BQ�BP�BO�BM�BL�BL�BL�BK�BJ�BH�BE�BD�BC�BC�BA�B@�B@�B=qB>wB<jB;dB:^B:^B9XB7LB8RB7LB5?B5?B49B33B1'B33B2-B1'B0!B0!B0!B0!B0!B0!B/B/B0!B,B+B(�B'�B%�B%�B%�B%�B%�B$�B#�B#�B#�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B%�B&�B&�B'�B'�B(�B'�B'�B(�B(�B,B2-B49B9XB9XB:^B;dB>wBA�BD�BE�BH�BI�BK�BL�BN�BW
BZB[#BaHBffBjBo�Bs�Bt�Bu�Bx�By�By�B{�B�B�B�+B�1B�PB�\B�bB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�'B�-B�3B�?B�XB�^B�dB�dB�dB�jB�jB�qB�wB�}B��BBÖBĜBĜBĜBƨBǮBȴB��B��B��B��B��B��B�B�
B�B�BB�`B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	1B		7B		7B		7B	
=B	
=B	DB	JB	\B	hB	oB	�B	�B	�B	"�B	$�B	.B	49B	7LB	7LB	9XB	9XB	9XB	9XB	9XB	9XB	:^B	;dB	;dB	;dB	=qB	=qB	>wB	C�B	D�B	E�B	G�B	I�B	I�B	I�B	J�B	L�B	P�B	R�B	VB	W
B	XB	ZB	[#B	_;B	bNB	ffB	ffB	ffB	ffB	ffB	gmB	hsB	n�B	p�B	u�B	v�B	w�B	v�B	w�B	{�B	{�B	|�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�JB	�PB	�VB	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�'B	�'B	�-B	�?B	�XB	�jB	�}B	��B	B	ÖB	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B
JB
PB
PB
PB
PB
bB
bB
bB
bB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
1'B
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
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
=qB
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
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
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
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
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
L�B
L�B
M�B
M�B
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
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
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
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
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
s�B
t�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBDB�B6FB;dB<jB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB<jB<jB<jB<jB<jB<jB<jB<jB=qB=qB=qB=qB=qB=qBA�BA�B@�B?}B?}B?}B?}B:^B0!B)�B�B	7B��B�B�B�fB��B��B�}B��B�7B}�Br�B^5BL�B@�B9XB%�BJBB�mB�#B�B�B��B��B��B��BǮBŢB�-B��Bk�BW
BS�BI�B:^B7LB5?B/B+B$�B!�B�BhB1B
��B
�B
�B
�;B
��B
B
�9B
�!B
�B
��B
��B
�=B
|�B
w�B
p�B
bNB
K�B
9XB
�B
�B
�B
uB
JB	��B	��B	�B	�B	�B	�`B	�/B	��B	��B	�FB	�3B	�B	��B	��B	�B	p�B	e`B	_;B	YB	W
B	R�B	N�B	I�B	G�B	A�B	6FB	,B	$�B	�B	�B	oB	PB	B��B��B�B�B�mB�ZB�HB�5B�/B�)B�B�B�B��B��B��B��BɺBȴBǮBƨBŢB��B�}B�wB�qB�qB�jB�jB�dB�XB�LB�!B�B��B��B��B��B��B��B�{B�bB�DB�+B�B�B� B}�Bz�Bw�Bs�Bq�Bl�BhsBe`BcTB_;B^5B]/B[#BYBXBT�BQ�BP�BO�BM�BL�BL�BL�BK�BJ�BH�BE�BD�BC�BC�BA�B@�B@�B=qB>wB<jB;dB:^B:^B9XB7LB8RB7LB5?B5?B49B33B1'B33B2-B1'B0!B0!B0!B0!B0!B0!B/B/B0!B,B+B(�B'�B%�B%�B%�B%�B%�B$�B#�B#�B#�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B%�B&�B&�B'�B'�B(�B'�B'�B(�B(�B,B2-B49B9XB9XB:^B;dB>wBA�BD�BE�BH�BI�BK�BL�BN�BW
BZB[#BaHBffBjBo�Bs�Bt�Bu�Bx�By�By�B{�B�B�B�+B�1B�PB�\B�bB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�'B�-B�3B�?B�XB�^B�dB�dB�dB�jB�jB�qB�wB�}B��BBÖBĜBĜBĜBƨBǮBȴB��B��B��B��B��B��B�B�
B�B�BB�`B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	1B		7B		7B		7B	
=B	
=B	DB	JB	\B	hB	oB	�B	�B	�B	"�B	$�B	.B	49B	7LB	7LB	9XB	9XB	9XB	9XB	9XB	9XB	:^B	;dB	;dB	;dB	=qB	=qB	>wB	C�B	D�B	E�B	G�B	I�B	I�B	I�B	J�B	L�B	P�B	R�B	VB	W
B	XB	ZB	[#B	_;B	bNB	ffB	ffB	ffB	ffB	ffB	gmB	hsB	n�B	p�B	u�B	v�B	w�B	v�B	w�B	{�B	{�B	|�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�JB	�PB	�VB	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�'B	�'B	�-B	�?B	�XB	�jB	�}B	��B	B	ÖB	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B
JB
PB
PB
PB
PB
bB
bB
bB
bB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
1'B
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
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
=qB
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
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
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
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
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
L�B
L�B
M�B
M�B
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
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
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
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
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
s�B
t�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230315140227                              AO  ARCAADJP                                                                    20230315140227    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230315140227  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230315140227  QCF$                G�O�G�O�G�O�0               