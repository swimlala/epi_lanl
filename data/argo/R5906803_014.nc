CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-16T17:22:48Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Op   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  rt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �d   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230516172248  20230516172248  5906803 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9276                            2B  A   NAVIS_A                         1437                            170425                          863 @����#V 1   @���!�@:�˒:)��d4����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A       @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ffA��A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��BA33BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33C 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��3D��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfDfD��fD�fD�FfDÆfD��fD�fD�FfDĆfD��fD�fD�FfDņfD��fD�fD�FfDƆfD��fD�fD�FfDǆfD��fD�fD�FfDȆfD��fD�fD�FfDɆfD��fD�fD�FfDʆfD��fD�fD�FfDˆfD��fD�fD�FfD̆fD��fD�fD�FfD͆fD��fD�fD�FfDΆfD��fD�fD�FfDφfD��fD�fD�FfDІfD��fD�fD�FfDцfD��fD�fD�FfD҆fD��fD�fD�C3DӆfD��fD�fD�FfDԆfD��fD�fD�FfDՆfD��fD�fD�FfDֆfD��fD�fD�FfD׆fD��fD�fD�FfD؆fD��fD�fD�FfDنfD��fD�fD�FfDچfD��fD�fD�FfDۆfD��fD�fD�FfD܆fD��fD�fD�FfD݆fD��fD�fD�FfDކfD��fD�fD�FfD߆fD��fD�fD�FfD��fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD��fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD�fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�	�D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��
A���A��
A��
A���A��A��#A��A��AоwAП�AП�AЙ�AЗ�AЛ�AН�AЕ�A�x�A�l�A�JA�$�AΕ�A�|�A�VAʾwAǧ�A�I�Aé�A�VA�33A��TA��mA��A�~�A���A�G�A��-A�bA���A��DA�dZA�  A�A�A��A��A��RA�dZA��A��hA��A�n�A�A�ffA��A���A�JA��A���A��mA�K�A�"�A��A�l�A�XA�C�A�A�t�A���A��hA�x�A��+A���A�l�A��
A�7LA���A�S�A���A�XA��FA�ZA�&�A��
A�dZA��jA��A�%A�1'A�VA���A�+A��A~�A}�Az�9Ayx�Aw�wAv��As��Ap��An��An��Anz�An$�Am�;AmhsAl �Ak`BAj�Ah5?Ae|�Ac"�A`~�A]�wA\�A[��AZM�AW`BAU��AUoAT��AT��ATE�AS�ASAQG�AP�AN�!AN5?AM�wAM
=ALbAKx�AJ��AJ��AJ��AJE�AI�AH�AG7LAD�AC"�AAl�A@9XA?O�A>�RA=��A=��A<$�A:~�A9�wA9x�A9K�A8��A8�\A7�FA7\)A7�A6�DA5�A5�A4�\A4�A3�hA2�\A2  A1�A1`BA0��A/K�A.��A-O�A+
=A)��A%�A#��A#��A#x�A"�`A"�jA!�A v�A�A��AffA�A
=A1'A��A��A��AE�Al�A�uA�
AdZAȴA1'A��Ax�AG�A/A
=A��AĜA~�AI�A{A��AA�AoA�#AoA	�A	�A	`BA	7LA�\At�A^5A�HA$�A��AS�A�A��Av�A(�A��A��A �A {@�l�@�hs@�v�@��7@��y@���@���@���@��@�@ꟾ@�x�@�z�@��@睲@��y@旍@�@�@�(�@ޏ\@�n�@�M�@�{@�O�@�Z@�O�@ׅ@�;d@ָR@�G�@��@�
=@́@��`@̬@̣�@̃@�Z@�I�@��;@ʰ!@���@�dZ@���@��@��y@�-@�@�@��7@�&�@�V@���@�ȴ@�%@��F@���@�v�@�j@��+@��@���@�Q�@���@��
@���@�;d@��!@�^5@�X@�I�@�  @���@��P@�l�@�@�n�@��#@��h@�G�@�7L@�/@��@�V@��@��/@���@��j@�r�@��@��@���@�$�@�{@��h@��@���@���@�dZ@�K�@��y@�O�@��@�C�@�5?@��#@��^@���@�G�@�V@��9@��D@�Q�@���@��R@�M�@�5?@�{@���@��@��#@���@�O�@��D@��F@��@�p�@�O�@�%@�I�@�\)@��H@���@�p�@�?}@�bN@�S�@��T@�%@��u@��@�r�@�bN@�Q�@�I�@�9X@�(�@�1@��;@��;@��
@�dZ@��R@�5?@�G�@��9@�z�@��
@��@��P@�\)@�\)@�dZ@�dZ@�l�@�t�@�|�@�|�@�\)@�@��!@�n�@�n�@�v�@�n�@�n�@�n�@�ff@�^5@�E�@�^5@���@���@���@���@��\@�{@���@��@�J@��h@��@��@�=q@���@�@��h@�`B@��@��/@�z�@�1@��@��@�
=@�@���@���@��H@��R@���@�^5@�$�@�J@���@��7@�X@�G�@�/@�%@�%@�%@��`@�Ĝ@���@��D@�z�@�r�@�9X@�9X@�1'@�1'@�1@|�@l�@~�@}��@|(�@{S�@{o@z�@zn�@y7L@xbN@wK�@v��@u@t��@t(�@t(�@t�@t�@sƨ@sC�@s33@s"�@so@s@r�@r�H@r��@r��@r�!@r�\@rn�@rM�@r=q@r�@q�@q�@q��@q�^@q�7@qX@q%@p�9@p�@p�@pA�@o��@o
=@n�R@n{@m�h@m`B@mO�@m�@lZ@l(�@k�F@k33@k@ko@j�H@j�\@j^5@jJ@iX@i%@h��@hĜ@h��@h�@hr�@hA�@hb@g�;@g|�@g�@f�+@f$�@e�h@eV@dZ@c"�@b��@b�!@b�\@b^5@bM�@bM�@bM�@b^5@bn�@bn�@b�@a�@aG�@`�9@_
=@^V@^@]@]�h@]�h@]�@]p�@]`B@]O�@]V@\�/@\��@\��@\��@\��@\�/@\��@\�@[�F@[ƨ@\�@[��@Z��@Yhs@Y&�@Y�@Y%@X�`@X�9@Xr�@X  @W�w@W;d@V��@VV@V5?@V$�@V{@U�T@U��@U@U�-@Up�@U/@T��@Tj@T9X@Sƨ@S33@R��@R�\@Rn�@R�@QX@Pr�@O|�@N��@NE�@N{@M�T@M��@M@M�@MO�@L�@Kƨ@KdZ@KC�@K"�@J��@J�H@J�H@J��@J��@J�\@I�@I�^@I7L@I&�@H��@HbN@Hr�@HA�@H �@H �@Hb@G�@G�@G|�@F��@E@D�j@D�D@Dz�@Dz�@Dj@Dj@Dj@DZ@DZ@DI�@C�
@C�@C�@Ct�@CS�@Co@C@B�H@B�!@B��@B�\@BM�@B�@A�@A�#@A�#@A�#@A��@Ax�@AG�@A&�@@��@?�@>��@>�@>5?@>$�@>{@=��@=p�@<��@<1@;�
@;�F@;��@;dZ@:�!@:^5@:�@:J@9��@9��@9x�@9%@8��@8�9@8�@8bN@8b@7�@7��@7�P@7|�@7+@6v�@6$�@6@6@5�@5�T@5@5@5@5@5�-@5@5@5@5@5@5`B@4�@4�D@4�@3��@3o@2~�@1��@1��@1hs@1&�@0��@0�@01'@/�w@/�P@/K�@/�@.��@.��@.��@.�y@.v�@.E�@.$�@-@-?}@,�/@,�@+t�@*��@*�@)��@)X@(r�@(  @'�;@'��@'�@'�P@'\)@'�@&�R@&v�@&5?@%@%��@%��@%�@%`B@%O�@%?}@$��@$I�@$I�@$9X@$(�@$�@$�@$�@$�@#��@#�m@#�m@#�F@#��@#��@#t�@#"�@"�@"��@"~�@"^5@!��@!G�@ ��@ �@  �@l�@+@�@�R@��@��@ff@5?@{@@�@�@�@�T@�@�T@�T@�T@�T@��@��@O�@?}@/@�@(�@��@�
@�F@"�@@��@��@��@=q@��@�^@�7@hs@X@&�@Ĝ@1'@�w@|�@K�@�@�R@v�@5?@�@�@�T@@@�-@��@��@Z@�@�F@�H@�#@��@hs@X@X@X@X@G�@�@�9@�@r�@r�@r�@r�@r�@bN@Q�@ �@  @b@�@�;@�;@��@�@�@K�@
=@�y@�R@��@ff@�@`B@?}@V@�/@Z@�
@��@dZ@33@o@@
�H@
��@
��@
�!@
�!@
��@
�\@
�\@
�\@
~�@
~�@
n�@
^5@
M�@
-@
J@	�@	�^@	��@	�7@	hs@	%@��@r�@bN@1'@b@�;@��@��@|�@K�@�@
=@
=@��@�@��@ff@E�@�T@O�@�@�/@�j@�j@��@z�@j@9X@9X@9X@9X@9X@9X@9X@(�@�@1@��@��@�m@ƨ@��@S�@C�@C�@C�@C�@33@�@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��
A���A��
A��
A���A��A��#A��A��AоwAП�AП�AЙ�AЗ�AЛ�AН�AЕ�A�x�A�l�A�JA�$�AΕ�A�|�A�VAʾwAǧ�A�I�Aé�A�VA�33A��TA��mA��A�~�A���A�G�A��-A�bA���A��DA�dZA�  A�A�A��A��A��RA�dZA��A��hA��A�n�A�A�ffA��A���A�JA��A���A��mA�K�A�"�A��A�l�A�XA�C�A�A�t�A���A��hA�x�A��+A���A�l�A��
A�7LA���A�S�A���A�XA��FA�ZA�&�A��
A�dZA��jA��A�%A�1'A�VA���A�+A��A~�A}�Az�9Ayx�Aw�wAv��As��Ap��An��An��Anz�An$�Am�;AmhsAl �Ak`BAj�Ah5?Ae|�Ac"�A`~�A]�wA\�A[��AZM�AW`BAU��AUoAT��AT��ATE�AS�ASAQG�AP�AN�!AN5?AM�wAM
=ALbAKx�AJ��AJ��AJ��AJE�AI�AH�AG7LAD�AC"�AAl�A@9XA?O�A>�RA=��A=��A<$�A:~�A9�wA9x�A9K�A8��A8�\A7�FA7\)A7�A6�DA5�A5�A4�\A4�A3�hA2�\A2  A1�A1`BA0��A/K�A.��A-O�A+
=A)��A%�A#��A#��A#x�A"�`A"�jA!�A v�A�A��AffA�A
=A1'A��A��A��AE�Al�A�uA�
AdZAȴA1'A��Ax�AG�A/A
=A��AĜA~�AI�A{A��AA�AoA�#AoA	�A	�A	`BA	7LA�\At�A^5A�HA$�A��AS�A�A��Av�A(�A��A��A �A {@�l�@�hs@�v�@��7@��y@���@���@���@��@�@ꟾ@�x�@�z�@��@睲@��y@旍@�@�@�(�@ޏ\@�n�@�M�@�{@�O�@�Z@�O�@ׅ@�;d@ָR@�G�@��@�
=@́@��`@̬@̣�@̃@�Z@�I�@��;@ʰ!@���@�dZ@���@��@��y@�-@�@�@��7@�&�@�V@���@�ȴ@�%@��F@���@�v�@�j@��+@��@���@�Q�@���@��
@���@�;d@��!@�^5@�X@�I�@�  @���@��P@�l�@�@�n�@��#@��h@�G�@�7L@�/@��@�V@��@��/@���@��j@�r�@��@��@���@�$�@�{@��h@��@���@���@�dZ@�K�@��y@�O�@��@�C�@�5?@��#@��^@���@�G�@�V@��9@��D@�Q�@���@��R@�M�@�5?@�{@���@��@��#@���@�O�@��D@��F@��@�p�@�O�@�%@�I�@�\)@��H@���@�p�@�?}@�bN@�S�@��T@�%@��u@��@�r�@�bN@�Q�@�I�@�9X@�(�@�1@��;@��;@��
@�dZ@��R@�5?@�G�@��9@�z�@��
@��@��P@�\)@�\)@�dZ@�dZ@�l�@�t�@�|�@�|�@�\)@�@��!@�n�@�n�@�v�@�n�@�n�@�n�@�ff@�^5@�E�@�^5@���@���@���@���@��\@�{@���@��@�J@��h@��@��@�=q@���@�@��h@�`B@��@��/@�z�@�1@��@��@�
=@�@���@���@��H@��R@���@�^5@�$�@�J@���@��7@�X@�G�@�/@�%@�%@�%@��`@�Ĝ@���@��D@�z�@�r�@�9X@�9X@�1'@�1'@�1@|�@l�@~�@}��@|(�@{S�@{o@z�@zn�@y7L@xbN@wK�@v��@u@t��@t(�@t(�@t�@t�@sƨ@sC�@s33@s"�@so@s@r�@r�H@r��@r��@r�!@r�\@rn�@rM�@r=q@r�@q�@q�@q��@q�^@q�7@qX@q%@p�9@p�@p�@pA�@o��@o
=@n�R@n{@m�h@m`B@mO�@m�@lZ@l(�@k�F@k33@k@ko@j�H@j�\@j^5@jJ@iX@i%@h��@hĜ@h��@h�@hr�@hA�@hb@g�;@g|�@g�@f�+@f$�@e�h@eV@dZ@c"�@b��@b�!@b�\@b^5@bM�@bM�@bM�@b^5@bn�@bn�@b�@a�@aG�@`�9@_
=@^V@^@]@]�h@]�h@]�@]p�@]`B@]O�@]V@\�/@\��@\��@\��@\��@\�/@\��@\�@[�F@[ƨ@\�@[��@Z��@Yhs@Y&�@Y�@Y%@X�`@X�9@Xr�@X  @W�w@W;d@V��@VV@V5?@V$�@V{@U�T@U��@U@U�-@Up�@U/@T��@Tj@T9X@Sƨ@S33@R��@R�\@Rn�@R�@QX@Pr�@O|�@N��@NE�@N{@M�T@M��@M@M�@MO�@L�@Kƨ@KdZ@KC�@K"�@J��@J�H@J�H@J��@J��@J�\@I�@I�^@I7L@I&�@H��@HbN@Hr�@HA�@H �@H �@Hb@G�@G�@G|�@F��@E@D�j@D�D@Dz�@Dz�@Dj@Dj@Dj@DZ@DZ@DI�@C�
@C�@C�@Ct�@CS�@Co@C@B�H@B�!@B��@B�\@BM�@B�@A�@A�#@A�#@A�#@A��@Ax�@AG�@A&�@@��@?�@>��@>�@>5?@>$�@>{@=��@=p�@<��@<1@;�
@;�F@;��@;dZ@:�!@:^5@:�@:J@9��@9��@9x�@9%@8��@8�9@8�@8bN@8b@7�@7��@7�P@7|�@7+@6v�@6$�@6@6@5�@5�T@5@5@5@5@5�-@5@5@5@5@5@5`B@4�@4�D@4�@3��@3o@2~�@1��@1��@1hs@1&�@0��@0�@01'@/�w@/�P@/K�@/�@.��@.��@.��@.�y@.v�@.E�@.$�@-@-?}@,�/@,�@+t�@*��@*�@)��@)X@(r�@(  @'�;@'��@'�@'�P@'\)@'�@&�R@&v�@&5?@%@%��@%��@%�@%`B@%O�@%?}@$��@$I�@$I�@$9X@$(�@$�@$�@$�@$�@#��@#�m@#�m@#�F@#��@#��@#t�@#"�@"�@"��@"~�@"^5@!��@!G�@ ��@ �@  �@l�@+@�@�R@��@��@ff@5?@{@@�@�@�@�T@�@�T@�T@�T@�T@��@��@O�@?}@/@�@(�@��@�
@�F@"�@@��@��@��@=q@��@�^@�7@hs@X@&�@Ĝ@1'@�w@|�@K�@�@�R@v�@5?@�@�@�T@@@�-@��@��@Z@�@�F@�H@�#@��@hs@X@X@X@X@G�@�@�9@�@r�@r�@r�@r�@r�@bN@Q�@ �@  @b@�@�;@�;@��@�@�@K�@
=@�y@�R@��@ff@�@`B@?}@V@�/@Z@�
@��@dZ@33@o@@
�H@
��@
��@
�!@
�!@
��@
�\@
�\@
�\@
~�@
~�@
n�@
^5@
M�@
-@
J@	�@	�^@	��@	�7@	hs@	%@��@r�@bN@1'@b@�;@��@��@|�@K�@�@
=@
=@��@�@��@ff@E�@�T@O�@�@�/@�j@�j@��@z�@j@9X@9X@9X@9X@9X@9X@9X@(�@�@1@��@��@�m@ƨ@��@S�@C�@C�@C�@C�@33@�@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�^B�^B�XB�XB�XB�^B�^B�^B�^B�^B�^B�LB�3B�3B�3B�3B�9B�FB�?B�-B�9B�B�uB�7Bv�BffBP�B,B�BB�B�qB�B��B��B��B�oB�\B�DB�+B�Bz�Bn�BbNBZBR�BJ�BG�BD�B@�B=qB<jB:^B5?B(�B�B{B��B�B�;B��BɺB�^B�3B�B�B�B��B�uB�Bv�BdZBW
BM�BE�B<jB2-B�BB
��B
�mB
�#B
��B
��B
��B
ĜB
�dB
�B
��B
�\B
�B
m�B
VB
9XB
'�B
#�B
{B
1B	��B	�B	�yB	��B	ȴB	ŢB	ŢB	B	��B	�qB	�LB	�!B	�B	��B	�VB	~�B	p�B	aHB	YB	T�B	J�B	>wB	33B	1'B	/B	.B	,B	(�B	%�B	�B	�B	bB	PB	
=B	+B	B��B��B��B��B��B��B�B�B�NB�)B��B��B��B��BȴBǮBÖB�wB�dB�XB�RB�LB�?B�9B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B�hB�bB�B~�Bz�Bo�Bn�Bm�Bk�BjBhsBdZBbNB`BB^5B\)BYBXBS�BP�BN�BL�BK�BI�BG�BF�BE�BD�BC�BB�BA�BA�BA�B@�B@�B?}B>wB<jB;dB8RB49B2-B0!B0!B.B-B,B,B)�B'�B$�B#�B"�B"�B!�B!�B �B �B�B�B�B�B�B�B�B�B�BuBuBoBhBhBbBbBbB\B\B\BVBVBPB\BVBPBPBPBJBJBPBJBJBDBDBJBVBVBVBVBVBVBVBVBPBVBVBbBoB{BuB�B�B�B�B�B�B{B�B�B�B�B�B$�B(�B+B.B/B0!B1'B2-B49B5?B6FB9XB<jB=qB=qB>wB>wB@�BB�BC�BC�BD�BD�BE�BE�BF�BG�BH�BH�BH�BJ�BL�BN�BQ�BS�BS�BVB]/B]/B_;B_;B_;BaHBiyBm�Bt�By�B{�B|�B|�B~�B~�B�B�B�B�%B�=B�DB�DB�JB�JB�JB�JB�JB�VB�oB��B��B��B��B��B��B�B�'B�FB�RB�XB�wBÖB��B��B��B��B��B��B��B�B�B�
B�B�#B�)B�)B�BB�ZB�fB�B�B�B��B��B��B��B��B��B	  B	B	B	B	%B	1B	JB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	 �B	�B	�B	!�B	#�B	#�B	+B	1'B	33B	49B	5?B	6FB	9XB	;dB	>wB	A�B	E�B	K�B	P�B	P�B	Q�B	Q�B	R�B	S�B	VB	W
B	ZB	]/B	^5B	aHB	cTB	e`B	e`B	ffB	hsB	iyB	jB	m�B	o�B	p�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	w�B	z�B	{�B	{�B	|�B	�B	�B	�%B	�1B	�DB	�VB	�bB	�bB	�bB	�bB	�oB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�FB	�RB	�XB	�^B	�dB	��B	��B	ÖB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�TB	�HB	�BB	�BB	�BB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
+B
1B
	7B
	7B

=B

=B

=B

=B

=B
PB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
hB
bB
hB
hB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
5?B
5?B
6FB
7LB
7LB
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
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
>wB
>wB
?}B
@�B
@�B
A�B
B�B
C�B
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
F�B
F�B
F�B
F�B
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
H�B
H�B
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
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�^B�^B�XB�XB�XB�^B�^B�^B�^B�^B�^B�LB�3B�3B�3B�3B�9B�FB�?B�-B�9B�B�uB�7Bv�BffBP�B,B�BB�B�qB�B��B��B��B�oB�\B�DB�+B�Bz�Bn�BbNBZBR�BJ�BG�BD�B@�B=qB<jB:^B5?B(�B�B{B��B�B�;B��BɺB�^B�3B�B�B�B��B�uB�Bv�BdZBW
BM�BE�B<jB2-B�BB
��B
�mB
�#B
��B
��B
��B
ĜB
�dB
�B
��B
�\B
�B
m�B
VB
9XB
'�B
#�B
{B
1B	��B	�B	�yB	��B	ȴB	ŢB	ŢB	B	��B	�qB	�LB	�!B	�B	��B	�VB	~�B	p�B	aHB	YB	T�B	J�B	>wB	33B	1'B	/B	.B	,B	(�B	%�B	�B	�B	bB	PB	
=B	+B	B��B��B��B��B��B��B�B�B�NB�)B��B��B��B��BȴBǮBÖB�wB�dB�XB�RB�LB�?B�9B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B�hB�bB�B~�Bz�Bo�Bn�Bm�Bk�BjBhsBdZBbNB`BB^5B\)BYBXBS�BP�BN�BL�BK�BI�BG�BF�BE�BD�BC�BB�BA�BA�BA�B@�B@�B?}B>wB<jB;dB8RB49B2-B0!B0!B.B-B,B,B)�B'�B$�B#�B"�B"�B!�B!�B �B �B�B�B�B�B�B�B�B�B�BuBuBoBhBhBbBbBbB\B\B\BVBVBPB\BVBPBPBPBJBJBPBJBJBDBDBJBVBVBVBVBVBVBVBVBPBVBVBbBoB{BuB�B�B�B�B�B�B{B�B�B�B�B�B$�B(�B+B.B/B0!B1'B2-B49B5?B6FB9XB<jB=qB=qB>wB>wB@�BB�BC�BC�BD�BD�BE�BE�BF�BG�BH�BH�BH�BJ�BL�BN�BQ�BS�BS�BVB]/B]/B_;B_;B_;BaHBiyBm�Bt�By�B{�B|�B|�B~�B~�B�B�B�B�%B�=B�DB�DB�JB�JB�JB�JB�JB�VB�oB��B��B��B��B��B��B�B�'B�FB�RB�XB�wBÖB��B��B��B��B��B��B��B�B�B�
B�B�#B�)B�)B�BB�ZB�fB�B�B�B��B��B��B��B��B��B	  B	B	B	B	%B	1B	JB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	 �B	�B	�B	!�B	#�B	#�B	+B	1'B	33B	49B	5?B	6FB	9XB	;dB	>wB	A�B	E�B	K�B	P�B	P�B	Q�B	Q�B	R�B	S�B	VB	W
B	ZB	]/B	^5B	aHB	cTB	e`B	e`B	ffB	hsB	iyB	jB	m�B	o�B	p�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	w�B	z�B	{�B	{�B	|�B	�B	�B	�%B	�1B	�DB	�VB	�bB	�bB	�bB	�bB	�oB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�FB	�RB	�XB	�^B	�dB	��B	��B	ÖB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�TB	�HB	�BB	�BB	�BB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
+B
1B
	7B
	7B

=B

=B

=B

=B

=B
PB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
hB
bB
hB
hB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
5?B
5?B
6FB
7LB
7LB
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
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
>wB
>wB
?}B
@�B
@�B
A�B
B�B
C�B
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
F�B
F�B
F�B
F�B
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
H�B
H�B
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
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
q�B
q�B
q�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                       ( - , * % $ %                                                                                           !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ! 4 ?000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230516172248                                          AO  ARCAADJP                                                                    20230516172248    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230516172248  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230516172248  QCF$                G�O�G�O�G�O�0               