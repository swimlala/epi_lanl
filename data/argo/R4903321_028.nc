CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-01-12T12:00:31Z creation      
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
_FillValue                 �  \d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20210112120031  20210112120031  4903321 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8290                            2B  A   NAVIS_A                         1169                            170425                          863 @�V��b61   @�V+�s~@8�z�G��c2��`A�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB ��B(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D̅D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�ED��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�dZA�bNA�`BA�bNA�bNA�`BA�`BA�\)A�O�A�I�A�C�A�+A��A�oA�bA���A��A���A�A��A��mA��`A��A���A���A��RA��-A��FA��jA��^A��wA���A��`A�A��FA��!A��A���A���A���A�ZA�O�A�$�A���A��HA�VA��A�A�A�7LA�bNA�O�A��A�M�A�9XA���A�`BA��FA��A��A���A��hA��uA�M�A�
=A��jA���A�?}A�l�A���A�~�A��TA��A�1'A��PA��!A���A�ƨA�\)A�/A�(�A�A�;dA�t�A��HA�XA�A� �A�n�A�9XA��/A�l�A� �A�bNA�9XA�O�A�oA��A��A�
=A��HA"�A}7LA{�AxE�Au��At1Ap{An-Al�jAi�Af9XAc�A`9XA^�\A\�\A[�AY��AX��AW��AV(�AS��ARr�AQ/AN��ALz�AJbNAIS�AH��AHM�AGt�AF��AF�uAE�7AC�;AB�\AA�AAx�A@�A?`BA=�FA<VA;x�A:r�A9��A9�A8�DA7�A6��A5�A4z�A3`BA3�A2��A1��A1\)A1/A0�A/l�A.ĜA-�A,�DA+�FA*��A)��A'�mA&jA%hsA#��A"{A!��A �A bA�A\)A�Av�AG�AA(�A��A`BA�yA��A33AA�A|�AoA��A�^A�A�!AA�A�FA`BAQ�A;dA$�AO�A��A9XA
��A
  A	|�A�A��A�#A��A\)A��A-AO�AI�A��A��A��A��AK�A �j@��@���@�E�@�%@���@�&�@��9@�ƨ@���@��@�R@��T@�33@�o@��@�I�@�ƨ@�;d@�`B@���@�S�@��`@�ƨ@��@��@�O�@ܣ�@�j@ە�@�@���@׶F@��H@�x�@ԛ�@���@�v�@ёh@��@�
=@�V@�@���@ˍP@�E�@�&�@��/@ȓu@ǅ@�C�@Ƨ�@�V@ź^@�9X@öF@§�@�{@�O�@�Ĝ@�1@�+@���@��\@�@���@���@���@��@���@�=q@��9@���@�V@���@��@�A�@�|�@�J@�X@��@���@��j@��D@�Z@�ƨ@�n�@�@���@��
@�
=@���@��`@���@���@�ƨ@�K�@��H@��@���@���@�G�@���@��@�r�@� �@��F@��@�@�\)@��@���@�n�@���@��@��w@���@�?}@�p�@�p�@��7@�/@���@�1@���@��@�l�@��m@��@�{@�@�@�p�@�7L@�&�@�%@�z�@�1@�ƨ@���@�"�@��!@��R@��\@�v�@�^5@�-@��@�p�@��@��u@�z�@�b@��
@���@�t�@�
=@�
=@���@���@���@��+@�V@�E�@�5?@��#@��@�G�@��/@�9X@��;@�\)@�"�@��y@�ȴ@��!@��\@�V@�=q@�5?@�-@�{@��@�hs@�Ĝ@�I�@���@��;@���@�|�@�l�@�;d@�@��R@���@��\@�v�@�ff@�M�@��@��T@��h@�`B@�O�@�/@���@���@���@��@�Ĝ@��@�I�@��
@�ƨ@���@��@�|�@�K�@��@���@��R@���@�v�@�5?@�-@��T@�`B@��u@���@�dZ@�+@�
=@��\@�ff@�J@��@�V@���@�Z@�9X@�1'@�9X@�9X@�1@���@��@��y@��!@���@��\@�^5@�E�@�-@��@��#@���@��7@�x�@�x�@�hs@�7L@��9@��u@��D@�j@�Q�@�I�@�A�@��@;d@~��@~��@~ff@~5?@~@}@}�h@}��@}�@~$�@~E�@~�R@�@�b@��@� �@�@l�@+@~ff@~V@}@}/@|1@{S�@z=q@yX@y7L@y7L@y&�@x��@x1'@w�@v��@u��@u/@tj@t�@s�F@s��@sS�@s33@so@s@r�H@rM�@q�^@qX@p��@o�@o�P@o�@nȴ@n��@n@m@m�@l�/@l��@lj@lI�@l9X@l(�@l1@k�@k@j�!@j~�@i�@i��@h�`@h�9@h�@hr�@h1'@hb@g�@g��@g��@g|�@g\)@g�@f�@fv�@f5?@f{@f@e�-@ep�@e/@dj@c�
@cƨ@c�F@cC�@b��@b~�@a��@ax�@aX@`��@`�u@`bN@`b@_�@_�P@_\)@_;d@^�y@^ff@^@]�T@]�-@]��@]p�@\�/@\�j@\�@\z�@[��@[ƨ@[�F@[dZ@[o@Z~�@Y��@Y��@Y��@Yhs@Y�@X�u@XQ�@Xb@W�;@W+@V�+@V@U�@U�@T�@T�/@T�j@T��@Tz�@TZ@S�m@Sƨ@St�@SC�@S@R�@R�H@R~�@RJ@Q��@Q�7@Q7L@P��@P��@P�9@P�u@Pr�@PbN@P1'@O�@O;d@N��@Nȴ@Nff@M�@L�j@L��@Lz�@K�
@KdZ@K33@J�!@J�@I��@I��@I��@IX@I7L@IG�@IG�@IG�@I�@H��@H��@H�@H1'@H  @Gl�@F�@F��@F5?@E�-@EO�@D�/@DZ@C��@C�
@Ct�@C"�@B~�@B-@B�@B�@A��@Ahs@A7L@@�u@?�@?�P@?\)@?K�@?+@>��@>5?@=@=��@=�@=?}@=�@<�@<�D@<�@;�F@;S�@;o@:��@:��@:^5@9��@9X@9�@9�@8�9@8�u@8�@8r�@8r�@81'@8b@7��@7��@7\)@7K�@7+@6�y@6�@6�@6ȴ@6�R@6�R@6�R@6��@6v�@6V@6E�@6$�@5@5O�@4�/@4z�@4I�@3��@3��@3�
@3��@3t�@3t�@3t�@3dZ@3S�@3C�@3C�@3"�@2�@2�\@2^5@1��@1��@1�7@17L@0�`@0�@0Q�@/�;@/�w@/�@/��@/|�@/\)@/�@.�y@.�@.v�@.v�@.ff@.ff@.ff@.ff@.ff@.ff@.{@-@-��@-`B@-?}@-�@,�/@,��@,z�@,Z@,I�@,1@+�
@+ƨ@+�F@+��@+t�@+dZ@+C�@+"�@*�@*��@*^5@*=q@*=q@*-@*J@)�@)��@)x�@)7L@)&�@(��@(��@(Q�@( �@'�w@'|�@'K�@&�y@&��@&��@&�+@&ff@&E�@&$�@&{@%�-@%`B@%O�@%V@$�j@$9X@#�
@#�F@#t�@#dZ@#33@#@"�H@"��@"�!@"��@"��@"�\@"^5@"=q@!��@!�^@!X@!%@ ��@ �9@ ��@ �@ 1'@�w@�P@�P@|�@l�@K�@ȴ@5?@�-@�@p�@`B@`B@?}@/@�@�j@�D@Z@(�@��@��@dZ@"�@@��@�\@-@��@X@&�@%@Ĝ@�@bN@1'@�;@�P@\)@+@ȴ@��@E�@�@@�h@p�@O�@��@��@��@j@9X@1@�
@��@S�@o@��@��@�\@~�@n�@M�@-@��@��@��@7L@�@�`@�@bN@Q�@1'@b@  @  @�@�@�w@|�@\)@+@
=@�@�+@v�@ff@E�@@��@@�@O�@V@�@�/@��@�j@�j@�@�D@j@(�@1@1@��@�
@�F@��@�@dZ@C�@"�@
�@
��@
��@
�\@
n�@
�@	�#@	��@	��@	�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bNA�dZA�bNA�`BA�bNA�bNA�`BA�`BA�\)A�O�A�I�A�C�A�+A��A�oA�bA���A��A���A�A��A��mA��`A��A���A���A��RA��-A��FA��jA��^A��wA���A��`A�A��FA��!A��A���A���A���A�ZA�O�A�$�A���A��HA�VA��A�A�A�7LA�bNA�O�A��A�M�A�9XA���A�`BA��FA��A��A���A��hA��uA�M�A�
=A��jA���A�?}A�l�A���A�~�A��TA��A�1'A��PA��!A���A�ƨA�\)A�/A�(�A�A�;dA�t�A��HA�XA�A� �A�n�A�9XA��/A�l�A� �A�bNA�9XA�O�A�oA��A��A�
=A��HA"�A}7LA{�AxE�Au��At1Ap{An-Al�jAi�Af9XAc�A`9XA^�\A\�\A[�AY��AX��AW��AV(�AS��ARr�AQ/AN��ALz�AJbNAIS�AH��AHM�AGt�AF��AF�uAE�7AC�;AB�\AA�AAx�A@�A?`BA=�FA<VA;x�A:r�A9��A9�A8�DA7�A6��A5�A4z�A3`BA3�A2��A1��A1\)A1/A0�A/l�A.ĜA-�A,�DA+�FA*��A)��A'�mA&jA%hsA#��A"{A!��A �A bA�A\)A�Av�AG�AA(�A��A`BA�yA��A33AA�A|�AoA��A�^A�A�!AA�A�FA`BAQ�A;dA$�AO�A��A9XA
��A
  A	|�A�A��A�#A��A\)A��A-AO�AI�A��A��A��A��AK�A �j@��@���@�E�@�%@���@�&�@��9@�ƨ@���@��@�R@��T@�33@�o@��@�I�@�ƨ@�;d@�`B@���@�S�@��`@�ƨ@��@��@�O�@ܣ�@�j@ە�@�@���@׶F@��H@�x�@ԛ�@���@�v�@ёh@��@�
=@�V@�@���@ˍP@�E�@�&�@��/@ȓu@ǅ@�C�@Ƨ�@�V@ź^@�9X@öF@§�@�{@�O�@�Ĝ@�1@�+@���@��\@�@���@���@���@��@���@�=q@��9@���@�V@���@��@�A�@�|�@�J@�X@��@���@��j@��D@�Z@�ƨ@�n�@�@���@��
@�
=@���@��`@���@���@�ƨ@�K�@��H@��@���@���@�G�@���@��@�r�@� �@��F@��@�@�\)@��@���@�n�@���@��@��w@���@�?}@�p�@�p�@��7@�/@���@�1@���@��@�l�@��m@��@�{@�@�@�p�@�7L@�&�@�%@�z�@�1@�ƨ@���@�"�@��!@��R@��\@�v�@�^5@�-@��@�p�@��@��u@�z�@�b@��
@���@�t�@�
=@�
=@���@���@���@��+@�V@�E�@�5?@��#@��@�G�@��/@�9X@��;@�\)@�"�@��y@�ȴ@��!@��\@�V@�=q@�5?@�-@�{@��@�hs@�Ĝ@�I�@���@��;@���@�|�@�l�@�;d@�@��R@���@��\@�v�@�ff@�M�@��@��T@��h@�`B@�O�@�/@���@���@���@��@�Ĝ@��@�I�@��
@�ƨ@���@��@�|�@�K�@��@���@��R@���@�v�@�5?@�-@��T@�`B@��u@���@�dZ@�+@�
=@��\@�ff@�J@��@�V@���@�Z@�9X@�1'@�9X@�9X@�1@���@��@��y@��!@���@��\@�^5@�E�@�-@��@��#@���@��7@�x�@�x�@�hs@�7L@��9@��u@��D@�j@�Q�@�I�@�A�@��@;d@~��@~��@~ff@~5?@~@}@}�h@}��@}�@~$�@~E�@~�R@�@�b@��@� �@�@l�@+@~ff@~V@}@}/@|1@{S�@z=q@yX@y7L@y7L@y&�@x��@x1'@w�@v��@u��@u/@tj@t�@s�F@s��@sS�@s33@so@s@r�H@rM�@q�^@qX@p��@o�@o�P@o�@nȴ@n��@n@m@m�@l�/@l��@lj@lI�@l9X@l(�@l1@k�@k@j�!@j~�@i�@i��@h�`@h�9@h�@hr�@h1'@hb@g�@g��@g��@g|�@g\)@g�@f�@fv�@f5?@f{@f@e�-@ep�@e/@dj@c�
@cƨ@c�F@cC�@b��@b~�@a��@ax�@aX@`��@`�u@`bN@`b@_�@_�P@_\)@_;d@^�y@^ff@^@]�T@]�-@]��@]p�@\�/@\�j@\�@\z�@[��@[ƨ@[�F@[dZ@[o@Z~�@Y��@Y��@Y��@Yhs@Y�@X�u@XQ�@Xb@W�;@W+@V�+@V@U�@U�@T�@T�/@T�j@T��@Tz�@TZ@S�m@Sƨ@St�@SC�@S@R�@R�H@R~�@RJ@Q��@Q�7@Q7L@P��@P��@P�9@P�u@Pr�@PbN@P1'@O�@O;d@N��@Nȴ@Nff@M�@L�j@L��@Lz�@K�
@KdZ@K33@J�!@J�@I��@I��@I��@IX@I7L@IG�@IG�@IG�@I�@H��@H��@H�@H1'@H  @Gl�@F�@F��@F5?@E�-@EO�@D�/@DZ@C��@C�
@Ct�@C"�@B~�@B-@B�@B�@A��@Ahs@A7L@@�u@?�@?�P@?\)@?K�@?+@>��@>5?@=@=��@=�@=?}@=�@<�@<�D@<�@;�F@;S�@;o@:��@:��@:^5@9��@9X@9�@9�@8�9@8�u@8�@8r�@8r�@81'@8b@7��@7��@7\)@7K�@7+@6�y@6�@6�@6ȴ@6�R@6�R@6�R@6��@6v�@6V@6E�@6$�@5@5O�@4�/@4z�@4I�@3��@3��@3�
@3��@3t�@3t�@3t�@3dZ@3S�@3C�@3C�@3"�@2�@2�\@2^5@1��@1��@1�7@17L@0�`@0�@0Q�@/�;@/�w@/�@/��@/|�@/\)@/�@.�y@.�@.v�@.v�@.ff@.ff@.ff@.ff@.ff@.ff@.{@-@-��@-`B@-?}@-�@,�/@,��@,z�@,Z@,I�@,1@+�
@+ƨ@+�F@+��@+t�@+dZ@+C�@+"�@*�@*��@*^5@*=q@*=q@*-@*J@)�@)��@)x�@)7L@)&�@(��@(��@(Q�@( �@'�w@'|�@'K�@&�y@&��@&��@&�+@&ff@&E�@&$�@&{@%�-@%`B@%O�@%V@$�j@$9X@#�
@#�F@#t�@#dZ@#33@#@"�H@"��@"�!@"��@"��@"�\@"^5@"=q@!��@!�^@!X@!%@ ��@ �9@ ��@ �@ 1'@�w@�P@�P@|�@l�@K�@ȴ@5?@�-@�@p�@`B@`B@?}@/@�@�j@�D@Z@(�@��@��@dZ@"�@@��@�\@-@��@X@&�@%@Ĝ@�@bN@1'@�;@�P@\)@+@ȴ@��@E�@�@@�h@p�@O�@��@��@��@j@9X@1@�
@��@S�@o@��@��@�\@~�@n�@M�@-@��@��@��@7L@�@�`@�@bN@Q�@1'@b@  @  @�@�@�w@|�@\)@+@
=@�@�+@v�@ff@E�@@��@@�@O�@V@�@�/@��@�j@�j@�@�D@j@(�@1@1@��@�
@�F@��@�@dZ@C�@"�@
�@
��@
��@
�\@
n�@
�@	�#@	��@	��@	�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�`B�B��B��B��B��B��BB1B
=B
=BDB\B{B%�B>wB^5BbNBe`BiyBjBjBjBiyBhsBiyBbNBVBK�BD�BD�BJ�BW
B`BB^5BdZBcTB[#BS�BS�BH�BD�BE�BD�BA�B:^B49B49B2-B,B�B�B�B�B�B{B
=B��B�B�sB�B��B�?B�B��B�DBx�B_;BR�BJ�B@�B7LB,B'�B!�B�B
��B
�yB
��B
�wB
�B
��B
�PB
�B
o�B
P�B
=qB
33B
�B
B	��B	�TB	��B	ƨB	�!B	��B	�JB	z�B	o�B	dZB	^5B	VB	M�B	J�B	C�B	7LB	-B	'�B	�B	oB	1B	B��B��B��B�B�B�B�`B�5B�#B�B�B��B��BǮBÖB��B�qB�XB�LB�3B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�%B�B�B~�By�Bq�Bq�Bm�BgmBgmBhsBffBe`Be`BcTBcTBaHB^5B^5B]/B\)BZBXBW
BT�BT�BS�BR�BQ�BM�BK�BK�BI�BG�BG�BD�BB�B?}B>wB:^B7LB5?B33B5?B1'B0!B0!B0!B.B,B)�B'�B$�B#�B#�B$�B"�B"�B �B�B�B�B�B�B�B�B{BoBhB\BbBbB\B\B\BVBbB\B\B\B\BhBhBoBuBoBuBuBuBuB{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B �B �B!�B"�B$�B&�B(�B'�B(�B'�B'�B+B,B.B.B0!B33B7LB9XB=qB?}BB�BB�BE�BK�BN�BN�BO�BP�BQ�BS�BW
B^5BaHBgmBk�Bo�Bv�B|�B|�B}�B�B�+B�JB�VB�bB�uB��B��B��B��B��B��B��B��B�!B�-B�9B�XB�dB�qB�}BǮB��B��B��B��B��B��B��B��B��B�B�B�HB�B�B�B�B�B�B��B��B��B	B	B		7B		7B	VB	\B	bB	bB	bB	hB	hB	uB	uB	�B	�B	�B	�B	!�B	%�B	%�B	&�B	,B	.B	/B	2-B	33B	33B	6FB	9XB	:^B	=qB	C�B	F�B	L�B	N�B	Q�B	R�B	S�B	T�B	XB	XB	YB	YB	ZB	[#B	`BB	e`B	iyB	l�B	m�B	o�B	q�B	q�B	t�B	v�B	x�B	y�B	z�B	{�B	{�B	� B	�B	�+B	�JB	�VB	�\B	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�9B	�?B	�FB	�LB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�jB	�qB	�wB	�}B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�;B	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
	7B

=B

=B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
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
?}B
?}B
?}B
?}B
@�B
@�B
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
C�B
C�B
C�B
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
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
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
VB
VB
VB
VB
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
[#B
[#B
[#B
[#B
[#B
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
]/B
^5B
^5B
_;B
_;B
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
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
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
ffB
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
jB
jB
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
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�`B�B��B��B��B��B��BB1B
=B
=BDB\B{B%�B>wB^5BbNBe`BiyBjBjBjBiyBhsBiyBbNBVBK�BD�BD�BJ�BW
B`BB^5BdZBcTB[#BS�BS�BH�BD�BE�BD�BA�B:^B49B49B2-B,B�B�B�B�B�B{B
=B��B�B�sB�B��B�?B�B��B�DBx�B_;BR�BJ�B@�B7LB,B'�B!�B�B
��B
�yB
��B
�wB
�B
��B
�PB
�B
o�B
P�B
=qB
33B
�B
B	��B	�TB	��B	ƨB	�!B	��B	�JB	z�B	o�B	dZB	^5B	VB	M�B	J�B	C�B	7LB	-B	'�B	�B	oB	1B	B��B��B��B�B�B�B�`B�5B�#B�B�B��B��BǮBÖB��B�qB�XB�LB�3B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�%B�B�B~�By�Bq�Bq�Bm�BgmBgmBhsBffBe`Be`BcTBcTBaHB^5B^5B]/B\)BZBXBW
BT�BT�BS�BR�BQ�BM�BK�BK�BI�BG�BG�BD�BB�B?}B>wB:^B7LB5?B33B5?B1'B0!B0!B0!B.B,B)�B'�B$�B#�B#�B$�B"�B"�B �B�B�B�B�B�B�B�B{BoBhB\BbBbB\B\B\BVBbB\B\B\B\BhBhBoBuBoBuBuBuBuB{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B �B �B!�B"�B$�B&�B(�B'�B(�B'�B'�B+B,B.B.B0!B33B7LB9XB=qB?}BB�BB�BE�BK�BN�BN�BO�BP�BQ�BS�BW
B^5BaHBgmBk�Bo�Bv�B|�B|�B}�B�B�+B�JB�VB�bB�uB��B��B��B��B��B��B��B��B�!B�-B�9B�XB�dB�qB�}BǮB��B��B��B��B��B��B��B��B��B�B�B�HB�B�B�B�B�B�B��B��B��B	B	B		7B		7B	VB	\B	bB	bB	bB	hB	hB	uB	uB	�B	�B	�B	�B	!�B	%�B	%�B	&�B	,B	.B	/B	2-B	33B	33B	6FB	9XB	:^B	=qB	C�B	F�B	L�B	N�B	Q�B	R�B	S�B	T�B	XB	XB	YB	YB	ZB	[#B	`BB	e`B	iyB	l�B	m�B	o�B	q�B	q�B	t�B	v�B	x�B	y�B	z�B	{�B	{�B	� B	�B	�+B	�JB	�VB	�\B	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�9B	�?B	�FB	�LB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�jB	�qB	�wB	�}B	��B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�;B	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
	7B

=B

=B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
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
?}B
?}B
?}B
?}B
@�B
@�B
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
C�B
C�B
C�B
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
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
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
VB
VB
VB
VB
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
[#B
[#B
[#B
[#B
[#B
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
]/B
^5B
^5B
_;B
_;B
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
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
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
ffB
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
jB
jB
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
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210112120031                              AO  ARCAADJP                                                                    20210112120031    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210112120031  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210112120031  QCF$                G�O�G�O�G�O�0               