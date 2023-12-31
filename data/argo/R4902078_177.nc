CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-03-01T11:00:44Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20200301110044  20200301110044  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @�ؼMj�1   @��So�>@+o��v��dq�7Kƨ1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@љ�A��A*fgAH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B
33B33B33B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B�L�B��4B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt�3Du#3Du�3Dv#3Dv�3Dw#3Dw�3Dx#3Dx�3Dy#3Dy�3Dz#3Dz�3D{#3D{�3D|#3D|�3D}#3D}�3D~#3D~�3D#3D�3D��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D�D�њD��D�Q�DÑ�D�њD��D�Q�Dđ�D�њD��D�Q�Dő�D�њD��D�Q�DƑ�D�њD��D�Q�DǑ�D�њD��D�Q�Dȑ�D�њD��D�Q�Dɑ�D�њD��D�Q�Dʑ�D�њD��D�Q�Dˑ�D�њD��D�Q�D̑�D�њD��D�Q�D͑�D�њD��D�Q�DΑ�D�њD��D�Q�Dϑ�D�њD��D�Q�DБ�D�њD��D�Q�Dё�D�њD��D�Q�Dґ�D�њD��D�Q�Dӑ�D�њD��D�Q�Dԑ�D�њD��D�Q�DՑ�D�њD��D�Q�D֑�D�њD��D�Q�Dב�D�њD��D�Q�Dؑ�D�њD��D�Q�Dّ�D�њD��D�Q�Dڑ�D�њD��D�Q�Dۑ�D�њD��D�Q�Dܑ�D�њD��D�Q�Dݑ�D�њD��D�Q�Dޑ�D�њD��D�Q�Dߑ�D�њD��D�Q�D���D�њD��D�Q�DᑚD�њD��D�Q�D⑚D�њD��D�Q�D㑚D�њD��D�Q�D䑚D�њD��D�Q�D呚D�њD��D�Q�D摚D�њD��D�Q�D瑚D�њD��D�Q�D葚D�њD��D�Q�D鑚D�њD��D�Q�DꑚD�њD�gD�Q�D둚D�њD��D�Q�D쑚D�њD��D�Q�D푚D�њD��D�Q�DD�њD��D�Q�DD�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΙ�AΕ�AΗ�AΙ�AΗ�AΟ�AΥ�AΥ�AΧ�AΧ�AΩ�AΩ�AΩ�AάAΥ�AΥ�AΩ�AΩ�AΩ�AΥ�AΣ�AΥ�AΝ�AΛ�AΟ�AΡ�AΛ�A�p�A�=qA�I�A�5?A� �A���A���A���A�ƨA���A�A��!A���A�K�A��A�?}A�ȴA���A���A��
A�VA��yA�ZA��A��yA��PA�XA�XA�;dA�{A�M�A�JA�oA�n�A�33A���A�ffA��DA�VA�S�A�{A�dZA��`A�l�A�A���A�l�A�ZA�FA{�Ax��AvVAu\)At�HArE�AnĜAm/AlZAk�hAf��Ae�Ac�PA`M�A]��A\��AXjAS�AQhsAPffANM�AK�AI��AI"�AH9XAG
=AA��A?`BA>z�A;�A7�-A5��A5?}A4��A3��A2��A2�A1G�A0Q�A/K�A.^5A,�RA,�A+|�A+XA*�\A)�A)�TA)�;A)A)��A)�A)x�A)XA(�RA(�\A(JA'dZA&�/A%��A%�A$I�A#XA!��A!oA z�A�mAhsA33A�/AVA�jA�yAffA`BAbNAA��A{A-A��A��AA�A�PA"�A�`A��A&�AG�A��A�`A�jA9XA�PA�A\)A+A%A;dA"�A��AJAt�A?}A33A
=A��AA�A  A��A��At�AĜA�9A�\An�AZA�AO�AI�A �A��A`BA��A�A��AhsA?}A;dA;dA�A
ȴA
�A
bNA	ƨA	XA	%A�HA��AjA$�A��A`BAG�A33A+A�A�A�+AVA(�A�A�AdZA/AI�A�TAp�A�A��A��A1A�AdZA/AA ȴ@���@���@�33@���@��T@�?}@�Z@�S�@�M�@��^@��@��@��w@���@�G�@�V@���@� �@�E�@�G�@��/@��@���@�;d@��y@�n�@�=q@��@홚@���@� �@�@��@���@�`B@��@�9X@��;@�t�@�C�@���@�{@�?}@䛦@��m@��@��@��y@�M�@�7@�`B@��@��D@�1@�+@�n�@�@ݺ^@ݩ�@ݙ�@��@� �@ۥ�@ڰ!@�G�@؋D@�(�@��@��@�1@���@��m@�K�@֟�@�{@�hs@��/@Ӿw@�+@҇+@���@�&�@Ѓ@�1@��@ϝ�@�@�5?@��T@���@�@�@�`B@̬@�j@���@���@�E�@�@��T@ɡ�@�7L@ȴ9@��;@Ǯ@�\)@Ƨ�@�n�@�=q@�{@��@ř�@�&�@ēu@þw@�K�@�ȴ@�=q@��^@�?}@��@���@�1@�|�@��R@�~�@�=q@�J@��#@��-@���@�hs@�Ĝ@�I�@�  @��P@�dZ@�S�@�33@��@��y@���@�@�/@���@���@� �@�\)@��y@���@�~�@�=q@�{@���@���@��h@��@�x�@�hs@�G�@���@��@�"�@��!@�J@��@���@��/@���@���@�1'@��
@�t�@�
=@��T@��@��@�r�@�A�@��@���@��;@���@�ƨ@��P@�l�@�dZ@�\)@�C�@��y@��@�x�@�/@��`@���@��@�1'@���@�C�@��y@��!@��+@�E�@��^@�G�@��@���@�bN@� �@�\)@���@��@��^@���@�X@�&�@�%@���@�9X@�  @��;@���@�t�@�S�@�@���@�^5@�$�@���@���@�`B@�G�@�%@���@��@��9@��@��
@���@�\)@���@��-@��`@��9@���@�r�@���@�\)@�
=@���@�v�@�n�@�-@�@��-@�hs@���@�bN@�9X@� �@��m@��@�l�@�\)@�S�@�K�@�C�@�33@��@�~�@�{@�x�@�p�@�X@���@�j@�A�@�(�@� �@�  @���@�o@��H@��\@��#@���@�hs@���@���@�bN@��@���@���@���@�l�@�
=@�ȴ@���@�~�@�^5@�M�@�M�@�@�@���@���@��@�`B@��@��`@��@�Z@�9X@���@��F@��@���@�|�@�S�@��@��H@���@�n�@�V@�=q@��@��@���@�?}@���@�Z@�Q�@��m@�|�@�
=@��+@��@��T@���@�hs@�&�@�%@�Ĝ@��9@���@��@�r�@�Q�@�A�@�b@�w@|�@l�@;d@~�@~�+@~{@~{@}@|Z@{��@{ƨ@{�@{dZ@z�!@z=q@y�#@y��@y�7@yG�@y�@xb@w��@w�P@w\)@w;d@v�y@v��@vff@v$�@u@u�@u/@t�j@tj@s��@s��@s33@r�@r�H@rM�@q�@q�^@q7L@p��@o�@o|�@nȴ@n$�@m�T@m�h@mV@l�@lI�@k�F@k�@ko@j��@j��@j~�@j=q@i�@i��@iG�@i%@h��@hbN@h �@g�@g\)@fȴ@fV@f{@ep�@d�/@d�@d��@d�D@dz�@d9X@c��@co@bn�@a�#@a��@a&�@`r�@` �@_��@_�@^�@^��@^E�@]��@]/@\�D@[��@Z�@Z��@Z^5@Y�#@Y7L@X��@XA�@X �@W�;@W��@W
=@V�+@Vff@VE�@V$�@U@UO�@T��@T�D@TZ@SS�@S"�@So@So@S@R=q@Q7L@P��@P��@Pr�@PbN@PA�@P �@O�w@O�P@O+@N�R@N5?@M��@M?}@MV@L�@L�@K�
@KS�@J�@Jn�@JJ@I�^@I��@I�7@Ihs@I&�@H�9@HA�@G�;@G
=@FE�@E�T@E`B@D�@D�@Dz�@D(�@D�@D�@D�@D1@C�F@C��@Ct�@CS�@C@B��@Bn�@BM�@B-@BJ@A�#@A��@Ax�@A%@@Ĝ@@�9@@�u@@bN@@b@?K�@>V@=��@=�@<�@<�D@<z�@<j@<�@;��@;dZ@:~�@9x�@9%@8�u@8A�@81'@8 �@7�@7�@7|�@7+@6�@6��@6�+@6E�@5��@5�@5O�@5V@4�@4��@49X@3�
@3�@3o@2�H@2�!@2n�@2-@1��@1�^@1��@1x�@1G�@1%@0��@0��@0��@01'@/�;@/\)@/+@.��@.�@.ȴ@.�R@.��@.��@.V@.$�@-�@-��@-?}@,�@,j@,I�@,I�@,9X@+��@+t�@+dZ@+C�@*^5@)��@)��@)�7@)G�@(��@(��@(Ĝ@(��@(�@(bN@(A�@'�@'��@'�@'\)@'�@&��@&�y@&ȴ@&��@&{@%��@%�@%/@$��@$��@$��@$�D@$Z@$�@#�
@#��@#t�@#"�@#@#@"�@"��@"��@"M�@!��@!&�@!�@ ��@ ��@ Ĝ@ bN@ b@   @ r�@ Q�@   @�;@\)@;d@+@+@;d@��@��@�@�-@�@O�@O�@?}@?}@V@��@�@�D@Z@�@�
@dZ@S�@"�@o@@^5@-@�@��@��@��@�7@G�@&�@�`@�@1'@  @��@|�@l�@K�@��@ff@�@�h@p�@?}@`B@`B@`B@?}@�@/@��@j@I�@9X@(�@�@�m@ƨ@��@�@C�@�@��@��@��@�\@n�@J@�#@�#@��@�^@��@��@G�@��@��@�9@Q�@ �@�@�w@�@�@��@�@��@\)@�y@ȴ@�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AΙ�AΕ�AΗ�AΙ�AΗ�AΟ�AΥ�AΥ�AΧ�AΧ�AΩ�AΩ�AΩ�AάAΥ�AΥ�AΩ�AΩ�AΩ�AΥ�AΣ�AΥ�AΝ�AΛ�AΟ�AΡ�AΛ�A�p�A�=qA�I�A�5?A� �A���A���A���A�ƨA���A�A��!A���A�K�A��A�?}A�ȴA���A���A��
A�VA��yA�ZA��A��yA��PA�XA�XA�;dA�{A�M�A�JA�oA�n�A�33A���A�ffA��DA�VA�S�A�{A�dZA��`A�l�A�A���A�l�A�ZA�FA{�Ax��AvVAu\)At�HArE�AnĜAm/AlZAk�hAf��Ae�Ac�PA`M�A]��A\��AXjAS�AQhsAPffANM�AK�AI��AI"�AH9XAG
=AA��A?`BA>z�A;�A7�-A5��A5?}A4��A3��A2��A2�A1G�A0Q�A/K�A.^5A,�RA,�A+|�A+XA*�\A)�A)�TA)�;A)A)��A)�A)x�A)XA(�RA(�\A(JA'dZA&�/A%��A%�A$I�A#XA!��A!oA z�A�mAhsA33A�/AVA�jA�yAffA`BAbNAA��A{A-A��A��AA�A�PA"�A�`A��A&�AG�A��A�`A�jA9XA�PA�A\)A+A%A;dA"�A��AJAt�A?}A33A
=A��AA�A  A��A��At�AĜA�9A�\An�AZA�AO�AI�A �A��A`BA��A�A��AhsA?}A;dA;dA�A
ȴA
�A
bNA	ƨA	XA	%A�HA��AjA$�A��A`BAG�A33A+A�A�A�+AVA(�A�A�AdZA/AI�A�TAp�A�A��A��A1A�AdZA/AA ȴ@���@���@�33@���@��T@�?}@�Z@�S�@�M�@��^@��@��@��w@���@�G�@�V@���@� �@�E�@�G�@��/@��@���@�;d@��y@�n�@�=q@��@홚@���@� �@�@��@���@�`B@��@�9X@��;@�t�@�C�@���@�{@�?}@䛦@��m@��@��@��y@�M�@�7@�`B@��@��D@�1@�+@�n�@�@ݺ^@ݩ�@ݙ�@��@� �@ۥ�@ڰ!@�G�@؋D@�(�@��@��@�1@���@��m@�K�@֟�@�{@�hs@��/@Ӿw@�+@҇+@���@�&�@Ѓ@�1@��@ϝ�@�@�5?@��T@���@�@�@�`B@̬@�j@���@���@�E�@�@��T@ɡ�@�7L@ȴ9@��;@Ǯ@�\)@Ƨ�@�n�@�=q@�{@��@ř�@�&�@ēu@þw@�K�@�ȴ@�=q@��^@�?}@��@���@�1@�|�@��R@�~�@�=q@�J@��#@��-@���@�hs@�Ĝ@�I�@�  @��P@�dZ@�S�@�33@��@��y@���@�@�/@���@���@� �@�\)@��y@���@�~�@�=q@�{@���@���@��h@��@�x�@�hs@�G�@���@��@�"�@��!@�J@��@���@��/@���@���@�1'@��
@�t�@�
=@��T@��@��@�r�@�A�@��@���@��;@���@�ƨ@��P@�l�@�dZ@�\)@�C�@��y@��@�x�@�/@��`@���@��@�1'@���@�C�@��y@��!@��+@�E�@��^@�G�@��@���@�bN@� �@�\)@���@��@��^@���@�X@�&�@�%@���@�9X@�  @��;@���@�t�@�S�@�@���@�^5@�$�@���@���@�`B@�G�@�%@���@��@��9@��@��
@���@�\)@���@��-@��`@��9@���@�r�@���@�\)@�
=@���@�v�@�n�@�-@�@��-@�hs@���@�bN@�9X@� �@��m@��@�l�@�\)@�S�@�K�@�C�@�33@��@�~�@�{@�x�@�p�@�X@���@�j@�A�@�(�@� �@�  @���@�o@��H@��\@��#@���@�hs@���@���@�bN@��@���@���@���@�l�@�
=@�ȴ@���@�~�@�^5@�M�@�M�@�@�@���@���@��@�`B@��@��`@��@�Z@�9X@���@��F@��@���@�|�@�S�@��@��H@���@�n�@�V@�=q@��@��@���@�?}@���@�Z@�Q�@��m@�|�@�
=@��+@��@��T@���@�hs@�&�@�%@�Ĝ@��9@���@��@�r�@�Q�@�A�@�b@�w@|�@l�@;d@~�@~�+@~{@~{@}@|Z@{��@{ƨ@{�@{dZ@z�!@z=q@y�#@y��@y�7@yG�@y�@xb@w��@w�P@w\)@w;d@v�y@v��@vff@v$�@u@u�@u/@t�j@tj@s��@s��@s33@r�@r�H@rM�@q�@q�^@q7L@p��@o�@o|�@nȴ@n$�@m�T@m�h@mV@l�@lI�@k�F@k�@ko@j��@j��@j~�@j=q@i�@i��@iG�@i%@h��@hbN@h �@g�@g\)@fȴ@fV@f{@ep�@d�/@d�@d��@d�D@dz�@d9X@c��@co@bn�@a�#@a��@a&�@`r�@` �@_��@_�@^�@^��@^E�@]��@]/@\�D@[��@Z�@Z��@Z^5@Y�#@Y7L@X��@XA�@X �@W�;@W��@W
=@V�+@Vff@VE�@V$�@U@UO�@T��@T�D@TZ@SS�@S"�@So@So@S@R=q@Q7L@P��@P��@Pr�@PbN@PA�@P �@O�w@O�P@O+@N�R@N5?@M��@M?}@MV@L�@L�@K�
@KS�@J�@Jn�@JJ@I�^@I��@I�7@Ihs@I&�@H�9@HA�@G�;@G
=@FE�@E�T@E`B@D�@D�@Dz�@D(�@D�@D�@D�@D1@C�F@C��@Ct�@CS�@C@B��@Bn�@BM�@B-@BJ@A�#@A��@Ax�@A%@@Ĝ@@�9@@�u@@bN@@b@?K�@>V@=��@=�@<�@<�D@<z�@<j@<�@;��@;dZ@:~�@9x�@9%@8�u@8A�@81'@8 �@7�@7�@7|�@7+@6�@6��@6�+@6E�@5��@5�@5O�@5V@4�@4��@49X@3�
@3�@3o@2�H@2�!@2n�@2-@1��@1�^@1��@1x�@1G�@1%@0��@0��@0��@01'@/�;@/\)@/+@.��@.�@.ȴ@.�R@.��@.��@.V@.$�@-�@-��@-?}@,�@,j@,I�@,I�@,9X@+��@+t�@+dZ@+C�@*^5@)��@)��@)�7@)G�@(��@(��@(Ĝ@(��@(�@(bN@(A�@'�@'��@'�@'\)@'�@&��@&�y@&ȴ@&��@&{@%��@%�@%/@$��@$��@$��@$�D@$Z@$�@#�
@#��@#t�@#"�@#@#@"�@"��@"��@"M�@!��@!&�@!�@ ��@ ��@ Ĝ@ bN@ b@   @ r�@ Q�@   @�;@\)@;d@+@+@;d@��@��@�@�-@�@O�@O�@?}@?}@V@��@�@�D@Z@�@�
@dZ@S�@"�@o@@^5@-@�@��@��@��@�7@G�@&�@�`@�@1'@  @��@|�@l�@K�@��@ff@�@�h@p�@?}@`B@`B@`B@?}@�@/@��@j@I�@9X@(�@�@�m@ƨ@��@�@C�@�@��@��@��@�\@n�@J@�#@�#@��@�^@��@��@G�@��@��@�9@Q�@ �@�@�w@�@�@��@�@��@\)@�y@ȴ@�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BjBk�Bk�BjBk�Bk�Bk�BjBjBjBjBjBjBjBl�Bm�Bn�Bm�Bn�Bp�Bt�Bt�Bu�Bt�Bu�Bv�Bv�By�B|�B{�B}�B}�B~�B� B�B�PB
^5B
�?B
��BD�B{�B�Bp�BI�BC�BL�BG�BH�BG�BI�BVB[#B[#BXBZBVBdZBcTBaHB\)BaHB^5BbNBs�Bs�B\)B8RB�B
�B
q�B
hsB
cTB
P�B
'�B	�B	ɺB	�}B	�FB	��B	�?B	��B	��B	��B	ƨB	��B	�jB	��B	��B	�\B	z�B	m�B	dZB	L�B	:^B	7LB	F�B	YB	cTB	dZB	dZB	bNB	XB	G�B	[#B	hsB	|�B	{�B	�B	�VB	��B	��B	�9B	�XB	��B	ŢB	��B	��B	�B	�/B	�ZB	�`B	�B	��B	��B	��B	��B	��B	��B	��B	��B
DB
JB
	7B
+B
{B
!�B
#�B
"�B
!�B
�B
(�B
-B
.B
-B
-B
-B
1'B
/B
<jB
>wB
8RB
2-B
33B
8RB
E�B
K�B
L�B
F�B
@�B
<jB
=qB
?}B
B�B
N�B
S�B
S�B
VB
VB
T�B
R�B
S�B
T�B
W
B
YB
`BB
_;B
^5B
YB
VB
W
B
[#B
[#B
[#B
\)B
[#B
[#B
^5B
\)B
[#B
\)B
]/B
\)B
\)B
\)B
W
B
VB
YB
\)B
\)B
XB
T�B
Q�B
P�B
Q�B
R�B
S�B
S�B
R�B
Q�B
Q�B
P�B
O�B
P�B
P�B
O�B
M�B
L�B
K�B
I�B
I�B
J�B
J�B
K�B
K�B
J�B
J�B
I�B
I�B
H�B
G�B
F�B
B�B
A�B
@�B
?}B
?}B
>wB
>wB
?}B
@�B
@�B
@�B
?}B
<jB
;dB
:^B
:^B
:^B
9XB
8RB
9XB
9XB
7LB
6FB
5?B
49B
1'B
/B
1'B
5?B
49B
1'B
/B
-B
,B
)�B
(�B
'�B
&�B
%�B
(�B
)�B
'�B
&�B
%�B
$�B
#�B
"�B
"�B
!�B
 �B
!�B
#�B
#�B
"�B
 �B
�B
 �B
"�B
#�B
"�B
"�B
!�B
"�B
#�B
"�B
!�B
�B
�B
�B
�B
 �B
 �B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
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
%�B
%�B
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
)�B
)�B
)�B
)�B
)�B
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
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
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
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
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
;dB
;dB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
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
A�B
@�B
@�B
@�B
A�B
B�B
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
D�B
E�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
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
K�B
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
M�B
N�B
N�B
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
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
VB
VB
VB
VB
VB
VB
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
]/B
_;B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
cTB
cTB
cTB
dZB
dZB
dZB
e`B
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
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
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
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
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
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�1B
�+B
�+B
�1B
�+B
�+B
�+B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�=B
�=B
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�JB
�JB
�PB
�PB
�VB
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
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BZ4B[:B[:BZ4B[:B[:B[:BZ4BZ4BZ4BZ4BZ4BZ4BZ4B\@B]FB^MB]FB^MB`YBdqBdqBexBdqBexBf~Bf~Bi�Bl�Bk�Bm�Bm�Bn�Bo�Bp�B}B
M�B
��B
�~B4QBk�Bq�B`YB9oB3KB<�B7cB8iB7cB9oBE�BJ�BJ�BG�BI�BE�BTBS	BP�BK�BP�BM�BRBckBckBK�B(BBB
��B
a_B
X(B
S	B
@�B
�B	�FB	�oB	�2B	��B	��B	��B	�|B	íB	��B	�]B	�>B	�B	��B	�mB	B	j�B	]FB	TB	<�B	*B	'B	6]B	H�B	S	B	TB	TB	RB	G�B	7cB	J�B	X(B	l�B	k�B	r�B	~B	�[B	��B	��B	�B	�8B	�WB	��B	§B	��B	��B	�B	�B	�:B	�xB	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B
0B
�B
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
 �B
�B
,B
.,B
(B
!�B
"�B
(B
5WB
;|B
<�B
6]B
08B
,B
-&B
/2B
2DB
>�B
C�B
C�B
E�B
E�B
D�B
B�B
C�B
D�B
F�B
H�B
O�B
N�B
M�B
H�B
E�B
F�B
J�B
J�B
J�B
K�B
J�B
J�B
M�B
K�B
J�B
K�B
L�B
K�B
K�B
K�B
F�B
E�B
H�B
K�B
K�B
G�B
D�B
A�B
@�B
A�B
B�B
C�B
C�B
B�B
A�B
A�B
@�B
?�B
@�B
@�B
?�B
=�B
<�B
;|B
9oB
9oB
:vB
:vB
;|B
;|B
:vB
:vB
9oB
9oB
8iB
7cB
6]B
2DB
1>B
08B
/2B
/2B
.,B
.,B
/2B
08B
08B
08B
/2B
,B
+B
*B
*B
*B
)B
(B
)B
)B
'B
%�B
$�B
#�B
 �B
�B
 �B
$�B
#�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
�B
�B
�B
�B
zB
sB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
sB
sB
mB
sB
zB
zB
�B
zB
zB
mB
gB
gB
gB
gB
gB
gB
gB
mB
mB
aB
[B

UB
[B
[B
aB
aB
[B

UB
[B

UB

UB

UB

UB

UB

UB

UB
[B

UB
[B
[B
[B

UB

UB

UB

UB

UB

UB

UB

UB

UB
[B

UB
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
aB
[B
aB
aB
aB
aB
aB
aB
aB
gB
gB
gB
gB
gB
mB
gB
gB
gB
aB
aB
[B
[B
[B
aB
aB
gB
gB
gB
gB
gB
gB
gB
gB
gB
aB
[B
[B
[B
[B
[B
[B
gB
gB
gB
gB
gB
gB
mB
mB
mB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
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
%�B
%�B
'B
(B
(B
(B
(B
)B
)B
)B
)B
)B
*B
*B
+B
+B
,B
,B
,B
-&B
,B
-&B
-&B
-&B
.,B
.,B
.,B
.,B
/2B
/2B
/2B
/2B
/2B
08B
08B
08B
08B
08B
08B
08B
1>B
1>B
1>B
1>B
1>B
1>B
1>B
1>B
1>B
08B
08B
08B
1>B
2DB
1>B
1>B
1>B
2DB
2DB
2DB
2DB
3KB
3KB
3KB
3KB
3KB
3KB
4QB
5WB
7cB
7cB
7cB
7cB
8iB
8iB
9oB
8iB
9oB
:vB
:vB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
;|B
;|B
;|B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
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
A�B
A�B
A�B
A�B
A�B
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
E�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
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
J�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
N�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
RB
RB
RB
RB
RB
S	B
S	B
S	B
S	B
S	B
TB
S	B
S	B
S	B
TB
TB
TB
UB
VB
VB
VB
W"B
W"B
W"B
W"B
W"B
W"B
W"B
W"B
W"B
X(B
X(B
X(B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
Z4B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
\@B
\@B
\@B
]FB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
a_B
a_B
beB
beB
beB
beB
beB
ckB
ckB
ckB
ckB
ckB
ckB
dqB
dqB
ckB
ckB
dqB
dqB
exB
exB
exB
exB
exB
exB
exB
exB
exB
exB
f~B
f~B
f~B
f~B
g�B
g�B
g�B
f~B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
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
l�B
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
o�B
o�B
p�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
v�B
v�B
w�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
z�B
{�B
{�B
}B
}B
~B
}B
}B
~B
~B
~B
~B
B
B
B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B
�$B
�$B
�$B
�$B
�*B
�*B
�*B
�*B
�*B
�*B
�*B
�*B
�*B
�0B
�0B
�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.55 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200301110044              20200301110044  AO  ARCAADJP                                                                    20200301110044    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200301110044    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200301110044  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200301110044  QCF$                G�O�G�O�G�O�0               