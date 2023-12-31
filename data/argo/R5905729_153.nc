CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-07-05T09:00:58Z creation      
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ވ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20220705090058  20220705090058  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���CQ��1   @���y\�2@(=�E���dy�$�/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@���A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�7LA�9XA�?}A�?}A�A�A�A�A�E�A�I�A�I�A�G�A�G�A�K�A�Q�A�Q�A�O�A�O�A�O�A�VA�VA�VA�VA�ZA�\)A�\)A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�dZA�dZA�bNA�dZA�bNA�dZA�dZA�G�A�JA���A�M�AӬAė�A�=qA��A�JA��FA��A��A�t�A���A�/A��A�x�A��hA�^5A���A�E�A��PA�?}A�  A��Az�At�!Ar�/Ak��Ai�FAgx�Ab�A`VA]��A\��A\1'AX��ATZAP��AN�HAMO�AK;dAJ1AI
=AG�^AF�yAE�FAE�PAC��A?�
A<��A9VA77LA4��A3�A2A0��A/;dA-C�A,��A,�A,v�A,$�A+�A*��A)�A(jA'��A'G�A'7LA&~�A%�wA%�FA%��A$�jA$M�A$�A"�DA!?}A �A �HA ��A ĜA ��A��A�-A�PAO�A&�A�yA�RA�+A9XA=qA9XA$�A�AA��A�A��A\)AoA�RAA�7A
=A33A�A%A�A�A��AE�A��AO�A"�A�AM�AƨAp�AhsAK�A��AffA{A�
A�^A�PAdZA+A��A��AE�A�
AA��A�AdZAC�AVA��AM�A{A�A�PA�A�/A�9A��A^5A9XA��A�+A{A�A��Al�A�`AI�A�A�TAA��A�A;dAA
�/A
�RA
r�A
$�A	��A	�A	|�A	O�A��A�At�A�jAbA��A�FAhsAG�A�A��AffA �A�A��A��AdZA"�A��A�AffA �AA��A�7A\)AG�A;dA+A"�A �yA n�A �@���@��@���@�E�@��h@��D@��R@��D@��@���@�l�@�;d@�@���@�n�@�@��-@�/@�%@��j@�j@�I�@�(�@�ƨ@��T@�u@�j@�j@�Z@�Z@�bN@�Z@�Q�@�b@�5?@�V@�@�+@�=q@�&�@�@�1@�|�@�33@�R@�E�@���@��@�7@�7L@��@��@�V@�V@���@�9X@㝲@�K�@��y@��@�O�@�r�@�bN@�A�@� �@�S�@��@ް!@�~�@�V@���@�ff@�O�@�Ĝ@أ�@�j@�  @ץ�@�
=@�ȴ@ա�@�V@ԃ@�l�@ҧ�@�ff@�^5@�E�@�$�@�J@��@���@�x�@�7L@�r�@�C�@��y@·+@�V@��@�/@�Ĝ@̬@�I�@ˍP@�C�@��H@��@�@�@Ɂ@��/@��@�\)@�S�@�K�@�K�@�"�@�ȴ@�E�@��@�X@�Q�@�
=@�{@�O�@��u@�A�@�1@�"�@�~�@�x�@�A�@�  @��
@���@��F@�t�@��y@���@��7@�hs@�7L@��@��9@�1'@��w@��P@�t�@�\)@�K�@�"�@��y@�n�@�$�@���@�?}@���@��D@�I�@�b@��@�ƨ@���@�5?@�`B@��@���@��@�;d@�o@�v�@�J@�@�G�@���@��D@�Z@��F@��@�S�@��@���@��+@�J@�@�O�@��`@��@�9X@��m@�
=@��@��!@�~�@�ff@�=q@��T@�?}@���@��j@��u@�Z@� �@�1@��m@��F@�K�@�ȴ@�ff@��@�x�@�?}@��9@�A�@���@��;@���@�dZ@�"�@��!@�^5@�5?@��@�{@�J@���@��T@�@�hs@��/@�j@��@���@��;@���@�ƨ@��P@�S�@�+@��H@�5?@�{@��@��-@�?}@���@���@��u@�9X@�S�@��@���@�M�@��@���@���@��^@�x�@�/@���@��`@���@�r�@�I�@�(�@���@���@�dZ@�C�@�33@�o@�~�@�{@���@���@��@��u@�ƨ@��@�C�@���@���@�ff@��#@�hs@��@���@��@��@�A�@��@���@��
@��w@���@�\)@�;d@��+@��@���@�G�@�&�@���@��`@��9@�r�@��
@���@�dZ@�o@�M�@��-@���@�j@�(�@�1@�  @��
@���@�|�@�;d@��H@���@�5?@�@��h@��7@��@�V@�Ĝ@��9@��D@�9X@\)@~v�@~{@}O�@|Z@{�F@{o@z�!@z��@z�\@zM�@y��@yG�@y�@y%@x��@x�@x1'@w�;@w��@wl�@w�@v��@vV@u��@u�h@uV@t�@tZ@s��@sdZ@s"�@so@r~�@q�^@p�`@o�@o
=@n�@n��@n�+@nv�@nV@m�@m�-@m�@m?}@l�D@k�
@k��@kC�@ko@j�@j^5@i�^@iG�@h�9@h��@hbN@h1'@h  @g�@g�P@gK�@f�R@f$�@e��@e�@d��@dZ@d(�@cƨ@cC�@c33@co@b��@b^5@b=q@a�@aX@` �@_��@^�y@^V@^5?@^@]��@\��@\��@\Z@\�@[�m@[ƨ@[S�@[o@Z��@Z^5@Z-@Y�@Y��@Y7L@XĜ@Xr�@X �@W�@WK�@V��@V{@U@U�h@U?}@UV@T��@T�@T��@T9X@T1@S��@S�m@S��@St�@S33@Rn�@R=q@Q��@Q��@Qx�@QG�@P��@P�9@PQ�@O�@O�w@O��@O��@Ol�@N�y@Nȴ@N��@NV@N{@M�-@M�@L�j@L(�@K�F@K�@KS�@K33@K"�@J��@J�\@J-@I�#@I��@IG�@H��@H�@H �@G�@G�;@G�;@G�;@G�w@G��@Gl�@GK�@G+@F�@F��@Fv�@F@E@E�-@E`B@D��@D�@D�j@Dz�@DI�@D(�@C��@Cƨ@CdZ@BM�@B�@A��@Ax�@A7L@@�`@@��@@��@@��@@��@@�@@Q�@?�;@?K�@?
=@>�y@>��@>E�@>$�@=��@=�@=O�@<�j@<z�@<9X@<1@;��@;S�@:��@9�7@9X@97L@9�@8��@8��@81'@8  @7�w@7l�@6�y@6�@6�R@6V@6@5�@4��@4�D@4I�@3��@3��@2��@2�@1�#@1��@1�^@1�^@1�^@1��@17L@0��@0r�@0 �@/�;@/�P@/\)@.ȴ@.v�@.ff@.5?@.{@-�T@-��@-/@,j@,(�@+�m@+�F@+@*��@)��@)7L@(��@(��@(�u@(Q�@'�;@'+@&�+@&$�@%�T@%�@%/@%V@$��@$�j@$Z@$(�@#�
@#��@#t�@#33@"��@"~�@"n�@"^5@"-@!��@!�7@ Ĝ@ r�@ Q�@ 1'@   @��@�w@�@�P@|�@l�@K�@
=@�@�R@��@��@�+@V@5?@{@�T@�h@`B@?}@�@��@Z@1@�m@�@dZ@dZ@C�@@�!@M�@=q@=q@J@��@�@�#@��@��@�^@��@hs@G�@G�@%@��@Ĝ@�@1'@�@��@��@|�@\)@K�@;d@;d@+@+@
=@��@$�@@��@�@/@�@�j@�@��@I�@ƨ@��@��@��@��@�@dZ@S�@33@"�@o@o@o@@�@�H@�!@��@��@�\@n�@M�@�@J@��@�@�^@��@��@�7@�7@x�@7L@��@��@�u@  @�@��@\)@K�@;d@+@
=@�y@�@�+@$�@�@��@�-@�-@��@��@�h@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�7LA�9XA�?}A�?}A�A�A�A�A�E�A�I�A�I�A�G�A�G�A�K�A�Q�A�Q�A�O�A�O�A�O�A�VA�VA�VA�VA�ZA�\)A�\)A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�dZA�dZA�bNA�dZA�bNA�dZA�dZA�G�A�JA���A�M�AӬAė�A�=qA��A�JA��FA��A��A�t�A���A�/A��A�x�A��hA�^5A���A�E�A��PA�?}A�  A��Az�At�!Ar�/Ak��Ai�FAgx�Ab�A`VA]��A\��A\1'AX��ATZAP��AN�HAMO�AK;dAJ1AI
=AG�^AF�yAE�FAE�PAC��A?�
A<��A9VA77LA4��A3�A2A0��A/;dA-C�A,��A,�A,v�A,$�A+�A*��A)�A(jA'��A'G�A'7LA&~�A%�wA%�FA%��A$�jA$M�A$�A"�DA!?}A �A �HA ��A ĜA ��A��A�-A�PAO�A&�A�yA�RA�+A9XA=qA9XA$�A�AA��A�A��A\)AoA�RAA�7A
=A33A�A%A�A�A��AE�A��AO�A"�A�AM�AƨAp�AhsAK�A��AffA{A�
A�^A�PAdZA+A��A��AE�A�
AA��A�AdZAC�AVA��AM�A{A�A�PA�A�/A�9A��A^5A9XA��A�+A{A�A��Al�A�`AI�A�A�TAA��A�A;dAA
�/A
�RA
r�A
$�A	��A	�A	|�A	O�A��A�At�A�jAbA��A�FAhsAG�A�A��AffA �A�A��A��AdZA"�A��A�AffA �AA��A�7A\)AG�A;dA+A"�A �yA n�A �@���@��@���@�E�@��h@��D@��R@��D@��@���@�l�@�;d@�@���@�n�@�@��-@�/@�%@��j@�j@�I�@�(�@�ƨ@��T@�u@�j@�j@�Z@�Z@�bN@�Z@�Q�@�b@�5?@�V@�@�+@�=q@�&�@�@�1@�|�@�33@�R@�E�@���@��@�7@�7L@��@��@�V@�V@���@�9X@㝲@�K�@��y@��@�O�@�r�@�bN@�A�@� �@�S�@��@ް!@�~�@�V@���@�ff@�O�@�Ĝ@أ�@�j@�  @ץ�@�
=@�ȴ@ա�@�V@ԃ@�l�@ҧ�@�ff@�^5@�E�@�$�@�J@��@���@�x�@�7L@�r�@�C�@��y@·+@�V@��@�/@�Ĝ@̬@�I�@ˍP@�C�@��H@��@�@�@Ɂ@��/@��@�\)@�S�@�K�@�K�@�"�@�ȴ@�E�@��@�X@�Q�@�
=@�{@�O�@��u@�A�@�1@�"�@�~�@�x�@�A�@�  @��
@���@��F@�t�@��y@���@��7@�hs@�7L@��@��9@�1'@��w@��P@�t�@�\)@�K�@�"�@��y@�n�@�$�@���@�?}@���@��D@�I�@�b@��@�ƨ@���@�5?@�`B@��@���@��@�;d@�o@�v�@�J@�@�G�@���@��D@�Z@��F@��@�S�@��@���@��+@�J@�@�O�@��`@��@�9X@��m@�
=@��@��!@�~�@�ff@�=q@��T@�?}@���@��j@��u@�Z@� �@�1@��m@��F@�K�@�ȴ@�ff@��@�x�@�?}@��9@�A�@���@��;@���@�dZ@�"�@��!@�^5@�5?@��@�{@�J@���@��T@�@�hs@��/@�j@��@���@��;@���@�ƨ@��P@�S�@�+@��H@�5?@�{@��@��-@�?}@���@���@��u@�9X@�S�@��@���@�M�@��@���@���@��^@�x�@�/@���@��`@���@�r�@�I�@�(�@���@���@�dZ@�C�@�33@�o@�~�@�{@���@���@��@��u@�ƨ@��@�C�@���@���@�ff@��#@�hs@��@���@��@��@�A�@��@���@��
@��w@���@�\)@�;d@��+@��@���@�G�@�&�@���@��`@��9@�r�@��
@���@�dZ@�o@�M�@��-@���@�j@�(�@�1@�  @��
@���@�|�@�;d@��H@���@�5?@�@��h@��7@��@�V@�Ĝ@��9@��D@�9X@\)@~v�@~{@}O�@|Z@{�F@{o@z�!@z��@z�\@zM�@y��@yG�@y�@y%@x��@x�@x1'@w�;@w��@wl�@w�@v��@vV@u��@u�h@uV@t�@tZ@s��@sdZ@s"�@so@r~�@q�^@p�`@o�@o
=@n�@n��@n�+@nv�@nV@m�@m�-@m�@m?}@l�D@k�
@k��@kC�@ko@j�@j^5@i�^@iG�@h�9@h��@hbN@h1'@h  @g�@g�P@gK�@f�R@f$�@e��@e�@d��@dZ@d(�@cƨ@cC�@c33@co@b��@b^5@b=q@a�@aX@` �@_��@^�y@^V@^5?@^@]��@\��@\��@\Z@\�@[�m@[ƨ@[S�@[o@Z��@Z^5@Z-@Y�@Y��@Y7L@XĜ@Xr�@X �@W�@WK�@V��@V{@U@U�h@U?}@UV@T��@T�@T��@T9X@T1@S��@S�m@S��@St�@S33@Rn�@R=q@Q��@Q��@Qx�@QG�@P��@P�9@PQ�@O�@O�w@O��@O��@Ol�@N�y@Nȴ@N��@NV@N{@M�-@M�@L�j@L(�@K�F@K�@KS�@K33@K"�@J��@J�\@J-@I�#@I��@IG�@H��@H�@H �@G�@G�;@G�;@G�;@G�w@G��@Gl�@GK�@G+@F�@F��@Fv�@F@E@E�-@E`B@D��@D�@D�j@Dz�@DI�@D(�@C��@Cƨ@CdZ@BM�@B�@A��@Ax�@A7L@@�`@@��@@��@@��@@��@@�@@Q�@?�;@?K�@?
=@>�y@>��@>E�@>$�@=��@=�@=O�@<�j@<z�@<9X@<1@;��@;S�@:��@9�7@9X@97L@9�@8��@8��@81'@8  @7�w@7l�@6�y@6�@6�R@6V@6@5�@4��@4�D@4I�@3��@3��@2��@2�@1�#@1��@1�^@1�^@1�^@1��@17L@0��@0r�@0 �@/�;@/�P@/\)@.ȴ@.v�@.ff@.5?@.{@-�T@-��@-/@,j@,(�@+�m@+�F@+@*��@)��@)7L@(��@(��@(�u@(Q�@'�;@'+@&�+@&$�@%�T@%�@%/@%V@$��@$�j@$Z@$(�@#�
@#��@#t�@#33@"��@"~�@"n�@"^5@"-@!��@!�7@ Ĝ@ r�@ Q�@ 1'@   @��@�w@�@�P@|�@l�@K�@
=@�@�R@��@��@�+@V@5?@{@�T@�h@`B@?}@�@��@Z@1@�m@�@dZ@dZ@C�@@�!@M�@=q@=q@J@��@�@�#@��@��@�^@��@hs@G�@G�@%@��@Ĝ@�@1'@�@��@��@|�@\)@K�@;d@;d@+@+@
=@��@$�@@��@�@/@�@�j@�@��@I�@ƨ@��@��@��@��@�@dZ@S�@33@"�@o@o@o@@�@�H@�!@��@��@�\@n�@M�@�@J@��@�@�^@��@��@�7@�7@x�@7L@��@��@�u@  @�@��@\)@K�@;d@+@
=@�y@�@�+@$�@�@��@�-@�-@��@��@�h@�h1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�;B
�BB
�BB
�HB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�BB
�BB
�5B
�#B
��B
�RB
�B
��B
�9B
`BB
��B
��B
�B
��B
��B
ÖB
�3B
L�B
hsB
k�B
bNB
XB
33B	�B	��B
DB	�B	�-B	��B	~�B	��B	bNB	z�B	�JB	aHB	m�B	bNB	l�B	]/B	1'B	hB	�B	L�B	S�B	bNB	jB	y�B	jB	�B	�oB	�B	��B	o�B	k�B	cTB	s�B	p�B	}�B	�B	�B	�B	�JB	�'B	ĜB	ÖB	�}B	�XB	�wB	�XB	ŢB	�;B	��B
%B
1B
�B
33B
5?B
7LB
<jB
8RB
%�B
%�B
=qB
J�B
L�B
M�B
N�B
L�B
YB
cTB
dZB
jB
o�B
r�B
t�B
t�B
|�B
}�B
}�B
� B
� B
� B
~�B
z�B
u�B
x�B
w�B
t�B
w�B
w�B
�B
�B
�B
�B
�B
�B
{�B
z�B
|�B
�B
}�B
{�B
|�B
~�B
�B
�B
{�B
x�B
{�B
|�B
}�B
}�B
}�B
{�B
z�B
w�B
v�B
u�B
{�B
{�B
y�B
x�B
v�B
s�B
p�B
p�B
r�B
r�B
o�B
m�B
r�B
r�B
q�B
n�B
k�B
cTB
ZB
dZB
jB
hsB
cTB
_;B
^5B
aHB
ffB
e`B
e`B
dZB
bNB
bNB
bNB
bNB
_;B
_;B
\)B
^5B
\)B
YB
R�B
M�B
L�B
K�B
M�B
S�B
Q�B
O�B
Q�B
O�B
M�B
K�B
N�B
O�B
O�B
O�B
M�B
L�B
L�B
J�B
J�B
J�B
K�B
J�B
I�B
K�B
M�B
M�B
L�B
K�B
G�B
B�B
C�B
F�B
D�B
C�B
A�B
;dB
33B
.B
+B
9XB
C�B
B�B
A�B
A�B
?}B
>wB
=qB
=qB
<jB
>wB
=qB
<jB
<jB
9XB
49B
)�B
,B
;dB
=qB
<jB
<jB
;dB
9XB
6FB
0!B
%�B
'�B
-B
%�B
(�B
(�B
,B
.B
-B
0!B
.B
.B
0!B
2-B
2-B
1'B
2-B
33B
2-B
1'B
.B
(�B
&�B
)�B
(�B
"�B
$�B
%�B
.B
.B
,B
%�B
%�B
(�B
&�B
�B
�B
�B
�B
#�B
&�B
%�B
#�B
"�B
�B
 �B
�B
�B
�B
�B
�B
#�B
%�B
$�B
$�B
$�B
#�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
"�B
 �B
�B
�B
�B
�B
$�B
$�B
#�B
 �B
�B
�B
�B
�B
uB
bB
�B
�B
�B
�B
�B
�B
�B
�B
{B
 �B
!�B
"�B
 �B
�B
�B
�B
�B
!�B
 �B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
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
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
!�B
!�B
�B
#�B
#�B
"�B
!�B
!�B
�B
 �B
 �B
�B
!�B
"�B
 �B
�B
%�B
'�B
&�B
&�B
%�B
"�B
 �B
%�B
&�B
'�B
'�B
&�B
'�B
&�B
$�B
!�B
!�B
"�B
!�B
"�B
$�B
"�B
$�B
(�B
)�B
(�B
'�B
'�B
&�B
)�B
-B
.B
/B
/B
-B
,B
+B
&�B
&�B
'�B
,B
/B
/B
/B
.B
,B
+B
,B
)�B
&�B
/B
.B
,B
+B
,B
.B
,B
(�B
$�B
+B
1'B
.B
/B
49B
5?B
49B
1'B
1'B
2-B
49B
33B
33B
49B
49B
33B
33B
2-B
5?B
5?B
33B
.B
0!B
33B
2-B
.B
/B
-B
5?B
5?B
49B
7LB
7LB
49B
6FB
:^B
<jB
;dB
9XB
;dB
<jB
=qB
<jB
<jB
;dB
9XB
9XB
49B
8RB
8RB
;dB
>wB
>wB
>wB
<jB
:^B
7LB
<jB
;dB
8RB
49B
7LB
6FB
>wB
C�B
E�B
F�B
E�B
D�B
D�B
B�B
A�B
B�B
A�B
B�B
E�B
H�B
G�B
C�B
E�B
H�B
F�B
C�B
@�B
C�B
F�B
E�B
C�B
H�B
H�B
J�B
L�B
L�B
J�B
I�B
K�B
L�B
M�B
L�B
K�B
K�B
L�B
L�B
M�B
L�B
J�B
L�B
K�B
M�B
L�B
L�B
M�B
L�B
L�B
N�B
N�B
K�B
I�B
J�B
J�B
O�B
S�B
T�B
T�B
T�B
T�B
R�B
S�B
S�B
R�B
P�B
Q�B
T�B
T�B
VB
T�B
R�B
R�B
T�B
VB
YB
XB
XB
YB
XB
YB
XB
VB
VB
XB
XB
W
B
XB
[#B
ZB
[#B
]/B
\)B
[#B
[#B
\)B
ZB
XB
T�B
ZB
ZB
[#B
^5B
^5B
\)B
\)B
^5B
_;B
_;B
_;B
`BB
^5B
_;B
_;B
aHB
aHB
aHB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
_;B
aHB
cTB
dZB
cTB
e`B
e`B
e`B
dZB
dZB
ffB
ffB
ffB
dZB
dZB
cTB
aHB
e`B
e`B
e`B
ffB
ffB
e`B
e`B
e`B
ffB
gmB
hsB
hsB
gmB
e`B
hsB
gmB
ffB
e`B
e`B
cTB
ffB
e`B
gmB
iyB
iyB
jB
jB
hsB
iyB
hsB
hsB
jB
hsB
hsB
jB
jB
m�B
n�B
n�B
n�B
m�B
m�B
l�B
l�B
l�B
k�B
l�B
l�B
k�B
m�B
n�B
l�B
l�B
n�B
n�B
m�B
n�B
n�B
m�B
m�B
k�B
iyB
n�B
o�B
o�B
p�B
p�B
r�B
s�B
r�B
q�B
o�B
o�B
n�B
n�B
q�B
r�B
q�B
q�B
r�B
q�B
q�B
r�B
p�B
r�B
r�B
r�B
q�B
q�B
n�B
m�B
u�B
u�B
v�B
u�B
t�B
s�B
u�B
u�B
u�B
t�B
w�B
w�B
u�B
u�B
t�B
u�B
v�B
w�B
v�B
v�B
u�B
v�B
z�B
{�B
|�B
{�B
{�B
z�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
z�B
{�B
~�B
}�B
}�B
}�B
}�B
{�B
z�B
~�B
~�B
~�B
|�B
}�B
{�B
}�B
� B
� B
�B
~�B
}�B
{�B
|�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�B
�+B
�%B
�+B
�+B
�+B
�%B
�%B
�%B
�7B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�1B
�7B
�1B
�1B
�7B
�1B
�1B
�7B
�=B
�=B
�DB
�DB
�JB
�JB
�JB
�PB
�JB
�DB
�7B
�7B
�=B
�JB
�JB
�DB
�DB
�JB
�PB
�PB
�JB
�DB
�\B
�bB
�hB
�bB
�bB
�\B
�bB
�bB
�hB
�hB
�hB
�hB
�hB
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
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�bB
�hB
�hB
�hB
�\B
�oB
�oB
�oB
�{B
�{B
�{B
�uB
�uB
�uB
�uB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�;B
�BB
�BB
�HB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�BB
�BB
�5B
�#B
��B
�RB
�B
��B
�9B
`BB
��B
��B
�B
��B
��B
ÖB
�3B
L�B
hsB
k�B
bNB
XB
33B	�B	��B
DB	�B	�-B	��B	~�B	��B	bNB	z�B	�JB	aHB	m�B	bNB	l�B	]/B	1'B	hB	�B	L�B	S�B	bNB	jB	y�B	jB	�B	�oB	�B	��B	o�B	k�B	cTB	s�B	p�B	}�B	�B	�B	�B	�JB	�'B	ĜB	ÖB	�}B	�XB	�wB	�XB	ŢB	�;B	��B
%B
1B
�B
33B
5?B
7LB
<jB
8RB
%�B
%�B
=qB
J�B
L�B
M�B
N�B
L�B
YB
cTB
dZB
jB
o�B
r�B
t�B
t�B
|�B
}�B
}�B
� B
� B
� B
~�B
z�B
u�B
x�B
w�B
t�B
w�B
w�B
�B
�B
�B
�B
�B
�B
{�B
z�B
|�B
�B
}�B
{�B
|�B
~�B
�B
�B
{�B
x�B
{�B
|�B
}�B
}�B
}�B
{�B
z�B
w�B
v�B
u�B
{�B
{�B
y�B
x�B
v�B
s�B
p�B
p�B
r�B
r�B
o�B
m�B
r�B
r�B
q�B
n�B
k�B
cTB
ZB
dZB
jB
hsB
cTB
_;B
^5B
aHB
ffB
e`B
e`B
dZB
bNB
bNB
bNB
bNB
_;B
_;B
\)B
^5B
\)B
YB
R�B
M�B
L�B
K�B
M�B
S�B
Q�B
O�B
Q�B
O�B
M�B
K�B
N�B
O�B
O�B
O�B
M�B
L�B
L�B
J�B
J�B
J�B
K�B
J�B
I�B
K�B
M�B
M�B
L�B
K�B
G�B
B�B
C�B
F�B
D�B
C�B
A�B
;dB
33B
.B
+B
9XB
C�B
B�B
A�B
A�B
?}B
>wB
=qB
=qB
<jB
>wB
=qB
<jB
<jB
9XB
49B
)�B
,B
;dB
=qB
<jB
<jB
;dB
9XB
6FB
0!B
%�B
'�B
-B
%�B
(�B
(�B
,B
.B
-B
0!B
.B
.B
0!B
2-B
2-B
1'B
2-B
33B
2-B
1'B
.B
(�B
&�B
)�B
(�B
"�B
$�B
%�B
.B
.B
,B
%�B
%�B
(�B
&�B
�B
�B
�B
�B
#�B
&�B
%�B
#�B
"�B
�B
 �B
�B
�B
�B
�B
�B
#�B
%�B
$�B
$�B
$�B
#�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
"�B
 �B
�B
�B
�B
�B
$�B
$�B
#�B
 �B
�B
�B
�B
�B
uB
bB
�B
�B
�B
�B
�B
�B
�B
�B
{B
 �B
!�B
"�B
 �B
�B
�B
�B
�B
!�B
 �B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
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
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
!�B
!�B
�B
#�B
#�B
"�B
!�B
!�B
�B
 �B
 �B
�B
!�B
"�B
 �B
�B
%�B
'�B
&�B
&�B
%�B
"�B
 �B
%�B
&�B
'�B
'�B
&�B
'�B
&�B
$�B
!�B
!�B
"�B
!�B
"�B
$�B
"�B
$�B
(�B
)�B
(�B
'�B
'�B
&�B
)�B
-B
.B
/B
/B
-B
,B
+B
&�B
&�B
'�B
,B
/B
/B
/B
.B
,B
+B
,B
)�B
&�B
/B
.B
,B
+B
,B
.B
,B
(�B
$�B
+B
1'B
.B
/B
49B
5?B
49B
1'B
1'B
2-B
49B
33B
33B
49B
49B
33B
33B
2-B
5?B
5?B
33B
.B
0!B
33B
2-B
.B
/B
-B
5?B
5?B
49B
7LB
7LB
49B
6FB
:^B
<jB
;dB
9XB
;dB
<jB
=qB
<jB
<jB
;dB
9XB
9XB
49B
8RB
8RB
;dB
>wB
>wB
>wB
<jB
:^B
7LB
<jB
;dB
8RB
49B
7LB
6FB
>wB
C�B
E�B
F�B
E�B
D�B
D�B
B�B
A�B
B�B
A�B
B�B
E�B
H�B
G�B
C�B
E�B
H�B
F�B
C�B
@�B
C�B
F�B
E�B
C�B
H�B
H�B
J�B
L�B
L�B
J�B
I�B
K�B
L�B
M�B
L�B
K�B
K�B
L�B
L�B
M�B
L�B
J�B
L�B
K�B
M�B
L�B
L�B
M�B
L�B
L�B
N�B
N�B
K�B
I�B
J�B
J�B
O�B
S�B
T�B
T�B
T�B
T�B
R�B
S�B
S�B
R�B
P�B
Q�B
T�B
T�B
VB
T�B
R�B
R�B
T�B
VB
YB
XB
XB
YB
XB
YB
XB
VB
VB
XB
XB
W
B
XB
[#B
ZB
[#B
]/B
\)B
[#B
[#B
\)B
ZB
XB
T�B
ZB
ZB
[#B
^5B
^5B
\)B
\)B
^5B
_;B
_;B
_;B
`BB
^5B
_;B
_;B
aHB
aHB
aHB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
_;B
aHB
cTB
dZB
cTB
e`B
e`B
e`B
dZB
dZB
ffB
ffB
ffB
dZB
dZB
cTB
aHB
e`B
e`B
e`B
ffB
ffB
e`B
e`B
e`B
ffB
gmB
hsB
hsB
gmB
e`B
hsB
gmB
ffB
e`B
e`B
cTB
ffB
e`B
gmB
iyB
iyB
jB
jB
hsB
iyB
hsB
hsB
jB
hsB
hsB
jB
jB
m�B
n�B
n�B
n�B
m�B
m�B
l�B
l�B
l�B
k�B
l�B
l�B
k�B
m�B
n�B
l�B
l�B
n�B
n�B
m�B
n�B
n�B
m�B
m�B
k�B
iyB
n�B
o�B
o�B
p�B
p�B
r�B
s�B
r�B
q�B
o�B
o�B
n�B
n�B
q�B
r�B
q�B
q�B
r�B
q�B
q�B
r�B
p�B
r�B
r�B
r�B
q�B
q�B
n�B
m�B
u�B
u�B
v�B
u�B
t�B
s�B
u�B
u�B
u�B
t�B
w�B
w�B
u�B
u�B
t�B
u�B
v�B
w�B
v�B
v�B
u�B
v�B
z�B
{�B
|�B
{�B
{�B
z�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
z�B
{�B
~�B
}�B
}�B
}�B
}�B
{�B
z�B
~�B
~�B
~�B
|�B
}�B
{�B
}�B
� B
� B
�B
~�B
}�B
{�B
|�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�B
�B
�B
�+B
�%B
�+B
�+B
�+B
�%B
�%B
�%B
�7B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�1B
�7B
�1B
�1B
�7B
�1B
�1B
�7B
�=B
�=B
�DB
�DB
�JB
�JB
�JB
�PB
�JB
�DB
�7B
�7B
�=B
�JB
�JB
�DB
�DB
�JB
�PB
�PB
�JB
�DB
�\B
�bB
�hB
�bB
�bB
�\B
�bB
�bB
�hB
�hB
�hB
�hB
�hB
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
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�bB
�hB
�hB
�hB
�\B
�oB
�oB
�oB
�{B
�{B
�{B
�uB
�uB
�uB
�uB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220705090058                              AO  ARCAADJP                                                                    20220705090058    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220705090058  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220705090058  QCF$                G�O�G�O�G�O�0               