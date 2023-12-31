CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-06-15T09:01:09Z creation      
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
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20220615090109  20220615090109  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���"���1   @��Խ�9�@'���l�D�d}����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB���B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB���B�  B�  B�  B���B���B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�  A�A�  A�  A�A�A�A�A�%A�1A�1A�%A�A�A�A�%A�1A�1A�
=A�
=A�JA�JA�JA�VA�VA�
=A�
=A�JA�JA�A���A��A��TA��A���A���A���A�K�A�+A��HA�`BA��A��;A�ȴA�33A��A�?}A��#A�A�A�/A���A��#A�M�A���A���A�S�A��A�VA�r�A��!A�l�A���A�  A�A���A|z�AvffAlVAe�Ab��A]t�AW�PATAL�AJ  AH�/AF�AF�uAF1AE�hAE`BAD��ADA�AC�
AAG�A=A<$�A8�+A8{A5��A2z�A1�TA1�A/C�A.�9A/7LA/+A.�!A/;dA/oA.�A.=qA,�A+��A*��A)p�A(E�A'x�A&�A&1'A%��A%�wA&~�A&��A%�TA%��A%�^A%�
A%��A%A$�9A#"�A"z�A"ZA"VA!�A �A �!A {A�#A�-A7LA��A��Av�A=qA�FA�A�yA�+A��A��A�A
=A�A�A&�AXA�RA�hA
=A�A$�A��AXAoA�jA��AbA  A�;A��A�-At�A"�A�+A�A�wA��A��A�A/A`BA7LA�yA�Az�AbNA1AAG�A�/A�uA1'A�TA�^Al�A�`A�A��A-A�A�^A��A�7AS�A"�A
�HA
��A
Q�A
A	��A	��A	XA	�A�jAffA�;A��A�AK�A&�AA��A�RA��A�+AZAJA33AĜA(�AA��A�A�mA�FAXA�yA�!A��A��AffA5?A$�A��A�AO�A ��A �RA �A M�@��@���@��y@���@�E�@��7@��@��@�j@�Q�@�1'@��
@�C�@���@�J@��7@�V@�r�@��m@��P@��!@�`B@��@�1'@�t�@�!@��T@�`B@��/@�t�@�!@�ff@�V@�-@�@��T@��#@��^@���@�ȴ@�=q@�^@陚@�x�@�?}@�bN@�|�@��@��@�^5@�x�@�V@�@�bN@�1@�w@�K�@�V@�@��@��@�?}@�V@���@�z�@��@ޟ�@�p�@���@� �@���@���@���@ۥ�@�v�@ف@��@�Ĝ@�bN@ו�@�v�@թ�@�Ĝ@�(�@�ƨ@�t�@�dZ@�K�@�o@�v�@ѩ�@��@�b@�t�@��y@�=q@�X@�r�@�b@˥�@��y@�$�@ə�@�p�@���@�9X@Ə\@�5?@�J@���@��T@��T@��#@š�@ģ�@�1@��m@��
@�ƨ@å�@Ý�@�t�@��@�@�-@��-@���@�j@�1'@��w@�o@���@���@�^5@��@���@�\)@�$�@�J@�x�@��@�9X@��m@��w@�"�@��@���@�n�@��@��@�Ĝ@��@���@�dZ@�S�@�C�@�+@���@�n�@��@���@�x�@�&�@��@�b@��;@�o@���@��@��@�/@�z�@��@��!@�=q@�5?@���@�z�@���@���@�o@��+@�ff@�E�@�-@�{@���@�hs@�7L@��@�%@���@���@���@�o@�ȴ@��\@�v�@�-@���@�7L@���@�j@�1'@��m@���@�l�@�C�@�@���@���@��\@�n�@�=q@��T@�O�@��`@��9@�r�@�ƨ@�;d@���@��+@�^5@�M�@�=q@�J@���@��-@���@��h@�G�@�&�@���@��;@��@�t�@�dZ@�S�@�C�@�"�@���@��@��@�G�@�%@���@��/@���@��j@��u@�bN@��@�ƨ@�|�@��@���@���@�M�@��@��#@���@��7@�`B@�O�@���@�j@�9X@�1@���@�l�@��+@��@��7@�V@�r�@�9X@�1@��@�|�@�K�@�
=@���@�ff@�$�@���@�hs@�%@��`@��/@���@�(�@��
@���@�|�@�"�@�~�@�E�@��@�@��@��@���@�`B@���@�bN@�(�@��@�1@�  @��m@�l�@�S�@�o@�E�@�@���@�?}@���@���@�bN@�1'@��@�  @�w@��@
=@~�@~�+@~$�@}��@}`B@}V@{��@z��@y��@xA�@xb@w��@v�y@v5?@u��@u�@u?}@t��@t�j@tZ@t�@s�m@sƨ@sƨ@sƨ@s�F@s�@sC�@s"�@r��@r=q@q�^@q�^@q7L@pr�@o�@oK�@nȴ@n5?@m�-@m`B@l�j@lI�@k�m@k��@kdZ@ko@j~�@i��@ix�@i%@h�u@hA�@g�@g�P@g�@f�+@f{@e`B@eV@dI�@cdZ@c@b�H@b��@bJ@a��@a�7@a7L@a�@`��@`�u@`r�@` �@_��@^v�@]p�@]V@\Z@[�m@["�@Z�!@Z~�@Y��@Y�@Y��@YX@Y7L@Y7L@Y&�@X��@XĜ@X�9@XA�@Wl�@V��@V�R@Vff@U��@UV@T��@Tz�@TZ@S��@S��@S33@So@R��@R-@Q��@Q�7@QG�@P�@PQ�@P �@O��@N�y@N5?@N@Mp�@L�@LZ@K�m@K��@K�@KdZ@K"�@J��@Jn�@I�@I�^@I�7@H�9@G�w@G�P@G+@F��@Fȴ@F��@F��@FE�@F{@E�T@E@E�-@E�@E`B@EO�@EV@D��@D��@Dj@C�m@C�
@Cƨ@C�@C33@Bn�@A��@A�7@AX@A%@@Ĝ@@Q�@@b@?�@?��@?��@?\)@>��@>�R@>�+@>ff@>E�@>{@=@=?}@<�D@<1@;�m@;��@;�@;�@;�@;�@;�@;t�@;S�@:�@:�!@:~�@:-@9�#@9��@8��@8bN@8  @7�w@7|�@7;d@7+@7
=@6ȴ@6��@6E�@5@5��@5`B@5?}@4��@4�@41@3��@3S�@3"�@2�H@2~�@2M�@2-@1��@1�@1�#@1�^@1hs@1�@0�9@0 �@/�w@/��@/�P@/�P@/K�@.��@.�@.ȴ@.v�@-@-��@-�@-p�@-`B@-`B@-`B@-?}@-/@-V@,�@,�D@,j@,9X@+ƨ@+"�@*�!@*��@*=q@)��@)�#@)�@)��@)��@)G�@)7L@)7L@(��@(�u@(r�@(Q�@( �@'�@'�;@'�w@'|�@'l�@'\)@'\)@'�@&�y@&�+@&E�@%�-@%?}@$�/@$�@$�D@$z�@$j@$j@$j@$j@$�@#"�@"�!@"~�@"M�@"J@!�^@!�7@!hs@!7L@!&�@ ��@ �9@ �u@ �u@ �@ bN@ 1'@  �@ b@   @�;@�w@�@|�@\)@;d@ȴ@E�@$�@�T@�-@�h@�@`B@/@�j@�@(�@��@��@��@�m@�F@��@�@t�@S�@33@"�@"�@"�@@��@n�@^5@-@J@�@��@�7@��@��@��@r�@r�@bN@bN@Q�@1'@�@��@|�@�y@�R@v�@V@5?@$�@�@��@�@O�@�/@I�@1@1@�
@ƨ@S�@C�@"�@�@��@=q@�#@�^@�^@�^@��@��@hs@�`@r�@ �@  @  @�@|�@\)@K�@�@
=@�R@v�@V@E�@$�@@�@�@�@�T@��@`B@O�@/@�@��@��@��@�j@�D@Z@9X@�@1@��@�m@�
1111111111111111111111111111111111111111111111111111144111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�  A�A�  A�  A�A�A�A�A�%A�1A�1A�%A�A�A�A�%A�1A�1A�
=A�
=A�JA�JA�JA�VA�VA�
=A�
=A�JA�JA�A���A��A��TA��A���A���A���A�K�A�+A��HA�`BA��A��;A�ȴA�33A��A�?}A��#A�A�A�/A���A��#A�M�A���A���A�S�A��A�VA�r�A��!A�l�A���A�  A�A���A|z�AvffAlVAe�Ab��A]t�AW�PATAL�AJ  AH�/AF�AF�uAF1AE�hAE`BAD��ADA�AC�
AAG�A=A<$�A8�+A8{A5��A2z�A1�TA1�A/C�A.�9A/7LA/+A.�!A/;dA/oA.�A.=qA,�A+��A*��A)p�A(E�A'x�A&�A&1'A%��A%�wA&~�A&��A%�TA%��A%�^A%�
A%��A%A$�9A#"�A"z�A"ZA"VA!�A �A �!A {A�#A�-A7LA��A��Av�A=qA�FA�A�yA�+A��A��A�A
=A�A�A&�AXA�RA�hA
=A�A$�A��AXAoA�jA��AbA  A�;A��A�-At�A"�A�+A�A�wA��A��A�A/A`BA7LA�yA�Az�AbNA1AAG�A�/A�uA1'A�TA�^Al�A�`A�A��A-A�A�^A��A�7AS�A"�A
�HA
��A
Q�A
A	��A	��A	XA	�A�jAffA�;A��A�AK�A&�AA��A�RA��A�+AZAJA33AĜA(�AA��A�A�mA�FAXA�yA�!A��A��AffA5?A$�A��A�AO�A ��A �RA �A M�@��@���@��y@���@�E�@��7@��@��@�j@�Q�@�1'@��
@�C�@���@�J@��7@�V@�r�@��m@��P@��!@�`B@��@�1'@�t�@�!@��T@�`B@��/@�t�@�!@�ff@�V@�-@�@��T@��#@��^@���@�ȴ@�=q@�^@陚@�x�@�?}@�bN@�|�@��@��@�^5@�x�@�V@�@�bN@�1@�w@�K�@�V@�@��@��@�?}@�V@���@�z�@��@ޟ�@�p�@���@� �@���@���@���@ۥ�@�v�@ف@��@�Ĝ@�bN@ו�@�v�@թ�@�Ĝ@�(�@�ƨ@�t�@�dZ@�K�@�o@�v�@ѩ�@��@�b@�t�@��y@�=q@�X@�r�@�b@˥�@��y@�$�@ə�@�p�@���@�9X@Ə\@�5?@�J@���@��T@��T@��#@š�@ģ�@�1@��m@��
@�ƨ@å�@Ý�@�t�@��@�@�-@��-@���@�j@�1'@��w@�o@���@���@�^5@��@���@�\)@�$�@�J@�x�@��@�9X@��m@��w@�"�@��@���@�n�@��@��@�Ĝ@��@���@�dZ@�S�@�C�@�+@���@�n�@��@���@�x�@�&�@��@�b@��;@�o@���@��@��@�/@�z�@��@��!@�=q@�5?@���@�z�@���@���@�o@��+@�ff@�E�@�-@�{@���@�hs@�7L@��@�%@���@���@���@�o@�ȴ@��\@�v�@�-@���@�7L@���@�j@�1'@��m@���@�l�@�C�@�@���@���@��\@�n�@�=q@��T@�O�@��`@��9@�r�@�ƨ@�;d@���@��+@�^5@�M�@�=q@�J@���@��-@���@��h@�G�@�&�@���@��;@��@�t�@�dZ@�S�@�C�@�"�@���@��@��@�G�@�%@���@��/@���@��j@��u@�bN@��@�ƨ@�|�@��@���@���@�M�@��@��#@���@��7@�`B@�O�@���@�j@�9X@�1@���@�l�@��+@��@��7@�V@�r�@�9X@�1@��@�|�@�K�@�
=@���@�ff@�$�@���@�hs@�%@��`@��/@���@�(�@��
@���@�|�@�"�@�~�@�E�@��@�@��@��@���@�`B@���@�bN@�(�@��@�1@�  @��m@�l�@�S�@�o@�E�@�@���@�?}@���@���@�bN@�1'@��@�  @�w@��@
=@~�@~�+@~$�@}��@}`B@}V@{��@z��@y��@xA�@xb@w��@v�y@v5?@u��@u�@u?}@t��@t�j@tZ@t�@s�m@sƨ@sƨ@sƨ@s�F@s�@sC�@s"�@r��@r=q@q�^@q�^@q7L@pr�@o�@oK�@nȴ@n5?@m�-@m`B@l�j@lI�@k�m@k��@kdZ@ko@j~�@i��@ix�@i%@h�u@hA�@g�@g�P@g�@f�+@f{@e`B@eV@dI�@cdZ@c@b�H@b��@bJ@a��@a�7@a7L@a�@`��@`�u@`r�@` �@_��@^v�@]p�@]V@\Z@[�m@["�@Z�!@Z~�@Y��@Y�@Y��@YX@Y7L@Y7L@Y&�@X��@XĜ@X�9@XA�@Wl�@V��@V�R@Vff@U��@UV@T��@Tz�@TZ@S��@S��@S33@So@R��@R-@Q��@Q�7@QG�@P�@PQ�@P �@O��@N�y@N5?@N@Mp�@L�@LZ@K�m@K��@K�@KdZ@K"�@J��@Jn�@I�@I�^@I�7@H�9@G�w@G�P@G+@F��@Fȴ@F��@F��@FE�@F{@E�T@E@E�-@E�@E`B@EO�@EV@D��@D��@Dj@C�m@C�
@Cƨ@C�@C33@Bn�@A��@A�7@AX@A%@@Ĝ@@Q�@@b@?�@?��@?��@?\)@>��@>�R@>�+@>ff@>E�@>{@=@=?}@<�D@<1@;�m@;��@;�@;�@;�@;�@;�@;t�@;S�@:�@:�!@:~�@:-@9�#@9��@8��@8bN@8  @7�w@7|�@7;d@7+@7
=@6ȴ@6��@6E�@5@5��@5`B@5?}@4��@4�@41@3��@3S�@3"�@2�H@2~�@2M�@2-@1��@1�@1�#@1�^@1hs@1�@0�9@0 �@/�w@/��@/�P@/�P@/K�@.��@.�@.ȴ@.v�@-@-��@-�@-p�@-`B@-`B@-`B@-?}@-/@-V@,�@,�D@,j@,9X@+ƨ@+"�@*�!@*��@*=q@)��@)�#@)�@)��@)��@)G�@)7L@)7L@(��@(�u@(r�@(Q�@( �@'�@'�;@'�w@'|�@'l�@'\)@'\)@'�@&�y@&�+@&E�@%�-@%?}@$�/@$�@$�D@$z�@$j@$j@$j@$j@$�@#"�@"�!@"~�@"M�@"J@!�^@!�7@!hs@!7L@!&�@ ��@ �9@ �u@ �u@ �@ bN@ 1'@  �@ b@   @�;@�w@�@|�@\)@;d@ȴ@E�@$�@�T@�-@�h@�@`B@/@�j@�@(�@��@��@��@�m@�F@��@�@t�@S�@33@"�@"�@"�@@��@n�@^5@-@J@�@��@�7@��@��@��@r�@r�@bN@bN@Q�@1'@�@��@|�@�y@�R@v�@V@5?@$�@�@��@�@O�@�/@I�@1@1@�
@ƨ@S�@C�@"�@�@��@=q@�#@�^@�^@�^@��@��@hs@�`@r�@ �@  @  @�@|�@\)@K�@�@
=@�R@v�@V@E�@$�@@�@�@�@�T@��@`B@O�@/@�@��@��@��@�j@�D@Z@9X@�@1@��@�m@�
1111111111111111111111111111111111111111111111111111144111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
r�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
u�B
w�B
z�B
{�B
r�B
YB
0!B
,B
C�B	��B
2-B
&�B
2-B
�DB
��B
�dB
�B
z�B
B�B
|�B
�}B
��B
��B
�!B
v�B
�uB
�3B
��B
��B
1'B	��B	��B	ÖB	y�B	z�B	z�B	cTB	T�B	9XB	aHB	/B	B		7B��B��B	�B	
=B	$�B	"�B	�B	�B	bB	B��B��B��B��B�B�B�qB�}B��B	B��B	�B	M�B	P�B	N�B	\)B	�B	�7B	�PB	��B	��B	��B	��B	�B	�jB	��B	��B	�5B	�yB	��B
JB
B
hB
�B
 �B
!�B
!�B
�B
hB
 �B
+B
+B
!�B
$�B
2-B
,B
33B
6FB
5?B
<jB
>wB
=qB
A�B
>wB
=qB
E�B
B�B
?}B
O�B
M�B
ZB
aHB
`BB
`BB
^5B
T�B
F�B
Q�B
R�B
S�B
VB
[#B
[#B
ZB
cTB
`BB
gmB
jB
k�B
o�B
m�B
k�B
dZB
e`B
hsB
q�B
k�B
ffB
o�B
t�B
r�B
q�B
l�B
r�B
o�B
jB
k�B
gmB
e`B
ffB
dZB
cTB
bNB
]/B
VB
H�B
J�B
L�B
VB
XB
ZB
YB
VB
W
B
VB
VB
S�B
T�B
VB
T�B
S�B
Q�B
O�B
N�B
L�B
Q�B
VB
S�B
S�B
T�B
S�B
T�B
S�B
Q�B
N�B
J�B
B�B
F�B
H�B
P�B
R�B
R�B
P�B
M�B
I�B
J�B
M�B
P�B
P�B
N�B
M�B
N�B
L�B
H�B
G�B
F�B
M�B
L�B
I�B
G�B
H�B
E�B
G�B
H�B
D�B
F�B
F�B
H�B
H�B
G�B
D�B
B�B
A�B
@�B
>wB
>wB
<jB
;dB
:^B
5?B
/B
8RB
49B
33B
1'B
0!B
33B
1'B
)�B
/B
5?B
7LB
6FB
6FB
6FB
49B
0!B
'�B
�B
-B
1'B
49B
49B
1'B
,B
)�B
.B
.B
-B
(�B
+B
-B
-B
,B
+B
(�B
%�B
(�B
.B
-B
)�B
)�B
(�B
%�B
!�B
�B
�B
!�B
#�B
%�B
(�B
&�B
#�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
"�B
#�B
#�B
#�B
"�B
�B
�B
�B
"�B
"�B
"�B
"�B
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
�B
�B
�B
�B
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
�B
�B
#�B
#�B
"�B
!�B
 �B
 �B
#�B
"�B
!�B
!�B
�B
!�B
�B
 �B
�B
!�B
"�B
�B
�B
�B
!�B
$�B
 �B
�B
!�B
&�B
$�B
$�B
)�B
)�B
)�B
)�B
(�B
'�B
)�B
)�B
+B
(�B
%�B
#�B
$�B
)�B
+B
,B
)�B
(�B
(�B
+B
+B
,B
,B
-B
-B
.B
-B
.B
/B
/B
.B
-B
,B
)�B
+B
-B
,B
(�B
+B
-B
0!B
1'B
2-B
1'B
1'B
0!B
2-B
2-B
1'B
/B
/B
-B
+B
2-B
2-B
49B
49B
33B
2-B
0!B
.B
/B
49B
5?B
6FB
6FB
6FB
6FB
5?B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
9XB
9XB
7LB
8RB
9XB
6FB
5?B
8RB
8RB
6FB
6FB
33B
5?B
9XB
8RB
8RB
<jB
=qB
=qB
;dB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
>wB
@�B
@�B
?}B
=qB
>wB
A�B
A�B
?}B
>wB
B�B
D�B
D�B
D�B
D�B
C�B
@�B
>wB
D�B
E�B
G�B
G�B
F�B
E�B
B�B
E�B
C�B
@�B
D�B
H�B
G�B
H�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
K�B
L�B
K�B
K�B
J�B
J�B
I�B
F�B
F�B
J�B
F�B
O�B
M�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
O�B
P�B
O�B
N�B
N�B
P�B
N�B
L�B
O�B
N�B
O�B
P�B
P�B
Q�B
P�B
R�B
S�B
S�B
T�B
S�B
S�B
R�B
T�B
T�B
VB
VB
VB
VB
T�B
T�B
VB
T�B
W
B
T�B
VB
YB
[#B
ZB
YB
ZB
\)B
\)B
\)B
\)B
[#B
\)B
ZB
XB
W
B
W
B
\)B
[#B
]/B
]/B
^5B
`BB
`BB
bNB
bNB
aHB
cTB
cTB
cTB
bNB
bNB
aHB
_;B
^5B
aHB
bNB
bNB
`BB
aHB
cTB
e`B
e`B
dZB
dZB
dZB
e`B
dZB
dZB
dZB
ffB
e`B
cTB
ffB
gmB
e`B
cTB
e`B
hsB
ffB
ffB
hsB
iyB
jB
k�B
k�B
jB
iyB
iyB
iyB
jB
jB
hsB
hsB
l�B
l�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
n�B
n�B
n�B
m�B
m�B
o�B
o�B
m�B
m�B
k�B
m�B
p�B
p�B
o�B
o�B
o�B
q�B
r�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
q�B
p�B
o�B
p�B
q�B
u�B
t�B
u�B
v�B
v�B
v�B
u�B
t�B
t�B
s�B
s�B
t�B
s�B
r�B
r�B
q�B
r�B
t�B
u�B
v�B
v�B
w�B
w�B
u�B
v�B
u�B
u�B
x�B
w�B
w�B
v�B
v�B
u�B
w�B
x�B
y�B
x�B
x�B
z�B
z�B
z�B
{�B
{�B
z�B
y�B
x�B
y�B
y�B
z�B
|�B
}�B
}�B
|�B
{�B
|�B
|�B
{�B
z�B
~�B
~�B
� B
� B
� B
� B
~�B
~�B
~�B
}�B
|�B
}�B
}�B
|�B
|�B
}�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�%B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�7B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�DB
�=B
�=B
�=B
�=B
�1B
�1B
�DB
�=B
�DB
�DB
�DB
�DB
�=B
�=B
�DB
�=B
�JB
�VB
�PB
�PB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�PB
�JB
�JB
�PB
�PB
�PB
�VB
�PB
�PB
�PB
�JB
�VB
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�VB
�VB
�VB
�PB
�\B
�bB
�hB
�bB
�bB
�bB
�bB
�hB
�bB
�\B
�\B
�oB
�uB
�oB
�uB
�oB
�uB
�uB
�uB
�oB
�oB
�uB
��B
��B
��B
��B
��B
�{B
�uB
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
��1111111111111111111111111111111111111111111111111111144111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
r�B
q�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
s�B
u�B
w�B
z�B
{�B
r�B
YB
0!B
,B
C�B	��B
2-B
&�B
2-B
�DB
��B
�dB
�B
z�B
B�B
|�B
�}B
��B
��B
�!B
v�B
�uB
�3B
��B
��B
1'B	��B	��B	ÖB	y�B	z�B	z�B	cTB	T�B	9XB	aHB	/B	B		7B��B��B	�B	
=B	$�B	"�B	�B	�B	bB	B��B��B��B��B�B�B�qB�}B��B	B��B	�B	M�B	P�B	N�B	\)B	�B	�7B	�PB	��B	��B	��B	��B	�B	�jB	��B	��B	�5B	�yB	��B
JB
B
hB
�B
 �B
!�B
!�B
�B
hB
 �B
+B
+B
!�B
$�B
2-B
,B
33B
6FB
5?B
<jB
>wB
=qB
A�B
>wB
=qB
E�B
B�B
?}B
O�B
M�B
ZB
aHB
`BB
`BB
^5B
T�B
F�B
Q�B
R�B
S�B
VB
[#B
[#B
ZB
cTB
`BB
gmB
jB
k�B
o�B
m�B
k�B
dZB
e`B
hsB
q�B
k�B
ffB
o�B
t�B
r�B
q�B
l�B
r�B
o�B
jB
k�B
gmB
e`B
ffB
dZB
cTB
bNB
]/B
VB
H�B
J�B
L�B
VB
XB
ZB
YB
VB
W
B
VB
VB
S�B
T�B
VB
T�B
S�B
Q�B
O�B
N�B
L�B
Q�B
VB
S�B
S�B
T�B
S�B
T�B
S�B
Q�B
N�B
J�B
B�B
F�B
H�B
P�B
R�B
R�B
P�B
M�B
I�B
J�B
M�B
P�B
P�B
N�B
M�B
N�B
L�B
H�B
G�B
F�B
M�B
L�B
I�B
G�B
H�B
E�B
G�B
H�B
D�B
F�B
F�B
H�B
H�B
G�B
D�B
B�B
A�B
@�B
>wB
>wB
<jB
;dB
:^B
5?B
/B
8RB
49B
33B
1'B
0!B
33B
1'B
)�B
/B
5?B
7LB
6FB
6FB
6FB
49B
0!B
'�B
�B
-B
1'B
49B
49B
1'B
,B
)�B
.B
.B
-B
(�B
+B
-B
-B
,B
+B
(�B
%�B
(�B
.B
-B
)�B
)�B
(�B
%�B
!�B
�B
�B
!�B
#�B
%�B
(�B
&�B
#�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
"�B
#�B
#�B
#�B
"�B
�B
�B
�B
"�B
"�B
"�B
"�B
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
�B
�B
�B
�B
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
�B
�B
#�B
#�B
"�B
!�B
 �B
 �B
#�B
"�B
!�B
!�B
�B
!�B
�B
 �B
�B
!�B
"�B
�B
�B
�B
!�B
$�B
 �B
�B
!�B
&�B
$�B
$�B
)�B
)�B
)�B
)�B
(�B
'�B
)�B
)�B
+B
(�B
%�B
#�B
$�B
)�B
+B
,B
)�B
(�B
(�B
+B
+B
,B
,B
-B
-B
.B
-B
.B
/B
/B
.B
-B
,B
)�B
+B
-B
,B
(�B
+B
-B
0!B
1'B
2-B
1'B
1'B
0!B
2-B
2-B
1'B
/B
/B
-B
+B
2-B
2-B
49B
49B
33B
2-B
0!B
.B
/B
49B
5?B
6FB
6FB
6FB
6FB
5?B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
9XB
9XB
7LB
8RB
9XB
6FB
5?B
8RB
8RB
6FB
6FB
33B
5?B
9XB
8RB
8RB
<jB
=qB
=qB
;dB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
>wB
@�B
@�B
?}B
=qB
>wB
A�B
A�B
?}B
>wB
B�B
D�B
D�B
D�B
D�B
C�B
@�B
>wB
D�B
E�B
G�B
G�B
F�B
E�B
B�B
E�B
C�B
@�B
D�B
H�B
G�B
H�B
I�B
J�B
K�B
L�B
L�B
L�B
L�B
K�B
L�B
K�B
K�B
J�B
J�B
I�B
F�B
F�B
J�B
F�B
O�B
M�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
P�B
P�B
O�B
P�B
O�B
N�B
N�B
P�B
N�B
L�B
O�B
N�B
O�B
P�B
P�B
Q�B
P�B
R�B
S�B
S�B
T�B
S�B
S�B
R�B
T�B
T�B
VB
VB
VB
VB
T�B
T�B
VB
T�B
W
B
T�B
VB
YB
[#B
ZB
YB
ZB
\)B
\)B
\)B
\)B
[#B
\)B
ZB
XB
W
B
W
B
\)B
[#B
]/B
]/B
^5B
`BB
`BB
bNB
bNB
aHB
cTB
cTB
cTB
bNB
bNB
aHB
_;B
^5B
aHB
bNB
bNB
`BB
aHB
cTB
e`B
e`B
dZB
dZB
dZB
e`B
dZB
dZB
dZB
ffB
e`B
cTB
ffB
gmB
e`B
cTB
e`B
hsB
ffB
ffB
hsB
iyB
jB
k�B
k�B
jB
iyB
iyB
iyB
jB
jB
hsB
hsB
l�B
l�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
n�B
n�B
n�B
m�B
m�B
o�B
o�B
m�B
m�B
k�B
m�B
p�B
p�B
o�B
o�B
o�B
q�B
r�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
q�B
p�B
o�B
p�B
q�B
u�B
t�B
u�B
v�B
v�B
v�B
u�B
t�B
t�B
s�B
s�B
t�B
s�B
r�B
r�B
q�B
r�B
t�B
u�B
v�B
v�B
w�B
w�B
u�B
v�B
u�B
u�B
x�B
w�B
w�B
v�B
v�B
u�B
w�B
x�B
y�B
x�B
x�B
z�B
z�B
z�B
{�B
{�B
z�B
y�B
x�B
y�B
y�B
z�B
|�B
}�B
}�B
|�B
{�B
|�B
|�B
{�B
z�B
~�B
~�B
� B
� B
� B
� B
~�B
~�B
~�B
}�B
|�B
}�B
}�B
|�B
|�B
}�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�%B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�7B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�DB
�=B
�=B
�=B
�=B
�1B
�1B
�DB
�=B
�DB
�DB
�DB
�DB
�=B
�=B
�DB
�=B
�JB
�VB
�PB
�PB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�PB
�JB
�JB
�PB
�PB
�PB
�VB
�PB
�PB
�PB
�JB
�VB
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�VB
�VB
�VB
�PB
�\B
�bB
�hB
�bB
�bB
�bB
�bB
�hB
�bB
�\B
�\B
�oB
�uB
�oB
�uB
�oB
�uB
�uB
�uB
�oB
�oB
�uB
��B
��B
��B
��B
��B
�{B
�uB
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
��1111111111111111111111111111111111111111111111111111144111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220615090109                              AO  ARCAADJP                                                                    20220615090109    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220615090109  QCP$                G�O�G�O�G�O�205F03E         AO  ARGQQCPL                                                                    20220615090109  QCF$                G�O�G�O�G�O�4000            