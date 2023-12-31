CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:11:52Z creation;2022-06-04T19:11:52Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20220604191152  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ض�&~�/1   @ض���β@0�XbM��d1&�y1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�3C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$33C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CPL�CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~33C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�3C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$33C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CPL�CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~33C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�YA�G�A�FA�LdA�!A��PA��A��A��pA�҉A���A˿�A˺�A˹�A˶zA˳�A˱�A˪�A˩�AˮIAˣ�A�n�A��A�+A�1A�یAɘ+A���A��"A���A�*0A�qvA���A���A��A�+�A�[WA�(A��A��A�lWA�M�A�_Aß!A��A"A��A��A�H�A��A�q�A�5?A�%�A���A���A�e�A��]A��8A���A�>A� \A���A�p�A�l�A���A�S�A��/A�5?A��=A�i�A�)*A�J�A���A�V�A���A�=�A�h�A��eA��&A�'RA��A��uA�/�A���A�H�A��yA�cTA� �A���A� 'A�K)A�>A���A���A���A���A�b�A��A�(�A���A�(�A�'�A�%�A�r�A�^�A{�MAx�dAt*0Ap=qAnݘAm�"AmeAl�AfƨAeAa��A_A]'RAV��AR��AM��ALF�AI��AFW�AB��A?;A<�A:�A:7LA9}VA8��A5�zA35�A2��A1��A1:�A0�A.خA.4nA,ԕA+��A)1A%�)A#�A"��A#P�A$"�A#��A"|�A"��A!�A!{A FA�A�AjAOA/A�jAT�AuA\�A!A�A	A'�A8AC�A��AN�Av�AMA�A�-A~(A:�A`BAںA�A�_A�A�rA�A}VA��A�EA+�A9XA
�*A	�bA�VA�mAAq�A�A��A(�A�A�eA:�A��A+A�VAN<A9XA)_A ��A �uA .I@�� @�J�@�R�@�=�@�ȴ@��@��@�|@�@�{@��[@�ϫ@��o@��&@��1@��6@���@�͟@�e@�?�@�
=@�	@��,@�[�@�RT@�خ@�A@�/�@�g�@�kQ@�?@���@���@�[W@���@�xl@��@�j@�P�@�E�@�*@�4�@�4@�V�@��@�:@�c�@�'�@��@�W�@��)@�2a@��[@�X@ߤ@@ޣ�@�q�@ުe@�ی@���@���@��?@��@� �@َ"@�%@�Q@�!-@�D�@ԩ�@��@Ӥ@@��f@�z@��o@Ѡ'@��M@иR@�?�@�g�@��,@�_�@�($@Ͱ�@�j�@�J#@�@���@��@�j�@�#�@ʔF@�A�@�
�@���@�P�@�*0@��@ȴ9@�ff@���@�|@�+�@Ɗr@�7@��@Ŕ�@�E9@�+@���@�h
@�;�@��@�  @��j@Ã{@��@��[@�@�l�@�e@�@�q@�+k@�  @�x@���@��V@�[W@�ی@�_�@�˒@�[W@��/@�u�@�\�@�YK@�U2@�%�@��[@�'�@�W�@���@��H@��~@��@��@�x@��@��v@���@��@��@@�[W@�֡@�0U@�	�@��@���@�4@��R@�#:@���@��h@�c@�Q�@�/�@�@��@��[@��$@�|�@���@�rG@���@���@��@�C-@�  @�@��=@�x�@�4@��}@��r@���@�x@�X@�4@��@���@��@�|�@�c�@�/@��@�ں@�ȴ@��h@�a|@�C�@��@���@��6@�M@�b@���@��9@��^@���@�x@�L�@�^5@���@�!�@�+@��@��5@���@��@�7�@��Z@���@�dZ@�33@�� @�#:@��.@��@��	@�Vm@���@���@��1@��@�B[@��
@���@�\)@�ں@�Z@���@�|@�dZ@�8@�!�@��@�q@��@���@�_p@�2a@��P@�{�@�N�@�0U@�M@���@�F@���@���@�N�@���@�a�@�#�@��/@��F@�9X@�$@�	�@��@�X@�>�@�xl@��z@��f@�iD@�s�@�a�@�9�@�*0@�"�@�@��|@��}@��@�}V@�e�@�Q�@�4n@��@�hs@���@��$@�l�@�4@��K@�`B@�@��?@��O@�E�@�-�@�U2@�[�@�5?@�x@���@�|�@�Vm@���@��+@�c @��#@�k�@��	@���@��\@�tT@�>B@�J@���@���@���@��N@�iD@��P@��H@���@��u@��r@�^5@�6�@���@���@���@�l�@�2a@��@��9@���@��x@�e@��W@��H@���@�|@�X�@��@��@�z�@�n�@�Z�@�Ft@�)�@�	�@��@��9@���@�_p@��@��x@��A@�d�@�!�@�A@��@x@@O@8@.I@C@�@~�M@~��@~�1@~ff@}��@}��@}0�@|�5@|�j@|��@|��@|/�@{�a@{RT@z�@zȴ@z3�@y�z@yX@x�j@x�@w��@w!-@v��@vE�@u!�@t�.@t��@t�o@tc�@t�@s�[@rff@ru@q��@q��@p�U@p7�@p	�@o�@o�a@o�@ob�@o/�@oC@n�@n�,@n� @n($@m�^@m[W@m�@l�E@ly>@l�@kݘ@k�f@k"�@k"�@k�@j��@j{�@ja|@j@�@j�@j	@i�Z@i@i7L@h�_@h7�@g��@g=@f�@f�1@f;�@e�N@e*0@d�p@dh�@dFt@d7�@d �@c��@cO@c�@b�@bi�@a�@a�X@aV@`��@`,=@_��@_�{@_x@_dZ@_E9@_&@_S@^�@^_@]��@]e,@][W@]:�@\��@\��@\  @Z�@Z��@ZC�@Y�@Y��@Y?}@X��@X�z@X:�@W�
@W��@W�$@WO@W�@Vȴ@V�+@VR�@U�N@U0�@TV�@S��@S�a@S��@Sqv@S�@R�\@R!�@QrG@P�/@O��@Oj�@O
=@N�@Ns�@NZ�@N;�@N&�@N�@M��@M��@M-w@L�e@LQ�@L,=@L~@K�a@J�H@J��@JGE@I��@Im]@I+@H�v@Hj@H1@G�@G�V@G�V@G�{@Fߤ@F��@F_@E��@E��@EN<@D�K@D�z@D9X@CS@Bȴ@B��@B($@A�@As�@A�@@~(@@Xy@@�@?�]@?��@?��@?�*@?F�@>��@>�@>�b@>~�@>&�@=�@=IR@=�@<�@<oi@<2�@;ݘ@;�:@;v`@;a@;1�@:�,@:��@:�r@:M�@9�-@9=�@8��@8��@8��@8j@8  @7��@7@6͟@6��@6~�@65?@5�)@5�z@5��@52a@4��@4Xy@4�@3�F@3RT@3o@3�@2ߤ@2�x@1��@1�7@1j@10�@0�@0Xy@0:�@/� @/�q@/��@/|�@/H�@.�"@.�'@.c @.1�@-�D@-ϫ@-Q�@-%@,�@,��@,U2@,  @+� @+��@+~�@+"�@*�r@*s�@*5?@)�T@)��@)�7@)<6@)@(�5@(��@(Z@(�@'ݘ@'��@'J#@'�@&�@&�@&��@&�\@&~�@&d�@&?@%�o@%Y�@%-w@$֡@$�e@$[�@$Q�@$*�@$�@#�@@#�@#~�@#g�@#=@"��@"��@"{�@"E�@"	@!@!�@!j@!5�@ �)@ ��@ �@ M@ !@�
@�$@o@�s@��@p;@4@��@�n@�h@}�@\�@;@��@��@|�@1@��@t�@C�@4�@�@�c@�}@��@ff@E�@_@��@�@w2@&�@ѷ@>B@�@� @��@j�@6z@�]@n�@e@�@�o@��@w2@e,@\�@�@�?@�z@]d@$@��@ƨ@�0@�@v`@H�@C@�m@��@�@\�@4@�>@��@�@��@��@7L@�@�@�@��@��@*�@�m@�$@Mj@�2@�'@��@��@~�@V@($@_@�@�@��@��@Vm@;@��@��@��@bN@V�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�YA�G�A�FA�LdA�!A��PA��A��A��pA�҉A���A˿�A˺�A˹�A˶zA˳�A˱�A˪�A˩�AˮIAˣ�A�n�A��A�+A�1A�یAɘ+A���A��"A���A�*0A�qvA���A���A��A�+�A�[WA�(A��A��A�lWA�M�A�_Aß!A��A"A��A��A�H�A��A�q�A�5?A�%�A���A���A�e�A��]A��8A���A�>A� \A���A�p�A�l�A���A�S�A��/A�5?A��=A�i�A�)*A�J�A���A�V�A���A�=�A�h�A��eA��&A�'RA��A��uA�/�A���A�H�A��yA�cTA� �A���A� 'A�K)A�>A���A���A���A���A�b�A��A�(�A���A�(�A�'�A�%�A�r�A�^�A{�MAx�dAt*0Ap=qAnݘAm�"AmeAl�AfƨAeAa��A_A]'RAV��AR��AM��ALF�AI��AFW�AB��A?;A<�A:�A:7LA9}VA8��A5�zA35�A2��A1��A1:�A0�A.خA.4nA,ԕA+��A)1A%�)A#�A"��A#P�A$"�A#��A"|�A"��A!�A!{A FA�A�AjAOA/A�jAT�AuA\�A!A�A	A'�A8AC�A��AN�Av�AMA�A�-A~(A:�A`BAںA�A�_A�A�rA�A}VA��A�EA+�A9XA
�*A	�bA�VA�mAAq�A�A��A(�A�A�eA:�A��A+A�VAN<A9XA)_A ��A �uA .I@�� @�J�@�R�@�=�@�ȴ@��@��@�|@�@�{@��[@�ϫ@��o@��&@��1@��6@���@�͟@�e@�?�@�
=@�	@��,@�[�@�RT@�خ@�A@�/�@�g�@�kQ@�?@���@���@�[W@���@�xl@��@�j@�P�@�E�@�*@�4�@�4@�V�@��@�:@�c�@�'�@��@�W�@��)@�2a@��[@�X@ߤ@@ޣ�@�q�@ުe@�ی@���@���@��?@��@� �@َ"@�%@�Q@�!-@�D�@ԩ�@��@Ӥ@@��f@�z@��o@Ѡ'@��M@иR@�?�@�g�@��,@�_�@�($@Ͱ�@�j�@�J#@�@���@��@�j�@�#�@ʔF@�A�@�
�@���@�P�@�*0@��@ȴ9@�ff@���@�|@�+�@Ɗr@�7@��@Ŕ�@�E9@�+@���@�h
@�;�@��@�  @��j@Ã{@��@��[@�@�l�@�e@�@�q@�+k@�  @�x@���@��V@�[W@�ی@�_�@�˒@�[W@��/@�u�@�\�@�YK@�U2@�%�@��[@�'�@�W�@���@��H@��~@��@��@�x@��@��v@���@��@��@@�[W@�֡@�0U@�	�@��@���@�4@��R@�#:@���@��h@�c@�Q�@�/�@�@��@��[@��$@�|�@���@�rG@���@���@��@�C-@�  @�@��=@�x�@�4@��}@��r@���@�x@�X@�4@��@���@��@�|�@�c�@�/@��@�ں@�ȴ@��h@�a|@�C�@��@���@��6@�M@�b@���@��9@��^@���@�x@�L�@�^5@���@�!�@�+@��@��5@���@��@�7�@��Z@���@�dZ@�33@�� @�#:@��.@��@��	@�Vm@���@���@��1@��@�B[@��
@���@�\)@�ں@�Z@���@�|@�dZ@�8@�!�@��@�q@��@���@�_p@�2a@��P@�{�@�N�@�0U@�M@���@�F@���@���@�N�@���@�a�@�#�@��/@��F@�9X@�$@�	�@��@�X@�>�@�xl@��z@��f@�iD@�s�@�a�@�9�@�*0@�"�@�@��|@��}@��@�}V@�e�@�Q�@�4n@��@�hs@���@��$@�l�@�4@��K@�`B@�@��?@��O@�E�@�-�@�U2@�[�@�5?@�x@���@�|�@�Vm@���@��+@�c @��#@�k�@��	@���@��\@�tT@�>B@�J@���@���@���@��N@�iD@��P@��H@���@��u@��r@�^5@�6�@���@���@���@�l�@�2a@��@��9@���@��x@�e@��W@��H@���@�|@�X�@��@��@�z�@�n�@�Z�@�Ft@�)�@�	�@��@��9@���@�_p@��@��x@��A@�d�@�!�@�A@��@x@@O@8@.I@C@�@~�M@~��@~�1@~ff@}��@}��@}0�@|�5@|�j@|��@|��@|/�@{�a@{RT@z�@zȴ@z3�@y�z@yX@x�j@x�@w��@w!-@v��@vE�@u!�@t�.@t��@t�o@tc�@t�@s�[@rff@ru@q��@q��@p�U@p7�@p	�@o�@o�a@o�@ob�@o/�@oC@n�@n�,@n� @n($@m�^@m[W@m�@l�E@ly>@l�@kݘ@k�f@k"�@k"�@k�@j��@j{�@ja|@j@�@j�@j	@i�Z@i@i7L@h�_@h7�@g��@g=@f�@f�1@f;�@e�N@e*0@d�p@dh�@dFt@d7�@d �@c��@cO@c�@b�@bi�@a�@a�X@aV@`��@`,=@_��@_�{@_x@_dZ@_E9@_&@_S@^�@^_@]��@]e,@][W@]:�@\��@\��@\  @Z�@Z��@ZC�@Y�@Y��@Y?}@X��@X�z@X:�@W�
@W��@W�$@WO@W�@Vȴ@V�+@VR�@U�N@U0�@TV�@S��@S�a@S��@Sqv@S�@R�\@R!�@QrG@P�/@O��@Oj�@O
=@N�@Ns�@NZ�@N;�@N&�@N�@M��@M��@M-w@L�e@LQ�@L,=@L~@K�a@J�H@J��@JGE@I��@Im]@I+@H�v@Hj@H1@G�@G�V@G�V@G�{@Fߤ@F��@F_@E��@E��@EN<@D�K@D�z@D9X@CS@Bȴ@B��@B($@A�@As�@A�@@~(@@Xy@@�@?�]@?��@?��@?�*@?F�@>��@>�@>�b@>~�@>&�@=�@=IR@=�@<�@<oi@<2�@;ݘ@;�:@;v`@;a@;1�@:�,@:��@:�r@:M�@9�-@9=�@8��@8��@8��@8j@8  @7��@7@6͟@6��@6~�@65?@5�)@5�z@5��@52a@4��@4Xy@4�@3�F@3RT@3o@3�@2ߤ@2�x@1��@1�7@1j@10�@0�@0Xy@0:�@/� @/�q@/��@/|�@/H�@.�"@.�'@.c @.1�@-�D@-ϫ@-Q�@-%@,�@,��@,U2@,  @+� @+��@+~�@+"�@*�r@*s�@*5?@)�T@)��@)�7@)<6@)@(�5@(��@(Z@(�@'ݘ@'��@'J#@'�@&�@&�@&��@&�\@&~�@&d�@&?@%�o@%Y�@%-w@$֡@$�e@$[�@$Q�@$*�@$�@#�@@#�@#~�@#g�@#=@"��@"��@"{�@"E�@"	@!@!�@!j@!5�@ �)@ ��@ �@ M@ !@�
@�$@o@�s@��@p;@4@��@�n@�h@}�@\�@;@��@��@|�@1@��@t�@C�@4�@�@�c@�}@��@ff@E�@_@��@�@w2@&�@ѷ@>B@�@� @��@j�@6z@�]@n�@e@�@�o@��@w2@e,@\�@�@�?@�z@]d@$@��@ƨ@�0@�@v`@H�@C@�m@��@�@\�@4@�>@��@�@��@��@7L@�@�@�@��@��@*�@�m@�$@Mj@�2@�'@��@��@~�@V@($@_@�@�@��@��@Vm@;@��@��@��@bN@V�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�ZB	�tB	��B	��B	�&B	��B	��B	��B	��B	��B	��B	��B	�B	�@B	�ZB	�ZB	�,B	�>B	�oB	�VB	��B
B
$tB
#�B
+B
5?B
GB
G+B
n�B
�bB
��B
��B
��B
��B
�B
�zB
�4B
ݘB
�yB�B�BmBpB*0B+kB%B$�B*�B5B<BWsBm]BxRBqBl�Bs�ButB|�B� B� B�xB��B��B��B�[B�QB	B1B�B�B# B'RB+�B-�B+QB �B)BB�B�B��B��B�BԕB��B��B�B�!B~�Bd�BU�B?�B-CBeB"B
��B
�MB
�	B
��B
�6B
pB
L~B
c�B
<�B
DB	�8B	�)B	�-B	��B	�[B	�*B	�pB	�-B	tB	a�B	M�B	AoB	 �B	B��B��B�KB�B�B��B�xB�B��B	�B	VB	�B	B	 vB	!�B	 B	~B	7B	B	�B	�B	�B	�B	�B	�B	8�B	X�B	l�B	h�B	t�B	tTB	l�B	j�B	r�B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�xB	�QB	�B	��B	��B	��B	��B	�nB	��B	�fB	�FB	��B	��B	�3B	��B	�B	�DB	�B	�uB	� B	�PB	�fB	��B	�%B	��B	��B	�FB	�+B	��B	�LB	�B	�fB	��B	��B	��B	�MB	�B	��B	�	B	�ZB	�zB	�xB	��B	�B	��B	��B	�B	��B	��B	��B	�nB	��B	�B	�/B	��B	�B	�$B	�$B	�4B	�B	��B	҉B	��B	�SB	�B	��B	�FB	�4B	�,B	�jB	�OB	�7B	�_B	�7B	�B	ںB	�]B	��B	ݘB	�]B	�xB	��B	�#B	چB	�B	�7B	�B	��B	�dB	��B	ބB	��B	ߤB	�vB	�hB	�,B	�NB	޸B	�/B	�B	�B	��B	�hB	�'B	ݘB	��B	�EB	�_B	�B	��B	�YB	�sB	�EB	��B	��B	ڠB	�7B	�)B	ܒB	�B	ܬB	ܬB	�/B	ݘB	��B	ݘB	��B	�VB	�VB	�;B	��B	�pB	��B	�B	ߤB	�B	��B	��B	� B	�B	�B	�B	�tB	�B	�B	�ZB	�ZB	�,B	�fB	�B	�2B	��B	�B	�LB	�LB	�fB	�2B	�2B	�2B	�B	�$B	�>B	�
B	��B	��B	�B	�yB	�DB	��B	��B	�B	��B	�sB	��B	�*B	�B	�>B	�B	�B	�B	�sB	�B	��B	�yB	�B	��B	��B	��B	�WB	��B	��B	�5B	�B	�5B	�B	�B	�UB	�vB	�B	��B	��B	�GB	��B	�hB	�9B	�nB	�TB	�B	�9B	�9B	�TB	�TB	�B	�TB	�B	�TB	��B	�+B	�zB	��B	��B	��B	��B	��B	�8B	�B	��B	�>B	�rB	��B	��B	�XB	��B	��B	�B	�*B	�B	�^B	��B	�JB	�0B	�B	�JB	��B	�B	�0B	�dB	�B	��B	��B	�B	�B	�B	�6B	��B	��B	�wB	��B	��B	��B	��B	��B	�.B
  B
 iB
 �B
UB
 B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
tB
�B
�B
fB
fB
	B
	B
�B

	B

�B

�B
xB
�B
�B
~B
~B
~B
dB
dB
�B
PB
B
�B
vB
�B
�B
@B
{B
�B
�B
�B
�B
B
�B
�B
�B
�B
2B
�B
mB
�B
YB
�B
�B
�B
7B
�B
�B
�B
�B
	B
�B
=B
	B
�B
B
1B
�B
EB
+B
�B
�B
�B
QB
�B
�B
�B
�B
5B
�B
B
B
5B
dB
dB
CB
B
B
CB
xB
�B
B
5B
�B
�B
OB
�B
!�B
"�B
# B
"�B
"�B
#:B
#TB
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&fB
&�B
&�B
'B
'mB
'�B
'�B
(�B
*KB
*KB
*B
*�B
*�B
*�B
*�B
+B
+6B
+�B
+�B
,�B
,�B
,�B
,�B
-]B
-�B
.IB
.cB
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/B
/OB
/�B
0oB
0oB
0�B
0�B
1AB
1vB
1�B
1�B
1�B
2aB
2GB
1�B
1�B
1�B
2-B
2�B
3�B
3MB
2�B
3B
49B
4�B
5B
5�B
7fB
7LB
72B
8�B
9�B
9�B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;JB
;�B
;dB
;JB
;�B
<jB
<PB
<jB
<�B
="B
<�B
<�B
=<B
=�B
=�B
=�B
>]B
>�B
?B
?�B
?cB
?cB
?B
>�B
>�B
?cB
@4B
@�B
@�B
@OB
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C{B
DMB
D�B
D�B
D�B
D�B
D�B
EB
D�B
EB
EB
E�B
FB
F%B
E�B
F%B
F%B
FYB
F�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
HB
HB
HfB
H�B
H�B
IB
I7B
I�B
I�B
I�B
J#B
J#B
I�B
J	B
J�B
K�B
K�B
L0B
LdB
L0B
K�B
KB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KB
J�B
J�B
J�B
KDB
K�B
LB
LB
L�B
M�B
M�B
M�B
N"B
NVB
N<B
NVB
N�B
N�B
N�B
N�B
O(B
O�B
OvB
O�B
O�B
PbB
P�B
P}B
P�B
P�B
P�B
QhB
Q�B
Q�B
RB
R�B
S[B
S&B
SB
S�B
S�B
TFB
T�B
T�B
UB
UgB
UgB
U�B
V9B
VSB
V�B
VmB
V�B
V�B
W?B
W�B
W�B
W�B
W�B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XyB
X�B
Y1B
Y1B
YB
Y1B
Y1B
Y�B
Y�B
Z7B
Z7B
Z�B
[WB
[�B
[qB
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
^B
^�B
^�B
^�B
_pB
_pB
_�B
_�B
_�B
`'B
`'B
`�B
`�B
`�B
`�B
a|B
a�B
a�B
a�B
bB
b4B
bhB
b�B
b�B
b�B
c:B
cTB
cTB
c�B
c�B
c�B
dB
dB
d&B
dtB
d�B
eB
eFB
e`B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
fB
e�B
fLB
f�B
f�B
g8B
g�B
g�B
g�B
h
B
h
B
h$B
h$B
h$B
hXB
h�B
h�B
h�B
iB
i_B
i_B
i�B
i�B
i�B
jB
jB
j�B
jB
j�B
j�B
kB
kB
k�B
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
l�B
mB
m)B
mCB
m)B
m�B
m�B
nIB
n}B
n}B
n}B
n�B
o B
o B
o5B
oOB
o�B
o�B
o�B
o�B
p!B
pUB
qB
q'B
qAB
qvB
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
s3B
sB
sB
shB
s�B
s�B
tTB
tnB
t�B
t�B
t�B
t�B
u%B
u%B
uZB
u�B
u�B
u�B
vB
vFB
v`B
vzB
vzB
vzB
v�B
v�B
wB
w2B
wfB
wfB
wfB
w�B
xB
xRB
xlB
y	B
y	B
y$B
y$B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
zx11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�ZB	�tB	��B	��B	�&B	��B	��B	��B	��B	��B	��B	��B	�B	�@B	�ZB	�ZB	�,B	�>B	�oB	�VB	��B
B
$tB
#�B
+B
5?B
GB
G+B
n�B
�bB
��B
��B
��B
��B
�B
�zB
�4B
ݘB
�yB�B�BmBpB*0B+kB%B$�B*�B5B<BWsBm]BxRBqBl�Bs�ButB|�B� B� B�xB��B��B��B�[B�QB	B1B�B�B# B'RB+�B-�B+QB �B)BB�B�B��B��B�BԕB��B��B�B�!B~�Bd�BU�B?�B-CBeB"B
��B
�MB
�	B
��B
�6B
pB
L~B
c�B
<�B
DB	�8B	�)B	�-B	��B	�[B	�*B	�pB	�-B	tB	a�B	M�B	AoB	 �B	B��B��B�KB�B�B��B�xB�B��B	�B	VB	�B	B	 vB	!�B	 B	~B	7B	B	�B	�B	�B	�B	�B	�B	8�B	X�B	l�B	h�B	t�B	tTB	l�B	j�B	r�B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�xB	�QB	�B	��B	��B	��B	��B	�nB	��B	�fB	�FB	��B	��B	�3B	��B	�B	�DB	�B	�uB	� B	�PB	�fB	��B	�%B	��B	��B	�FB	�+B	��B	�LB	�B	�fB	��B	��B	��B	�MB	�B	��B	�	B	�ZB	�zB	�xB	��B	�B	��B	��B	�B	��B	��B	��B	�nB	��B	�B	�/B	��B	�B	�$B	�$B	�4B	�B	��B	҉B	��B	�SB	�B	��B	�FB	�4B	�,B	�jB	�OB	�7B	�_B	�7B	�B	ںB	�]B	��B	ݘB	�]B	�xB	��B	�#B	چB	�B	�7B	�B	��B	�dB	��B	ބB	��B	ߤB	�vB	�hB	�,B	�NB	޸B	�/B	�B	�B	��B	�hB	�'B	ݘB	��B	�EB	�_B	�B	��B	�YB	�sB	�EB	��B	��B	ڠB	�7B	�)B	ܒB	�B	ܬB	ܬB	�/B	ݘB	��B	ݘB	��B	�VB	�VB	�;B	��B	�pB	��B	�B	ߤB	�B	��B	��B	� B	�B	�B	�B	�tB	�B	�B	�ZB	�ZB	�,B	�fB	�B	�2B	��B	�B	�LB	�LB	�fB	�2B	�2B	�2B	�B	�$B	�>B	�
B	��B	��B	�B	�yB	�DB	��B	��B	�B	��B	�sB	��B	�*B	�B	�>B	�B	�B	�B	�sB	�B	��B	�yB	�B	��B	��B	��B	�WB	��B	��B	�5B	�B	�5B	�B	�B	�UB	�vB	�B	��B	��B	�GB	��B	�hB	�9B	�nB	�TB	�B	�9B	�9B	�TB	�TB	�B	�TB	�B	�TB	��B	�+B	�zB	��B	��B	��B	��B	��B	�8B	�B	��B	�>B	�rB	��B	��B	�XB	��B	��B	�B	�*B	�B	�^B	��B	�JB	�0B	�B	�JB	��B	�B	�0B	�dB	�B	��B	��B	�B	�B	�B	�6B	��B	��B	�wB	��B	��B	��B	��B	��B	�.B
  B
 iB
 �B
UB
 B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
tB
�B
�B
fB
fB
	B
	B
�B

	B

�B

�B
xB
�B
�B
~B
~B
~B
dB
dB
�B
PB
B
�B
vB
�B
�B
@B
{B
�B
�B
�B
�B
B
�B
�B
�B
�B
2B
�B
mB
�B
YB
�B
�B
�B
7B
�B
�B
�B
�B
	B
�B
=B
	B
�B
B
1B
�B
EB
+B
�B
�B
�B
QB
�B
�B
�B
�B
5B
�B
B
B
5B
dB
dB
CB
B
B
CB
xB
�B
B
5B
�B
�B
OB
�B
!�B
"�B
# B
"�B
"�B
#:B
#TB
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&fB
&�B
&�B
'B
'mB
'�B
'�B
(�B
*KB
*KB
*B
*�B
*�B
*�B
*�B
+B
+6B
+�B
+�B
,�B
,�B
,�B
,�B
-]B
-�B
.IB
.cB
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/B
/OB
/�B
0oB
0oB
0�B
0�B
1AB
1vB
1�B
1�B
1�B
2aB
2GB
1�B
1�B
1�B
2-B
2�B
3�B
3MB
2�B
3B
49B
4�B
5B
5�B
7fB
7LB
72B
8�B
9�B
9�B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;JB
;�B
;dB
;JB
;�B
<jB
<PB
<jB
<�B
="B
<�B
<�B
=<B
=�B
=�B
=�B
>]B
>�B
?B
?�B
?cB
?cB
?B
>�B
>�B
?cB
@4B
@�B
@�B
@OB
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C{B
DMB
D�B
D�B
D�B
D�B
D�B
EB
D�B
EB
EB
E�B
FB
F%B
E�B
F%B
F%B
FYB
F�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
HB
HB
HfB
H�B
H�B
IB
I7B
I�B
I�B
I�B
J#B
J#B
I�B
J	B
J�B
K�B
K�B
L0B
LdB
L0B
K�B
KB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KB
J�B
J�B
J�B
KDB
K�B
LB
LB
L�B
M�B
M�B
M�B
N"B
NVB
N<B
NVB
N�B
N�B
N�B
N�B
O(B
O�B
OvB
O�B
O�B
PbB
P�B
P}B
P�B
P�B
P�B
QhB
Q�B
Q�B
RB
R�B
S[B
S&B
SB
S�B
S�B
TFB
T�B
T�B
UB
UgB
UgB
U�B
V9B
VSB
V�B
VmB
V�B
V�B
W?B
W�B
W�B
W�B
W�B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XyB
X�B
Y1B
Y1B
YB
Y1B
Y1B
Y�B
Y�B
Z7B
Z7B
Z�B
[WB
[�B
[qB
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
^B
^�B
^�B
^�B
_pB
_pB
_�B
_�B
_�B
`'B
`'B
`�B
`�B
`�B
`�B
a|B
a�B
a�B
a�B
bB
b4B
bhB
b�B
b�B
b�B
c:B
cTB
cTB
c�B
c�B
c�B
dB
dB
d&B
dtB
d�B
eB
eFB
e`B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
fB
e�B
fLB
f�B
f�B
g8B
g�B
g�B
g�B
h
B
h
B
h$B
h$B
h$B
hXB
h�B
h�B
h�B
iB
i_B
i_B
i�B
i�B
i�B
jB
jB
j�B
jB
j�B
j�B
kB
kB
k�B
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
l�B
mB
m)B
mCB
m)B
m�B
m�B
nIB
n}B
n}B
n}B
n�B
o B
o B
o5B
oOB
o�B
o�B
o�B
o�B
p!B
pUB
qB
q'B
qAB
qvB
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
s3B
sB
sB
shB
s�B
s�B
tTB
tnB
t�B
t�B
t�B
t�B
u%B
u%B
uZB
u�B
u�B
u�B
vB
vFB
v`B
vzB
vzB
vzB
v�B
v�B
wB
w2B
wfB
wfB
wfB
w�B
xB
xRB
xlB
y	B
y	B
y$B
y$B
y>B
yXB
yXB
y�B
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
zx11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191152  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191152  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191152                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041200  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041200  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                