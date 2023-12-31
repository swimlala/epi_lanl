CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-31T21:35:34Z creation;2018-10-31T21:35:37Z conversion to V3.1;2019-12-23T06:12:46Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181031213534  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               dA   JA  I2_0675_100                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؍.�=�1   @؍/K� @6�Xy=��cU��#��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�  A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj  Br  Bz  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C � C� C� C� C� C
� C� C� C� C� C� C� C� C� C� C� C � C"� C$� C&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�33C�33C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT&fDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D��D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�S3D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D D�� D� D�P DÐ D�� D� D�P DĐ D�� D� D�P DŐ D�� D� D�P DƐ D�� D� D�P Dǐ D�� D� D�P DȐ D�� D� D�P Dɐ D�� D� D�P Dʐ D�� D� D�P Dː D�� D� D�P D̐ D�� D� D�P D͐ D�� D� D�P Dΐ D�� D� D�P Dϐ D�� D� D�P DА D�� D� D�P Dѐ D�� D� D�P DҐ D�� D� D�P DӐ D�� D� D�P DԐ D�� D� D�P DՐ D�� D� D�P D֐ D�� D� D�P Dא D�� D� D�P Dؐ D�� D� D�P Dِ D�� D� D�P Dڐ D�� D� D�P Dې D�� D� D�P Dܐ D�� D� D�P Dݐ D�� D� D�P Dސ D�� D� D�P Dߐ D�� D� D�P D�� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsA�jA�hsA�ffA�dZA�dZA�dZA�dZA�ffA�ffA�hsA�hsA�hsA�jA�hsA�jA�n�A�r�A�v�A�z�A�~�A�~�Aω7Aϝ�Aϣ�Aϧ�Aϧ�Aϥ�Aϡ�Aϣ�Aϡ�A�r�A��Ȁ\A�S�A���A�  A�bA�^5A�r�A��A��A�Q�A�r�A�t�A�;dA���A�33A�S�A�%A�ȴA�-A�/A���A�  A�^5A�~�A��`A��!A�ZA��A�33A� �A�5?A���A�1A�Q�A�G�A���A���A�\)A�(�A���A�1A��hA�33A���A�l�A�z�A��A���A�oA�hsA��A�ĜA�VA�v�A�{A��A���A���A��A��uA��#A���A�Q�A� �A��yA�9XA��RA��/A�^5A��`A���A�A~�A}Az{AxffAw�PAwO�AwAv�At(�AsVAq�Aq�Apn�Am��Aj9XAi�AgdZAf�9Af~�Af(�Ad~�Ab�!A`A�A]�#A\��A[�PAX��AWS�AU�AS�TAQ�
AO�hAMp�AKl�AI�^AH��AH  AFM�AD^5ABȴA@5?A>�HA>E�A=l�A;p�A:(�A8ZA6�A6bNA5t�A3�
A3K�A2�uA1��A1A.��A-�#A-O�A,~�A+p�A*�uA)�wA(ȴA(��A'"�A%�A$��A#��A"�RA!��A bAhsA%A�AQ�A�#A�^A��A(�A\)AZA��AE�A(�A�A��A{A��A�
AO�AĜA$�A�yAbAC�AVA�A\)A
�9A	�mA1'A�^AhsA�AE�A"�A9XAffA33A  �@��@��9@��@�z�@��h@�|�@�M�@��@�9X@��m@�\)@�5?@���@�@���@�o@�@�ƨ@�@�O�@���@�R@⟾@�=q@�p�@�@�S�@�-@�x�@���@��H@���@�`B@�b@�"�@ՙ�@��@�x�@��m@�v�@�x�@�\)@��T@Ȭ@ư!@�p�@�A�@�o@��@�1'@��@�"�@��y@�-@��@�&�@���@��m@�
=@�M�@�?}@��j@�Z@��@�dZ@�$�@�O�@��@���@��!@�E�@���@�p�@���@�1'@�\)@���@��#@�hs@���@��@��;@�ƨ@�l�@�33@���@�-@��^@�7L@��@�z�@�|�@�K�@��y@�V@�{@��@�/@���@�Z@� �@�ƨ@�|�@�C�@�
=@��R@�E�@��@���@�x�@�x�@�`B@�G�@�7L@�V@��@��`@�Ĝ@��m@�dZ@�33@�o@�
=@�@��@��@��H@��R@�v�@�@�J@�{@�J@��#@���@�X@��@�Ĝ@���@��j@�bN@���@��@�ȴ@��\@�M�@���@��-@�X@�`B@�G�@��@���@�bN@�9X@�b@�|�@��H@��+@�E�@�E�@�V@�5?@�?}@��u@���@�{@�=q@�5?@�-@�J@�G�@��D@���@�z�@�Ĝ@�1'@�\)@�C�@��!@���@�+@��y@�ff@��@�bN@���@��/@�O�@�p�@��7@�O�@�V@�%@��`@��@�Z@� �@�1@��;@�C�@��@��@�33@�
=@��R@�n�@�-@��T@��7@�?}@�V@��`@���@�A�@� �@���@���@��P@�|�@�33@���@�-@���@�7L@��j@��@��@�Z@�1@���@��F@��F@��m@���@��F@���@��P@��@�l�@�dZ@�K�@�+@��@�ff@�^5@�M�@�=q@�5?@�-@�{@�@��^@�x�@�G�@��9@�j@�I�@� �@���@��F@�l�@�C�@�+@��@�n�@��@�J@���@��#@��-@���@�7L@��@�Ĝ@��9@�z�@�A�@�(�@�  @;d@
=@~ȴ@~�R@~��@~E�@}��@}@}�h@}p�@}?}@|��@|�D@|1@{dZ@{@z�!@z^5@z�@y��@y�#@y��@y��@x�`@x1'@w�;@w�P@wl�@w+@v�y@vȴ@v�R@vE�@u��@u?}@tz�@s��@s��@sdZ@so@r�!@r=q@qhs@pĜ@p�9@p�9@p�@pr�@pbN@pb@p  @o�@o��@o�@o�@o��@o�@nff@n@m��@mO�@l�@l�@l9X@l(�@l1@kƨ@kdZ@j�@jn�@j�@i�#@h��@g��@g+@f�R@fv�@f$�@f@e�T@e�@eV@d�@d9X@c��@c�
@c��@ct�@bM�@a�^@a&�@`�9@_�w@_�@^��@^$�@^{@^@]�-@]O�@]/@]V@\��@\�@\��@\Z@[�
@[��@[S�@Z�H@Z�!@Z�\@Z�@ZJ@ZJ@Y��@Y�@Y��@X�u@XQ�@W�P@W+@V�@V��@V��@V�+@Vv�@VV@V5?@V$�@U�-@U�@T��@Sƨ@R�H@R��@R~�@R�\@R�\@R^5@Q��@Q�7@QX@P�`@P��@Pb@O�w@O�@O|�@O\)@OK�@OK�@O;d@N�@NE�@N{@M�T@M�-@M�@M/@L��@L�j@Lj@LI�@L�@K��@K�
@K��@KS�@J��@Jn�@J=q@J-@JJ@I�@IG�@H��@H�@H �@H  @H  @H  @G�P@G|�@G;d@F�+@F5?@F{@E@E��@D�@D�@D�D@D�@C�
@CS�@B�@B��@B~�@BM�@A�#@A��@Ahs@AG�@A&�@@��@@�u@@A�@?�P@>��@>��@>��@>V@=��@=?}@<��@<j@<Z@<9X@<(�@<(�@<(�@<(�@<(�@<�@<1@;��@;�
@;�@;dZ@:��@:M�@:-@9�@9��@9X@9�@8�`@81'@7�@7;d@6�y@6v�@6$�@5��@5`B@5�@4�/@4�/@4��@4j@4I�@3ƨ@2�H@2�!@2�\@2n�@2M�@2M�@2=q@2-@2J@1hs@0�`@0Ĝ@0��@0�u@0�@0bN@0 �@0  @/�w@/��@/�P@/\)@/K�@.��@.��@.ff@.V@.E�@.{@-�@-�-@-p�@-/@,�/@,z�@,�@+��@+33@*��@*^5@*�@)�#@)x�@)�@(��@(Ĝ@(Ĝ@(�9@(�@(r�@(r�@(bN@( �@'�@'�;@'K�@&�y@&��@&V@&E�@&5?@&$�@&$�@&@%��@%�h@%`B@$��@$�j@$z�@$9X@$1@#�F@#dZ@#@"��@"��@"M�@"�@!�#@!��@!X@ ��@ �u@ A�@�;@�;@�P@�@��@�y@��@ff@E�@5?@{@��@`B@/@V@�/@�j@��@Z@�@��@ƨ@��@t�@dZ@33@o@�@��@n�@�@�#@��@��@x�@&�@%@��@��@�9@�u@bN@1'@ �@b@b@�;@|�@+@��@��@��@��@��@ff@ff@E�@�@�T@��@��@�h@p�@O�@�@�@z�@Z@I�@�@�
@�@dZ@C�@"�@�@@��@�\@~�@^5@=q@=q@�@�#@��@x�@hs@X@�@Ĝ@�u@bN@�w@\)@�@��@�+@v�@v�@ff@V@$�@{@@�@�T@��@@�-@��@p�@p�@?}@�@�/@��@�@�@��@z�@9X@�@1@��@�
@ƨ@��@dZ@33@
��@
M�@
J@	�^@	�^@	��@	hs@��@��@Ĝ@�9@�9@��@��@��@��@��@��@�u@bN@b@�;@��@�w@�@��@|�@l�@l�@\)@�@�y@��@v�@V@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�hsA�jA�hsA�ffA�dZA�dZA�dZA�dZA�ffA�ffA�hsA�hsA�hsA�jA�hsA�jA�n�A�r�A�v�A�z�A�~�A�~�Aω7Aϝ�Aϣ�Aϧ�Aϧ�Aϥ�Aϡ�Aϣ�Aϡ�A�r�A��Ȁ\A�S�A���A�  A�bA�^5A�r�A��A��A�Q�A�r�A�t�A�;dA���A�33A�S�A�%A�ȴA�-A�/A���A�  A�^5A�~�A��`A��!A�ZA��A�33A� �A�5?A���A�1A�Q�A�G�A���A���A�\)A�(�A���A�1A��hA�33A���A�l�A�z�A��A���A�oA�hsA��A�ĜA�VA�v�A�{A��A���A���A��A��uA��#A���A�Q�A� �A��yA�9XA��RA��/A�^5A��`A���A�A~�A}Az{AxffAw�PAwO�AwAv�At(�AsVAq�Aq�Apn�Am��Aj9XAi�AgdZAf�9Af~�Af(�Ad~�Ab�!A`A�A]�#A\��A[�PAX��AWS�AU�AS�TAQ�
AO�hAMp�AKl�AI�^AH��AH  AFM�AD^5ABȴA@5?A>�HA>E�A=l�A;p�A:(�A8ZA6�A6bNA5t�A3�
A3K�A2�uA1��A1A.��A-�#A-O�A,~�A+p�A*�uA)�wA(ȴA(��A'"�A%�A$��A#��A"�RA!��A bAhsA%A�AQ�A�#A�^A��A(�A\)AZA��AE�A(�A�A��A{A��A�
AO�AĜA$�A�yAbAC�AVA�A\)A
�9A	�mA1'A�^AhsA�AE�A"�A9XAffA33A  �@��@��9@��@�z�@��h@�|�@�M�@��@�9X@��m@�\)@�5?@���@�@���@�o@�@�ƨ@�@�O�@���@�R@⟾@�=q@�p�@�@�S�@�-@�x�@���@��H@���@�`B@�b@�"�@ՙ�@��@�x�@��m@�v�@�x�@�\)@��T@Ȭ@ư!@�p�@�A�@�o@��@�1'@��@�"�@��y@�-@��@�&�@���@��m@�
=@�M�@�?}@��j@�Z@��@�dZ@�$�@�O�@��@���@��!@�E�@���@�p�@���@�1'@�\)@���@��#@�hs@���@��@��;@�ƨ@�l�@�33@���@�-@��^@�7L@��@�z�@�|�@�K�@��y@�V@�{@��@�/@���@�Z@� �@�ƨ@�|�@�C�@�
=@��R@�E�@��@���@�x�@�x�@�`B@�G�@�7L@�V@��@��`@�Ĝ@��m@�dZ@�33@�o@�
=@�@��@��@��H@��R@�v�@�@�J@�{@�J@��#@���@�X@��@�Ĝ@���@��j@�bN@���@��@�ȴ@��\@�M�@���@��-@�X@�`B@�G�@��@���@�bN@�9X@�b@�|�@��H@��+@�E�@�E�@�V@�5?@�?}@��u@���@�{@�=q@�5?@�-@�J@�G�@��D@���@�z�@�Ĝ@�1'@�\)@�C�@��!@���@�+@��y@�ff@��@�bN@���@��/@�O�@�p�@��7@�O�@�V@�%@��`@��@�Z@� �@�1@��;@�C�@��@��@�33@�
=@��R@�n�@�-@��T@��7@�?}@�V@��`@���@�A�@� �@���@���@��P@�|�@�33@���@�-@���@�7L@��j@��@��@�Z@�1@���@��F@��F@��m@���@��F@���@��P@��@�l�@�dZ@�K�@�+@��@�ff@�^5@�M�@�=q@�5?@�-@�{@�@��^@�x�@�G�@��9@�j@�I�@� �@���@��F@�l�@�C�@�+@��@�n�@��@�J@���@��#@��-@���@�7L@��@�Ĝ@��9@�z�@�A�@�(�@�  @;d@
=@~ȴ@~�R@~��@~E�@}��@}@}�h@}p�@}?}@|��@|�D@|1@{dZ@{@z�!@z^5@z�@y��@y�#@y��@y��@x�`@x1'@w�;@w�P@wl�@w+@v�y@vȴ@v�R@vE�@u��@u?}@tz�@s��@s��@sdZ@so@r�!@r=q@qhs@pĜ@p�9@p�9@p�@pr�@pbN@pb@p  @o�@o��@o�@o�@o��@o�@nff@n@m��@mO�@l�@l�@l9X@l(�@l1@kƨ@kdZ@j�@jn�@j�@i�#@h��@g��@g+@f�R@fv�@f$�@f@e�T@e�@eV@d�@d9X@c��@c�
@c��@ct�@bM�@a�^@a&�@`�9@_�w@_�@^��@^$�@^{@^@]�-@]O�@]/@]V@\��@\�@\��@\Z@[�
@[��@[S�@Z�H@Z�!@Z�\@Z�@ZJ@ZJ@Y��@Y�@Y��@X�u@XQ�@W�P@W+@V�@V��@V��@V�+@Vv�@VV@V5?@V$�@U�-@U�@T��@Sƨ@R�H@R��@R~�@R�\@R�\@R^5@Q��@Q�7@QX@P�`@P��@Pb@O�w@O�@O|�@O\)@OK�@OK�@O;d@N�@NE�@N{@M�T@M�-@M�@M/@L��@L�j@Lj@LI�@L�@K��@K�
@K��@KS�@J��@Jn�@J=q@J-@JJ@I�@IG�@H��@H�@H �@H  @H  @H  @G�P@G|�@G;d@F�+@F5?@F{@E@E��@D�@D�@D�D@D�@C�
@CS�@B�@B��@B~�@BM�@A�#@A��@Ahs@AG�@A&�@@��@@�u@@A�@?�P@>��@>��@>��@>V@=��@=?}@<��@<j@<Z@<9X@<(�@<(�@<(�@<(�@<(�@<�@<1@;��@;�
@;�@;dZ@:��@:M�@:-@9�@9��@9X@9�@8�`@81'@7�@7;d@6�y@6v�@6$�@5��@5`B@5�@4�/@4�/@4��@4j@4I�@3ƨ@2�H@2�!@2�\@2n�@2M�@2M�@2=q@2-@2J@1hs@0�`@0Ĝ@0��@0�u@0�@0bN@0 �@0  @/�w@/��@/�P@/\)@/K�@.��@.��@.ff@.V@.E�@.{@-�@-�-@-p�@-/@,�/@,z�@,�@+��@+33@*��@*^5@*�@)�#@)x�@)�@(��@(Ĝ@(Ĝ@(�9@(�@(r�@(r�@(bN@( �@'�@'�;@'K�@&�y@&��@&V@&E�@&5?@&$�@&$�@&@%��@%�h@%`B@$��@$�j@$z�@$9X@$1@#�F@#dZ@#@"��@"��@"M�@"�@!�#@!��@!X@ ��@ �u@ A�@�;@�;@�P@�@��@�y@��@ff@E�@5?@{@��@`B@/@V@�/@�j@��@Z@�@��@ƨ@��@t�@dZ@33@o@�@��@n�@�@�#@��@��@x�@&�@%@��@��@�9@�u@bN@1'@ �@b@b@�;@|�@+@��@��@��@��@��@ff@ff@E�@�@�T@��@��@�h@p�@O�@�@�@z�@Z@I�@�@�
@�@dZ@C�@"�@�@@��@�\@~�@^5@=q@=q@�@�#@��@x�@hs@X@�@Ĝ@�u@bN@�w@\)@�@��@�+@v�@v�@ff@V@$�@{@@�@�T@��@@�-@��@p�@p�@?}@�@�/@��@�@�@��@z�@9X@�@1@��@�
@ƨ@��@dZ@33@
��@
M�@
J@	�^@	�^@	��@	hs@��@��@Ĝ@�9@�9@��@��@��@��@��@��@�u@bN@b@�;@��@�w@�@��@|�@l�@l�@\)@�@�y@��@v�@V@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BgmBgmBgmBgmBffBffBffBffBffBffBe`BffBffBffBffBffBgmBgmBhsBhsBiyBiyBjBo�Bp�Bq�Bq�Bq�Br�Br�Br�Br�Br�Bq�B��B��B�'B�B�B��B��B��B��B�oB�\B�1B�+B�1B�%B�B�B�1B�JB�bB�JBx�Bx�Br�Bq�Bq�Bv�B}�Bv�BbNBS�BJ�BB�B33B+B�B�BuB%B�fBɺB��B�=B|�Bm�BaHBaHBr�Bu�Bo�Bn�BdZB^5BYBVBS�BO�BF�B,B�B%B
�sB
ƨB
��B
�?B
�'B
�B
�B
��B
�%B
}�B
q�B
gmB
N�B
E�B
?}B
;dB
8RB
0!B
!�B
�B
JB
B	��B	�fB	ŢB	�^B	�B	��B	��B	��B	��B	�7B	x�B	hsB	_;B	W
B	I�B	@�B	7LB	/B	$�B	�B	hB	%B	  B��B�B�sB�#B��B�^B�B��B��B�bB�Bz�Bu�Br�Bq�Bm�Bl�Bk�BhsBffBgmBffBffBjBhsBe`BgmBcTBgmBdZB]/BaHBaHB`BBbNBZBZB_;B[#BYBXBS�BP�BP�BP�BoBM�BI�BH�BH�BE�BD�BA�B?}B=qB=qB:^B9XB6FB7LB6FB5?B49B33B2-B0!B/B.B-B,B)�B+B)�B'�B&�B%�B&�B%�B#�B$�B$�B%�B$�B#�B"�B#�B$�B$�B$�B$�B&�B+B+B+B,B,B+B+B,B+B+B+B)�B(�B(�B(�B)�B(�B)�B)�B2-B6FB5?B8RB7LB8RB>wB@�BA�BC�BF�BI�BK�BN�BO�BP�BS�BT�BXBZB\)B\)B^5BaHBdZBhsBk�Bl�Bm�Bp�Bs�Bu�Bu�Bw�By�By�Bz�B|�B� B�B�B�%B�1B�=B�PB�bB�oB�oB�uB�{B��B��B��B��B��B��B�B�B�B�-B�-B�-B�FB�dBBĜBȴB��B��B��B��B�B�#B�BB�`B�B�B�B�B�B��B��B	B	%B	%B	+B	1B		7B	
=B	JB	JB	\B	{B	�B	�B	�B	%�B	)�B	,B	.B	0!B	1'B	49B	6FB	8RB	:^B	>wB	C�B	F�B	H�B	I�B	L�B	M�B	P�B	Q�B	R�B	VB	VB	XB	YB	ZB	[#B	\)B	]/B	]/B	_;B	aHB	bNB	bNB	bNB	dZB	q�B	t�B	u�B	u�B	w�B	w�B	u�B	w�B	x�B	z�B	|�B	{�B	|�B	|�B	~�B	�B	�+B	�+B	�1B	�JB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�?B	�LB	�XB	�^B	�jB	�qB	�wB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�BB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
1B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
VB
VB
VB
VB
bB
hB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
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
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
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
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
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
=qB
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
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
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
K�B
K�B
K�B
L�B
L�B
M�B
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
O�B
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
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
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
YB
YB
YB
YB
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
[#B
[#B
[#B
\)B
\)B
\)B
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
^5B
^5B
^5B
^5B
^5B
_;B
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
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
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
e`B
e`B
e`B
e`B
ffB
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
k�B
k�B
k�B
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
o�B
o�B
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
p�B
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
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bg8BgRBgRBgRBf2BfLBf2BfLBfLBf2BeFBfLBf2BfLBfLBfLBgRBg8BhXBh>Bi_BiDBjKBoiBpoBq�BqvBqvBr|Br|Br|Br|Br|BqvB�gB��B��B��B��B��B��B��B�_B�:B�(B��B�B��B��B��B��B��B�B�.B�Bx�Bx�Br|BqvBqvBv�B}�Bv�BbBS�BJ�BB[B2�B*�B�BB@B�B�2BɠB��B�	B|�Bm]Ba-BaBr�Bu�BoiBncBd&B^BX�BU�BS�BO�BFtB+�BkB�B
�>B
�tB
�UB
�B
��B
��B
��B
��B
��B
}�B
qvB
g8B
N�B
EmB
?HB
;0B
8B
/�B
!�B
SB
B
�B	��B	�2B	�mB	�*B	��B	��B	��B	��B	�SB	�B	x�B	h>B	_B	V�B	I�B	@OB	7B	.�B	$�B	qB	4B	�B��B��B�B�>B��BбB�*B��B��B��B�.B��Bz�Bu�Br|BqvBm]BlWBkQBh>Bf2Bg8Bf2Bf2BjKBh>Be,Bg8BcBg8Bd&B\�BaBaB`BbBY�BY�B_BZ�BX�BW�BS�BP�BP�BP�G�O�BM�BI�BHfBHfBESBDgBAUB?HB="B="B:B9$B6B7B5�B5B4B2�B1�B/�B.�B-�B,�B+�B)�B*�B)�B'�B&�B%�B&�B%�B#�B$�B$�B%�B$�B#�B"�B#�B$�B$�B$�B$�B&�B*�B*�B*�B+�B+�B*�B*�B+�B*�B*�B*�B)�B(�B(�B(�B)�B(�B)�B)�B1�B6B5B8B7B8B>BB@4BAUBCGBFtBI�BK�BN�BO�BP�BS�BT�BW�BY�B[�B[�B]�BaBdBh>BkQBl=BmCBpoBshButButBw�By�By�Bz�B|�B�B��B��B��B��B��B�B�.B�:B�:B�@B�FB�SB�_B�qB�jB�pB��B��B��B��B��B��B��B�B�B�[B�gBȀB˒B͟BϫBҽB��B��B�B�,B�CB�iB�UB�UB�aB��B��B	�B	�B	�B	�B	�B	�B	
	B	�B	B	(B	FB	?B	eB	xB	%�B	)�B	+�B	-�B	/�B	0�B	4B	5�B	8B	:B	>(B	CGB	FtB	H�B	IlB	L~B	M�B	P�B	Q�B	R�B	U�B	U�B	W�B	X�B	Y�B	Z�B	[�B	\�B	\�B	_B	`�B	a�B	bB	bB	d&B	qvB	t�B	utB	u�B	w�B	w�B	utB	w�B	x�B	z�B	|�B	{�B	|�B	|�B	~�B	��B	��B	��B	��B	��B	�B	� B	�?B	�_B	�QB	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�$B	�B	�B	�"B	�(B	�.B	�UB	�UB	�AB	�GB	�GB	�GB	�MB	�gB	�gB	�gB	�MB	�SB	�_B	ɆB	ʌB	�xB	�~B	�~B	̈́B	ΥB	бB	ѝB	ҽB	ҽB	ҽB	өB	��B	ԯB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	� B	�B	�B	�,B	�2B	�2B	�B	�DB	�0B	�0B	�0B	�QB	�6B	�6B	�CB	�CB	�IB	�IB	�OB	�iB	�UB	�oB	�aB	�aB	�aB	�hB	�hB	�B	�B	�nB	�nB	��B	��B	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
	�B
	�B
B

�B
B
�B
�B
�B
�B
B
B
B
"B
B
B
.B
B
 B
:B
 B
:B
&B
@B
FB
,B
MB
MB
MB
MB
MB
?B
YB
EB
_B
kB
qB
qB
qB
WB
qB
xB
]B
]B
dB
dB
~B
dB
dB
�B
�B
jB
pB
pB
pB
 vB
 vB
 �B
 vB
 �B
 vB
!|B
!|B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
4B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
6B
6B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
8B
9	B
9$B
9$B
:B
;B
;0B
;B
;B
<B
<6B
=<B
=<B
="B
=<B
="B
=<B
=<B
="B
=<B
="B
=<B
=<B
="B
="B
>(B
>BB
>(B
>BB
?HB
?.B
?.B
?.B
@OB
A;B
A;B
AUB
B[B
BAB
B[B
B[B
BAB
BAB
B[B
B[B
CGB
B[B
CGB
DMB
DMB
DMB
DgB
DgB
DgB
DMB
DgB
DgB
EmB
ESB
EmB
EmB
ESB
FtB
FtB
FtB
FtB
G_B
G_B
GzB
GzB
G_B
GzB
G_B
H�B
HfB
H�B
H�B
HfB
IlB
IlB
I�B
IlB
I�B
J�B
J�B
J�B
K�B
K�B
KxB
L�B
L�B
M�B
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
O�B
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
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]�B
^B
]�B
^B
]�B
]�B
^B
^�B
^�B
^�B
_B
^�B
^�B
_B
`B
`B
_�B
_�B
_�B
`B
`B
_�B
aB
bB
a�B
bB
bB
bB
bB
c B
cB
c B
d&B
dB
d&B
d&B
dB
d&B
dB
dB
e,B
e,B
e,B
e,B
fB
f2B
fB
f2B
fB
f2B
gB
gB
gB
gB
g8B
gB
h>B
h>B
h>B
g8B
gB
h>B
h>B
h>B
h$B
h$B
iDB
i*B
i*B
i*B
iDB
i*B
iDB
iDB
i*B
iDB
i*B
i*B
i*B
j0B
jKB
j0B
jKB
j0B
j0B
jKB
k6B
kQB
k6B
kQB
k6B
kQB
k6B
kQB
kQB
l=B
lWB
m]B
m]B
mCB
mCB
m]B
nIB
nIB
ncB
nIB
oiB
oiB
oOB
oOB
oOB
oOB
oOB
oOB
oiB
oiB
pUB
pUB
poB
poB
pUB
qvB
q[B
qvB
qvB
q[B
raB
raB
r|B
r|B
raB
r|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.5(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811060043462018110600434620181106004346201811070036072018110700360720181107003607JA  ARFMdecpA19c                                                                20181101063515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181031213534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181031213535  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181031213535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181031213536  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181031213536  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181031213536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20181031213536  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20181031213536  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181031213536  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20181031213537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181031213537                      G�O�G�O�G�O�                JA  ARUP                                                                        20181031215602                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181101153950  CV  JULD            G�O�G�O�F�iu                JM  ARSQJMQC2.0                                                                 20181102000000  CF  PSAL_ADJUSTED_QCC�  C�  G�O�                JM  ARSQJMQC2.0                                                                 20181102000000  CF  TEMP_ADJUSTED_QCC�  C�  G�O�                JM  ARCAJMQC2.0                                                                 20181105154346  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181105154346  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181106153607  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                