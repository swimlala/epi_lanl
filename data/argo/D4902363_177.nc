CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-09T00:35:17Z creation;2017-11-09T00:35:21Z conversion to V3.1;2019-12-19T07:57:11Z update;     
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
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20171109003517  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_177                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�3�.u� 1   @�3��>� @;]c�e���di�*0U21   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ D�|�D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ D�|�D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA���A�AɸRAɸRAɺ^Aɺ^Aɺ^Aɺ^Aɺ^AɼjAɸRAɲ-Aɩ�Aɧ�A�hsA�{A��A��;A�Q�A���A�  A�XA���A��uA��HA���A��A�(�A�r�A��A��9A���A�1'A�ƨA��yA�A�A��A�5?A�S�A�/A��#A�oA�l�A�x�A�(�A�O�A�$�A���A��A�hsA��7A��A���A�=qA��uA�;dA���A���A��A�O�A��-A�E�A���A�ZA�t�A�l�A���A���A�=qA�A�G�A���A��+A��A�z�A�^5A�ƨA�jA�1A�mA�-AO�A~ȴA}x�A|ffA|A{XAz�Ax�Aw��Av�`Au�AtȴAs�As�Aq�hAo�hAn�An�DAm�FAl�HAl=qAkx�AkoAj�yAj�DAjA�Ai�AiK�Ahn�AgdZAf�Ae�AdE�Ac�Ab��Ab^5Ab�Ab  AaVA_�
A_A^�A^��A^  A]�7A]33A]VA\��A\-A\ �A\1A[`BAZ�AZ9XAY��AYXAX�+AW�#AWC�AV�AV��AVZAU7LASp�AR9XAQO�AQ
=APZAO��AN��AM�AK��AJ�`AJ��AJ�+AJZAJ-AIXAH��AG�AF��AF~�AE�AD�ADVAC��ABr�AB1A@��A?ƨA?t�A>�A>��A>ffA=�;A=O�A<�A<n�A<(�A;A:�`A9��A8n�A6��A4v�A4-A4 �A3�A1�^A0~�A/�-A/
=A.^5A-��A-��A-�A,=qA+S�A+VA*-A)�A)ƨA)\)A(�`A(ȴA(�A(�A'�A%��A$�`A$�A$E�A#|�A"��A!��A!K�A A�A7LA�9A�
AG�A��AM�A��A�`AA�A��AjA/A�RA�A�yA�!Az�A1A�PAS�A�RA1'A��AA�-A�RA�A�A
ȴA
M�A
{A	A	S�A�A��A �AoAA�AS�AA 1'@��T@��@�z�@���@��@�dZ@�@��@�hs@�&�@�j@�@�P@��@�E�@���@���@�F@�^5@�%@��@��@�F@�o@��@�^@��@�r�@ߝ�@���@��@���@��@�"�@�ff@�&�@��@Ӆ@ҸR@�G�@���@�bN@�1@ύP@�ȴ@��@͙�@��/@�+@ʧ�@��@���@��@���@��m@�E�@��@���@��@��u@��H@���@�X@� �@��P@�"�@���@�n�@���@���@��/@�;d@�n�@�v�@���@���@�o@�@�G�@���@�Z@�+@�M�@��h@��@�1'@���@�\)@��@�O�@��`@���@��9@���@���@��!@��@�@���@��@��@���@�9X@�1@� �@���@��@�Z@�r�@�@���@��+@��-@��F@�l�@�dZ@���@��#@�ff@��R@�p�@��@�K�@��R@���@�7L@��-@�O�@���@�@�bN@��T@���@���@��h@���@��h@��7@��@�x�@�p�@�X@�hs@��#@���@�X@��@���@�Ĝ@��u@�j@�1'@��;@���@�V@��\@�@���@� �@�
=@�G�@�1'@�1'@��@��P@��P@�;d@��H@�ȴ@���@��@��-@�X@��@��@���@�I�@��m@�S�@��H@��+@�^5@�V@�@��h@�`B@��`@�z�@�bN@�(�@�;@��@~��@~E�@}�h@}O�@}/@|��@|�@|I�@{�F@{S�@z�@z��@y�#@y��@yX@xĜ@x�@w�;@w��@w|�@v��@vE�@u��@u@uO�@uV@t�@t��@t�D@s�m@sdZ@s@s@s@r^5@qx�@qG�@q�@p�`@pĜ@p��@p�@p1'@o�;@o�P@o�@nȴ@m��@mp�@m/@l�@m/@l�D@k�
@k�@k33@j��@jM�@i��@i�#@i�^@i��@i�#@jJ@j-@jJ@iX@h��@g�@g��@g�w@g|�@gK�@g+@g+@g+@g
=@f��@fȴ@f�R@f��@fȴ@g�@g+@gK�@g\)@g\)@gl�@g��@g�@hb@h  @g�w@gl�@f{@e�@dj@d1@cƨ@c��@cdZ@c@b�!@bn�@b-@a��@a7L@a%@`�`@`�u@` �@_�@_�w@_�@_�P@^�R@^ff@^E�@^5?@^$�@^@]`B@\�@\z�@\�@[�
@[��@[��@[dZ@[@Z��@Z�\@Z-@Y��@XĜ@Xr�@XQ�@W�;@W\)@V��@Vv�@Vv�@Xb@X�u@X��@XbN@Xb@X  @Wl�@WK�@W�@V�y@V��@U�h@UO�@Tz�@S��@S�F@St�@SdZ@S"�@S@So@SC�@S�F@S�
@S��@S��@S��@S��@S�@So@R�\@RM�@R-@R�@Q��@Q�#@Q�^@Q��@Q��@Q�@Q%@P��@O�@Nȴ@M�T@M�T@O��@PQ�@P1'@P  @O�@O�;@O��@O�w@O��@N��@NV@N5?@M�@MO�@L��@L�@L9X@L�@L�@K�F@KS�@J�H@J~�@I��@I��@I�@I�@I�7@I&�@H�`@H��@H�u@H1'@G�;@G�@Fff@E�@E��@Dz�@Ct�@C��@D(�@C�F@CS�@B��@A�@AX@?�@?
=@>�y@>�y@>��@?\)@@  @?�P@?+@>��@>v�@>V@=�@=?}@<��@<��@<��@<�@;dZ@;o@:��@:=q@9�@9&�@8��@8��@8A�@8A�@8  @7�w@7\)@7
=@7
=@6ȴ@6V@6E�@5�@5��@5��@5O�@5V@4��@4j@4�@41@3ƨ@333@3o@2�!@2=q@1��@1��@1��@1��@1�@0�u@/��@/|�@/\)@/+@/�@.�y@.�@.ȴ@.�+@.E�@.{@-@-��@-�h@-�@-O�@,�@,z�@,�@+��@+�m@+ƨ@+�F@+��@+�@+dZ@+S�@+33@+"�@+@+@*��@*�!@*~�@*=q@*�@)��@)��@)�@)G�@)%@(Ĝ@(�@( �@'�w@'�P@'l�@'�@&ȴ@&ȴ@&�R@&��@&E�@&$�@&@%�-@%?}@$�@$�@$Z@$1@#��@#��@#�m@#�
@#ƨ@#�@#t�@#S�@#S�@"�@"�!@"-@!�@ ��@ �9@  �@�@|�@+@
=@�y@�@�R@�+@ff@V@5?@{@@�@�T@�-@?}@�D@9X@�@1@��@ƨ@�F@��@�@dZ@C�@@��@^5@n�@J@��@hs@G�@�@�`@�@A�@�@�P@\)@+@�y@��@��@��@��@5?@�@�T@�@`B@/@/@�@z�@I�@9X@9X@(�@��@�m@�
@t�@o@��@=q@��@�@��@��@hs@�`@��@r�@bN@ �@�@�w@|�@�@��@v�@v�@V@�@�T@�T@@��@p�@/@�@��@�D@I�@�@�@1@�m@�F@"�@@
�H@
��@
�\@
�\@
~�@
n�@
M�@
-@
-@
-@
-@
J@	�#@	��@	�^@	��@	hs@	X@��@Ĝ@��@�@bN@  @�@�@��@��@��@��@�@��@l�@+@�@ȴ@��@V@V@E�@{@�T@�-@��@�h@`B@�@�@�@��@�@z�@I�@9X@(�@(�@(�@��@�F@��@��@�@t�@�@�@dZ@"�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA���A�AɸRAɸRAɺ^Aɺ^Aɺ^Aɺ^Aɺ^AɼjAɸRAɲ-Aɩ�Aɧ�A�hsA�{A��A��;A�Q�A���A�  A�XA���A��uA��HA���A��A�(�A�r�A��A��9A���A�1'A�ƨA��yA�A�A��A�5?A�S�A�/A��#A�oA�l�A�x�A�(�A�O�A�$�A���A��A�hsA��7A��A���A�=qA��uA�;dA���A���A��A�O�A��-A�E�A���A�ZA�t�A�l�A���A���A�=qA�A�G�A���A��+A��A�z�A�^5A�ƨA�jA�1A�mA�-AO�A~ȴA}x�A|ffA|A{XAz�Ax�Aw��Av�`Au�AtȴAs�As�Aq�hAo�hAn�An�DAm�FAl�HAl=qAkx�AkoAj�yAj�DAjA�Ai�AiK�Ahn�AgdZAf�Ae�AdE�Ac�Ab��Ab^5Ab�Ab  AaVA_�
A_A^�A^��A^  A]�7A]33A]VA\��A\-A\ �A\1A[`BAZ�AZ9XAY��AYXAX�+AW�#AWC�AV�AV��AVZAU7LASp�AR9XAQO�AQ
=APZAO��AN��AM�AK��AJ�`AJ��AJ�+AJZAJ-AIXAH��AG�AF��AF~�AE�AD�ADVAC��ABr�AB1A@��A?ƨA?t�A>�A>��A>ffA=�;A=O�A<�A<n�A<(�A;A:�`A9��A8n�A6��A4v�A4-A4 �A3�A1�^A0~�A/�-A/
=A.^5A-��A-��A-�A,=qA+S�A+VA*-A)�A)ƨA)\)A(�`A(ȴA(�A(�A'�A%��A$�`A$�A$E�A#|�A"��A!��A!K�A A�A7LA�9A�
AG�A��AM�A��A�`AA�A��AjA/A�RA�A�yA�!Az�A1A�PAS�A�RA1'A��AA�-A�RA�A�A
ȴA
M�A
{A	A	S�A�A��A �AoAA�AS�AA 1'@��T@��@�z�@���@��@�dZ@�@��@�hs@�&�@�j@�@�P@��@�E�@���@���@�F@�^5@�%@��@��@�F@�o@��@�^@��@�r�@ߝ�@���@��@���@��@�"�@�ff@�&�@��@Ӆ@ҸR@�G�@���@�bN@�1@ύP@�ȴ@��@͙�@��/@�+@ʧ�@��@���@��@���@��m@�E�@��@���@��@��u@��H@���@�X@� �@��P@�"�@���@�n�@���@���@��/@�;d@�n�@�v�@���@���@�o@�@�G�@���@�Z@�+@�M�@��h@��@�1'@���@�\)@��@�O�@��`@���@��9@���@���@��!@��@�@���@��@��@���@�9X@�1@� �@���@��@�Z@�r�@�@���@��+@��-@��F@�l�@�dZ@���@��#@�ff@��R@�p�@��@�K�@��R@���@�7L@��-@�O�@���@�@�bN@��T@���@���@��h@���@��h@��7@��@�x�@�p�@�X@�hs@��#@���@�X@��@���@�Ĝ@��u@�j@�1'@��;@���@�V@��\@�@���@� �@�
=@�G�@�1'@�1'@��@��P@��P@�;d@��H@�ȴ@���@��@��-@�X@��@��@���@�I�@��m@�S�@��H@��+@�^5@�V@�@��h@�`B@��`@�z�@�bN@�(�@�;@��@~��@~E�@}�h@}O�@}/@|��@|�@|I�@{�F@{S�@z�@z��@y�#@y��@yX@xĜ@x�@w�;@w��@w|�@v��@vE�@u��@u@uO�@uV@t�@t��@t�D@s�m@sdZ@s@s@s@r^5@qx�@qG�@q�@p�`@pĜ@p��@p�@p1'@o�;@o�P@o�@nȴ@m��@mp�@m/@l�@m/@l�D@k�
@k�@k33@j��@jM�@i��@i�#@i�^@i��@i�#@jJ@j-@jJ@iX@h��@g�@g��@g�w@g|�@gK�@g+@g+@g+@g
=@f��@fȴ@f�R@f��@fȴ@g�@g+@gK�@g\)@g\)@gl�@g��@g�@hb@h  @g�w@gl�@f{@e�@dj@d1@cƨ@c��@cdZ@c@b�!@bn�@b-@a��@a7L@a%@`�`@`�u@` �@_�@_�w@_�@_�P@^�R@^ff@^E�@^5?@^$�@^@]`B@\�@\z�@\�@[�
@[��@[��@[dZ@[@Z��@Z�\@Z-@Y��@XĜ@Xr�@XQ�@W�;@W\)@V��@Vv�@Vv�@Xb@X�u@X��@XbN@Xb@X  @Wl�@WK�@W�@V�y@V��@U�h@UO�@Tz�@S��@S�F@St�@SdZ@S"�@S@So@SC�@S�F@S�
@S��@S��@S��@S��@S�@So@R�\@RM�@R-@R�@Q��@Q�#@Q�^@Q��@Q��@Q�@Q%@P��@O�@Nȴ@M�T@M�T@O��@PQ�@P1'@P  @O�@O�;@O��@O�w@O��@N��@NV@N5?@M�@MO�@L��@L�@L9X@L�@L�@K�F@KS�@J�H@J~�@I��@I��@I�@I�@I�7@I&�@H�`@H��@H�u@H1'@G�;@G�@Fff@E�@E��@Dz�@Ct�@C��@D(�@C�F@CS�@B��@A�@AX@?�@?
=@>�y@>�y@>��@?\)@@  @?�P@?+@>��@>v�@>V@=�@=?}@<��@<��@<��@<�@;dZ@;o@:��@:=q@9�@9&�@8��@8��@8A�@8A�@8  @7�w@7\)@7
=@7
=@6ȴ@6V@6E�@5�@5��@5��@5O�@5V@4��@4j@4�@41@3ƨ@333@3o@2�!@2=q@1��@1��@1��@1��@1�@0�u@/��@/|�@/\)@/+@/�@.�y@.�@.ȴ@.�+@.E�@.{@-@-��@-�h@-�@-O�@,�@,z�@,�@+��@+�m@+ƨ@+�F@+��@+�@+dZ@+S�@+33@+"�@+@+@*��@*�!@*~�@*=q@*�@)��@)��@)�@)G�@)%@(Ĝ@(�@( �@'�w@'�P@'l�@'�@&ȴ@&ȴ@&�R@&��@&E�@&$�@&@%�-@%?}@$�@$�@$Z@$1@#��@#��@#�m@#�
@#ƨ@#�@#t�@#S�@#S�@"�@"�!@"-@!�@ ��@ �9@  �@�@|�@+@
=@�y@�@�R@�+@ff@V@5?@{@@�@�T@�-@?}@�D@9X@�@1@��@ƨ@�F@��@�@dZ@C�@@��@^5@n�@J@��@hs@G�@�@�`@�@A�@�@�P@\)@+@�y@��@��@��@��@5?@�@�T@�@`B@/@/@�@z�@I�@9X@9X@(�@��@�m@�
@t�@o@��@=q@��@�@��@��@hs@�`@��@r�@bN@ �@�@�w@|�@�@��@v�@v�@V@�@�T@�T@@��@p�@/@�@��@�D@I�@�@�@1@�m@�F@"�@@
�H@
��@
�\@
�\@
~�@
n�@
M�@
-@
-@
-@
-@
J@	�#@	��@	�^@	��@	hs@	X@��@Ĝ@��@�@bN@  @�@�@��@��@��@��@�@��@l�@+@�@ȴ@��@V@V@E�@{@�T@�-@��@�h@`B@�@�@�@��@�@z�@I�@9X@(�@(�@(�@��@�F@��@��@�@t�@�@�@dZ@"�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�B�B��B��B��B��BĜB�LB�3B��Bz�Bx�BI�B?}B#�B �B�B��B��B��B�yB�NB��B��B�BɺB��B�?B��B��B�PB�Bw�Bs�Bp�Bq�Br�Bk�B^5BP�BB�B@�B8RB(�B�B�BhBB
��B
��B
�B
�B
�`B
�HB
�
B
ǮB
ǮB
B
�wB
�FB
�-B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�hB
�PB
�B
~�B
|�B
v�B
q�B
ffB
`BB
ZB
Q�B
L�B
C�B
@�B
8RB
)�B
)�B
$�B
�B
�B
{B
VB
PB
JB
	7B
+B
B
  B	��B	��B	�B	�B	�`B	�HB	�)B	�B	�B	��B	��B	ɺB	ŢB	ȴB	ƨB	B	��B	��B	�}B	�jB	�XB	�^B	�XB	�9B	�!B	�B	�B	��B	��B	��B	��B	��B	�{B	�bB	�+B	y�B	w�B	u�B	u�B	p�B	k�B	e`B	^5B	YB	YB	[#B	YB	W
B	T�B	O�B	M�B	F�B	C�B	A�B	;dB	<jB	9XB	5?B	/B	-B	%�B	�B	�B	�B	�B	�B	uB	bB	VB	JB	
=B	%B	  B��B�B�B�;B�B�B�ZB�B�B��B��B��B��B��BɺBŢB��BÖB�wB��B�}B�jB�dB�dB�RB�9B�B��B��B�B��B��B��B��B��B��B�{B��B�hB�bB�hB�PB�=B�+B�%B�B|�By�Bz�Bs�Bt�Bt�Bs�Bp�Bn�Bn�Bk�BhsBaHB]/BT�BT�BVBVBVBT�BVBT�BR�BP�BP�BM�BG�BD�B>wB2-B$�B/B(�B.B.B-B0!B2-B2-B49B33B2-B1'B2-B1'B/B.B,B)�B'�B'�B&�B$�B$�B+B+B-B,B,B+B)�B'�B'�B&�B0!B1'B0!B0!B6FB5?B6FB?}B@�B@�B?}B=qB=qB=qB<jB8RB<jB;dB8RB7LB2-B33B33B49B49B2-B49B6FB:^B>wB=qBD�BF�BG�BI�BI�BI�BF�BD�BI�BP�BN�BM�BVBT�B[#BZBXBT�BXBZB[#BbNBiyBm�Bl�Bm�Bo�Br�Bt�Bv�Bz�By�B}�B�B�B�B�B�B�B�+B�DB�\B��B��B��B��B��B��B��B�{B��B��B��B��B�!B�XB�FB�9B�XB�XBƨB��B�
B�B��BȴBŢB�qBƨB��B��B��B��B��B��B��B��B��B��B�5B�yB�B��B��B��B��B��B��B	  B	  B��B	B	B	B	B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	%B	+B	1B	1B	PB	bB	�B	�B	�B	�B	�B	�B	#�B	$�B	(�B	-B	.B	0!B	2-B	2-B	49B	8RB	<jB	=qB	=qB	>wB	=qB	@�B	C�B	E�B	G�B	F�B	K�B	K�B	L�B	O�B	O�B	S�B	T�B	VB	VB	XB	YB	[#B	]/B	^5B	^5B	_;B	_;B	bNB	e`B	gmB	gmB	ffB	e`B	ffB	ffB	gmB	hsB	iyB	jB	jB	k�B	l�B	l�B	m�B	m�B	p�B	s�B	t�B	u�B	u�B	w�B	y�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�bB	�bB	�bB	�bB	�hB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�jB	�qB	�qB	�wB	�}B	�}B	��B	ÖB	ĜB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�)B	�5B	�;B	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
%B
B
B
%B
1B
+B
+B
	7B
	7B
	7B
DB
JB
JB
JB
PB
VB
\B
bB
bB
bB
bB
bB
hB
oB
hB
hB
oB
oB
oB
oB
oB
hB
bB
hB
{B
�B
�B
�B
�B
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
(�B
(�B
)�B
+B
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
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
5?B
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
9XB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
?}B
@�B
@�B
@�B
@�B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
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
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
XB
YB
YB
ZB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
`BB
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
aHB
aHB
bNB
bNB
bNB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
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
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�B�B��B��B��B��B��B�B�B�B�B�B�2BбB�6B��B�jB�>B��B��B��BQ�BF?B)�B%�B�B-B �B��B�B�zB��B��BۦB�B�{B�RB�RB��B��B�B{dBv+BshBsMBsMBl�B`�BS�BD�BA�B9�B,BOB�BB_B �B
�^B
�B
�B
��B
�hB
�1B
��B
��B
ÖB
�cB
��B
�hB
�CB
�WB
�"B
�0B
��B
�B
�xB
�_B
��B
��B
�B
�"B
��B
�B
}�B
w�B
r�B
hsB
a|B
[=B
SuB
M�B
EB
AoB
:*B
,"B
*�B
%�B
�B
�B
MB
(B
�B
�B
	�B
�B
�B
 �B	�B	�B	��B	��B	��B	�4B	��B	ڠB	�yB	ՁB	�B	�B	ƎB	�B	�B	�GB	� B	��B	��B	�B	��B	��B	��B	�%B	��B	��B	��B	��B	��B	��B	�]B	�B	��B	�B	��B	|B	y>B	v�B	v`B	q�B	l�B	f�B	_�B	[	B	Y�B	[WB	YB	WsB	U�B	Q B	N�B	HKB	D3B	B'B	<�B	<�B	:*B	6FB	0�B	-�B	'mB	 �B	;B	]B	B	B	,B	B	�B	�B	
�B	�B	UB��B�UB�B��B��B��B�B�CB׍B�B��B��B�}B̈́BʌB��B��B�B�}B��B��B�B�B��B��B�%B��B��B��B��B��B��B��B��B�dB��B��B�mB��B�NB�B�VB�DB�KB�+B�3B~�B{dB{�Bu?ButBu%BtBq[BoOBo5BlqBiyBc B_;BWYBVmBWYBV�BV�BU�BVmBU�BS�BQ�BQhBN�BI7BFB@iB5tB(sB1B+QB/OB/OB.cB1B2�B2�B4�B3�B2�B1�B2|B1�B/�B.�B,�B*�B(�B(�B'�B&LB&2B+�B+�B-wB,�B,�B+�B*�B)DB)�B(�B0�B1�B1B0�B6�B5�B7LB?�BAB@�B@ B>B>B=�B="B9rB<�B<B9>B88B3�B4�B4nB5?B5ZB3hB5ZB7fB;B>�B>BBEBGBH1BI�BJ#BJ#BGzBE�BJrBQ4BO�BO(BV�BU�B[�BZ�BX�BU�BX�BZ�B[�Bb�Bi�Bn�BmwBnBo�Br�Bt�Bv�B{JB{B}�B�SB��B�MB��B��B�mB�_B�DB�vB�gB��B��B�yB�	B�B�qB��B��B��B�ZB�DB��B�rB�LB�?B��B��B�B� B�
BּB�pBɠB��B��B��B��B��B��B��B��B��B��B��B� B�B�B�B��B�B��B��B�B�.B�HB	 iB	 �B�HB	AB	�B	�B	�B��B��B��B�B�"B�6B�B�BB�BB�.B�HB�]B	 OB	[B	YB	EB	fB	�B	�B	�B	�B	�B	�B	�B	B	 B	#�B	%FB	)DB	-)B	.IB	0UB	2aB	2|B	4�B	8�B	<�B	=�B	=�B	>�B	=�B	@�B	C�B	E�B	G�B	GB	K�B	K�B	MB	PB	PB	TB	UB	V9B	VSB	XEB	Y1B	[WB	]IB	^OB	^OB	_pB	_pB	b�B	ezB	g�B	g�B	f�B	e�B	f�B	f�B	g�B	hsB	i�B	j�B	j�B	k�B	l�B	l�B	m�B	m�B	p�B	s�B	t�B	u�B	vB	xB	zB	|6B	}<B	}<B	B	� B	�'B	��B	�B	�+B	�RB	�xB	��B	��B	��B	�}B	�}B	��B	�}B	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�CB	�IB	��B	�vB	�vB	�aB	�GB	�hB	�TB	�nB	�ZB	�`B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	� B	�B	� B	�&B	�MB	�$B	�$B	�?B	�KB	�CB	�OB	�!B	��B	�LB	�yB	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�	B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	��B	�B	�>B	�B	�B	��B	��B
�B
SB
9B
B
9B
%B
?B
9B
mB
YB
KB
EB
_B
	RB
	RB
	lB
DB
dB
~B
~B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
YB
yB
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
&B
%�B
&B
'B
'B
'B
'B
)B
)DB
*KB
+B
+B
+B
+6B
+6B
+6B
,"B
-)B
./B
./B
./B
/OB
/5B
/5B
/5B
/5B
0UB
0;B
0;B
1'B
1AB
0;B
0UB
1[B
1AB
2GB
33B
3MB
33B
3MB
3MB
3hB
4TB
4TB
5?B
5ZB
5%B
5ZB
5ZB
5tB
6`B
6`B
6`B
6FB
6`B
5tB
7�B
7fB
7fB
7�B
8RB
9�B
9�B
9rB
9rB
:^B
:xB
:^B
9rB
;B
;B
;�B
;�B
<jB
<�B
=�B
?�B
@�B
@�B
@�B
@�B
?�B
?�B
?}B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
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
J�B
K�B
K�B
K�B
K�B
MB
MB
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q B
PB
Q B
Q B
Q B
RB
RB
RB
R B
RB
SB
R�B
R�B
SB
SB
SB
SB
S&B
R B
S&B
TB
TB
T�B
UB
U2B
U2B
UMB
VB
W$B
X+B
YB
Y1B
Z7B
Y1B
YKB
YKB
ZQB
ZB
Z7B
Z7B
[#B
[	B
[=B
[=B
[=B
[=B
[=B
\CB
\)B
]IB
]IB
]/B
]IB
]IB
]IB
]dB
^OB
_VB
_VB
_pB
`BB
_VB
_;B
_VB
`BB
`BB
`BB
`BB
`\B
abB
aHB
aHB
abB
aHB
bhB
bhB
bhB
cnB
b�B
bhB
bhB
cTB
cTB
cnB
dtB
dZB
dZB
e`B
dtB
dtB
e�B
ezB
ezB
ezB
ezB
ffB
f�B
ffB
ffB
ffB
g�B
g�B
g�B
h�B
hsB
hsB
h�B
h�B
i�B
i�B
iyB
iyB
iyB
iyB
i�B
i�B
jB
jeB
jB
jB
jeB
jB
j�B
j�B
j�B
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711130033012017111300330120171113003301201806221233152018062212331520180622123315201804050429062018040504290620180405042906  JA  ARFMdecpA19c                                                                20171109093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171109003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171109003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171109003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171109003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171109003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171109003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171109003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171109003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171109003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20171109005522                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171109153709  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171112153301  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171112153301  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192906  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033315  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                