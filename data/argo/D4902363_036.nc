CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-12T00:35:17Z creation;2016-09-12T00:35:20Z conversion to V3.1;2019-12-19T08:30:39Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
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
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160912003517  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               $A   JA  I2_0576_036                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��5�&N 1   @��6UUU�@;.�1����dk��3�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBW��B_ffBg  BoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU��DVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�>fD�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��fD�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�nf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѴ9AѶFAѲ-Aљ�Aї�AѓuAѓuAѓuAѓuAѓuAѓuAѓuAѕ�Aѕ�Aѕ�Aѕ�AыDAч+AуA�\)A�E�A� �A��A�ƨA�ffAκ^A�  Aɥ�A�dZA���A�-A��A��A��A�A���A�$�A��uA��;A�v�A�oA��uA�9XA���A�ȴA�33A��uA�A�A�`BA���A�?}A��hA�-A�S�A��A��TA�"�A�M�A�1'A�M�A�?}A���A�M�A��FA�-A�$�A��A��A��-A�bNA���A�ffA�l�A�G�A���A��A�|�A�z�A�\)A��jA��hA��A�ƨA�I�A�{A���A�|�A�E�A��A}oAz�AyVAx�9AwAu��At~�As��AqƨAp  AoC�An�\Am\)Al�`AlbNAj��Ah�uAf��Ae�7AeO�AeoAd��Ad$�AcS�Ab��Aa�Aa7LA`E�A_hsA^�A]�-A];dA\��A[ƨAZ�AXAVbNAV9XAU?}AU�AT�9AS�AR��AP��AP-AO��ANffAM��AL��AL��ALVAK�AK�hAK?}AJ �AH�jAG�PAGK�AGVAE�wAD�!ADJAB�+AA&�A@5?A?��A>��A=�-A=S�A<�HA<�\A<9XA;��A;��A;`BA;�A:z�A8�A7ƨA6�A4��A3�
A3�7A3C�A2��A2�yA2�`A2�/A2ȴA2�jA2�+A1�^A1%A0ĜA01'A.�HA.I�A-�A,�9A,=qA,A+K�A)��A(�A&jA%O�A#ƨA#G�A"��A"JA��An�A��A�A(�A��A��A��A�A�
A�AȴA��A�+AJAdZA��A��A~�A��AA�A
�A	/A�A�\A-A�^AC�AZAA�A�/AjA  A�hA&�AE�A V@�
=@�O�@���@�+@�ȴ@�~�@�{@�hs@���@�n�@��@�l�@�+@�^5@�9@�(�@�ȴ@��@�j@�=q@�{@��@柾@�5?@噚@�@�\)@�p�@���@���@�1@���@���@�bN@�ƨ@�~�@ԓu@�l�@�E�@�7L@��@��/@ЋD@�9X@��
@�
=@�$�@̋D@�33@�G�@��`@Ȭ@�(�@�E�@��@��@�=q@���@�5?@�A�@��D@�$�@�hs@��j@��D@�|�@���@��`@�Z@��^@��@��@��P@��F@��u@�  @�ƨ@��y@��w@�o@�?}@�ƨ@��@�{@�$�@���@�
=@�"�@��@�ff@�E�@�J@��-@�/@�bN@�|�@��@�l�@���@���@�A�@�O�@�^5@��y@�
=@���@�7L@���@��@��\@���@��@��@��u@��w@�"�@���@�^5@�@���@��`@���@�(�@��;@�|�@��H@���@�=q@���@��j@�\)@�v�@�E�@��@��9@�p�@���@�p�@�O�@�G�@���@��9@��9@��9@��j@��j@��@�A�@��!@�G�@��D@�1'@���@�o@�{@���@�x�@�hs@�G�@���@�Z@���@�+@���@���@��+@�^5@��#@�`B@���@��/@��j@��D@�I�@���@�t�@�@���@��@��@��!@��!@�{@�p�@��@�?}@�&�@���@�  @���@�|�@�"�@�ȴ@�E�@�{@��#@��@��#@�@��@��D@�9X@�j@�j@�bN@�9X@\)@~��@}�@}`B@|z�@|�@{33@y�7@x �@wl�@v�@w�@v�@v�+@vff@vff@vff@vV@u�-@up�@uV@t��@t(�@s��@r��@r~�@r^5@r-@rJ@q��@q��@r-@r�\@r�H@s�F@s"�@q��@rJ@rJ@q�@r�@r^5@r=q@r=q@q�#@qG�@q&�@q&�@q&�@q&�@q�@q�@q&�@p�9@pQ�@o�@o�P@o\)@o
=@n�R@nff@nV@n5?@m�T@m?}@l��@k��@kt�@kdZ@kC�@k33@k"�@ko@j�H@j�!@j~�@jM�@i��@i��@ihs@i7L@i�@h�`@h�@h  @hb@hb@h  @g��@gK�@f�R@f$�@e�@e@e��@e`B@e�@d�@d�@d�D@d�D@dz�@dI�@c�
@c��@co@b��@b�\@b�@a��@a��@ax�@`�u@_K�@_�@_
=@^��@^V@^V@^V@^E�@^$�@]@]�h@]`B@\�/@\�j@\��@\j@\I�@\�@[��@[dZ@[C�@["�@Z��@Z^5@ZM�@Z=q@Y��@Y�7@YG�@Xr�@X  @W��@V��@Vff@VV@U�-@U?}@UV@T��@T��@TZ@S�m@S�
@S�
@S��@So@R�H@R�!@R-@Q��@Qhs@Q7L@Q%@Pb@O�w@OK�@O
=@N�y@Nȴ@N��@Nff@NE�@N$�@M�T@M@M�h@M?}@M�@L��@LZ@LI�@L9X@L�@L1@L1@K�m@KdZ@J��@I��@H�`@H�9@H�u@H�@Hr�@HQ�@H  @G�w@G\)@G;d@F��@F5?@E��@E�h@Ep�@E/@EV@D�@D�D@DZ@D�@D1@C��@Cƨ@C�F@C�F@C��@C��@C�@Ct�@Ct�@CS�@Co@B�@B�@B�H@BM�@A�#@AX@@��@@�9@@1'@@b@?�@?�w@?;d@>��@>ȴ@>��@>ff@>@=�T@=�T@=��@=@=�h@=�@=`B@=/@<��@<j@<I�@<1@;�F@;S�@;"�@:��@:^5@:-@:J@9��@9x�@9G�@8��@8bN@8  @7�P@7�@6��@6�y@6�@6�R@6{@5�@5O�@4��@4��@4�D@4Z@4(�@3�m@3t�@3@2��@2��@2n�@2M�@2-@2�@1�^@1��@1��@1x�@17L@0��@0��@0r�@0bN@0Q�@0 �@/�P@/\)@/+@.�y@.�R@.�+@.5?@.{@-��@-�@-`B@-/@-V@,�/@,z�@,(�@+��@+dZ@+C�@+33@+@*��@*�\@*~�@*^5@*M�@*M�@*-@)�#@)�^@)7L@(��@(1'@'�@'K�@&�y@&�R@&��@&V@&E�@&$�@&{@&@%�h@%`B@%?}@%/@%/@%�@%�@%V@$�@$�/@$�j@$�D@$j@$9X@#��@#�F@#S�@"�!@"M�@!��@!�^@!��@!��@!��@!hs@!7L@ Ĝ@ bN@  �@   @   @�@��@�P@K�@�y@�@��@V@�@�T@��@�h@`B@�@�@Z@9X@(�@�m@dZ@C�@"�@��@M�@=q@�@J@��@�#@�7@Ĝ@bN@1'@ �@b@�@��@�P@��@��@�P@\)@�@ȴ@ff@$�@{@�@�@O�@�@�D@1@t�@C�@�@~�@-@�@��@hs@7L@�@�`@��@Ĝ@�9@��@Q�@�@��@�P@|�@l�@\)@+@�R@V@��@�h@`B@��@�@�@��@��@��@�D@I�@��@�
@ƨ@S�@
�H@
��@
��@
n�@
=q@	��@	�#@	��@	��@	��@	��@	�^@	�7@	7L@	�@��@��@��@��@Ĝ@�9@�u@�@r�@r�@A�@  @�@�;@��@�w@�@l�@K�@;d@+@
=@ȴ@��@ff@�@�h@`B@�@V@�@V@�@�/@��@��@�D@z�@j@I�@(�@(�@�@(�@�@�@�@ƨ@S�@"�@�@��@J@�^@��@��@�7@x�@x�@X@7L@ ��@ Ĝ@ �u@ bN@ 1'@ b?��w?�;d?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѴ9AѶFAѲ-Aљ�Aї�AѓuAѓuAѓuAѓuAѓuAѓuAѓuAѕ�Aѕ�Aѕ�Aѕ�AыDAч+AуA�\)A�E�A� �A��A�ƨA�ffAκ^A�  Aɥ�A�dZA���A�-A��A��A��A�A���A�$�A��uA��;A�v�A�oA��uA�9XA���A�ȴA�33A��uA�A�A�`BA���A�?}A��hA�-A�S�A��A��TA�"�A�M�A�1'A�M�A�?}A���A�M�A��FA�-A�$�A��A��A��-A�bNA���A�ffA�l�A�G�A���A��A�|�A�z�A�\)A��jA��hA��A�ƨA�I�A�{A���A�|�A�E�A��A}oAz�AyVAx�9AwAu��At~�As��AqƨAp  AoC�An�\Am\)Al�`AlbNAj��Ah�uAf��Ae�7AeO�AeoAd��Ad$�AcS�Ab��Aa�Aa7LA`E�A_hsA^�A]�-A];dA\��A[ƨAZ�AXAVbNAV9XAU?}AU�AT�9AS�AR��AP��AP-AO��ANffAM��AL��AL��ALVAK�AK�hAK?}AJ �AH�jAG�PAGK�AGVAE�wAD�!ADJAB�+AA&�A@5?A?��A>��A=�-A=S�A<�HA<�\A<9XA;��A;��A;`BA;�A:z�A8�A7ƨA6�A4��A3�
A3�7A3C�A2��A2�yA2�`A2�/A2ȴA2�jA2�+A1�^A1%A0ĜA01'A.�HA.I�A-�A,�9A,=qA,A+K�A)��A(�A&jA%O�A#ƨA#G�A"��A"JA��An�A��A�A(�A��A��A��A�A�
A�AȴA��A�+AJAdZA��A��A~�A��AA�A
�A	/A�A�\A-A�^AC�AZAA�A�/AjA  A�hA&�AE�A V@�
=@�O�@���@�+@�ȴ@�~�@�{@�hs@���@�n�@��@�l�@�+@�^5@�9@�(�@�ȴ@��@�j@�=q@�{@��@柾@�5?@噚@�@�\)@�p�@���@���@�1@���@���@�bN@�ƨ@�~�@ԓu@�l�@�E�@�7L@��@��/@ЋD@�9X@��
@�
=@�$�@̋D@�33@�G�@��`@Ȭ@�(�@�E�@��@��@�=q@���@�5?@�A�@��D@�$�@�hs@��j@��D@�|�@���@��`@�Z@��^@��@��@��P@��F@��u@�  @�ƨ@��y@��w@�o@�?}@�ƨ@��@�{@�$�@���@�
=@�"�@��@�ff@�E�@�J@��-@�/@�bN@�|�@��@�l�@���@���@�A�@�O�@�^5@��y@�
=@���@�7L@���@��@��\@���@��@��@��u@��w@�"�@���@�^5@�@���@��`@���@�(�@��;@�|�@��H@���@�=q@���@��j@�\)@�v�@�E�@��@��9@�p�@���@�p�@�O�@�G�@���@��9@��9@��9@��j@��j@��@�A�@��!@�G�@��D@�1'@���@�o@�{@���@�x�@�hs@�G�@���@�Z@���@�+@���@���@��+@�^5@��#@�`B@���@��/@��j@��D@�I�@���@�t�@�@���@��@��@��!@��!@�{@�p�@��@�?}@�&�@���@�  @���@�|�@�"�@�ȴ@�E�@�{@��#@��@��#@�@��@��D@�9X@�j@�j@�bN@�9X@\)@~��@}�@}`B@|z�@|�@{33@y�7@x �@wl�@v�@w�@v�@v�+@vff@vff@vff@vV@u�-@up�@uV@t��@t(�@s��@r��@r~�@r^5@r-@rJ@q��@q��@r-@r�\@r�H@s�F@s"�@q��@rJ@rJ@q�@r�@r^5@r=q@r=q@q�#@qG�@q&�@q&�@q&�@q&�@q�@q�@q&�@p�9@pQ�@o�@o�P@o\)@o
=@n�R@nff@nV@n5?@m�T@m?}@l��@k��@kt�@kdZ@kC�@k33@k"�@ko@j�H@j�!@j~�@jM�@i��@i��@ihs@i7L@i�@h�`@h�@h  @hb@hb@h  @g��@gK�@f�R@f$�@e�@e@e��@e`B@e�@d�@d�@d�D@d�D@dz�@dI�@c�
@c��@co@b��@b�\@b�@a��@a��@ax�@`�u@_K�@_�@_
=@^��@^V@^V@^V@^E�@^$�@]@]�h@]`B@\�/@\�j@\��@\j@\I�@\�@[��@[dZ@[C�@["�@Z��@Z^5@ZM�@Z=q@Y��@Y�7@YG�@Xr�@X  @W��@V��@Vff@VV@U�-@U?}@UV@T��@T��@TZ@S�m@S�
@S�
@S��@So@R�H@R�!@R-@Q��@Qhs@Q7L@Q%@Pb@O�w@OK�@O
=@N�y@Nȴ@N��@Nff@NE�@N$�@M�T@M@M�h@M?}@M�@L��@LZ@LI�@L9X@L�@L1@L1@K�m@KdZ@J��@I��@H�`@H�9@H�u@H�@Hr�@HQ�@H  @G�w@G\)@G;d@F��@F5?@E��@E�h@Ep�@E/@EV@D�@D�D@DZ@D�@D1@C��@Cƨ@C�F@C�F@C��@C��@C�@Ct�@Ct�@CS�@Co@B�@B�@B�H@BM�@A�#@AX@@��@@�9@@1'@@b@?�@?�w@?;d@>��@>ȴ@>��@>ff@>@=�T@=�T@=��@=@=�h@=�@=`B@=/@<��@<j@<I�@<1@;�F@;S�@;"�@:��@:^5@:-@:J@9��@9x�@9G�@8��@8bN@8  @7�P@7�@6��@6�y@6�@6�R@6{@5�@5O�@4��@4��@4�D@4Z@4(�@3�m@3t�@3@2��@2��@2n�@2M�@2-@2�@1�^@1��@1��@1x�@17L@0��@0��@0r�@0bN@0Q�@0 �@/�P@/\)@/+@.�y@.�R@.�+@.5?@.{@-��@-�@-`B@-/@-V@,�/@,z�@,(�@+��@+dZ@+C�@+33@+@*��@*�\@*~�@*^5@*M�@*M�@*-@)�#@)�^@)7L@(��@(1'@'�@'K�@&�y@&�R@&��@&V@&E�@&$�@&{@&@%�h@%`B@%?}@%/@%/@%�@%�@%V@$�@$�/@$�j@$�D@$j@$9X@#��@#�F@#S�@"�!@"M�@!��@!�^@!��@!��@!��@!hs@!7L@ Ĝ@ bN@  �@   @   @�@��@�P@K�@�y@�@��@V@�@�T@��@�h@`B@�@�@Z@9X@(�@�m@dZ@C�@"�@��@M�@=q@�@J@��@�#@�7@Ĝ@bN@1'@ �@b@�@��@�P@��@��@�P@\)@�@ȴ@ff@$�@{@�@�@O�@�@�D@1@t�@C�@�@~�@-@�@��@hs@7L@�@�`@��@Ĝ@�9@��@Q�@�@��@�P@|�@l�@\)@+@�R@V@��@�h@`B@��@�@�@��@��@��@�D@I�@��@�
@ƨ@S�@
�H@
��@
��@
n�@
=q@	��@	�#@	��@	��@	��@	��@	�^@	�7@	7L@	�@��@��@��@��@Ĝ@�9@�u@�@r�@r�@A�@  @�@�;@��@�w@�@l�@K�@;d@+@
=@ȴ@��@ff@�@�h@`B@�@V@�@V@�@�/@��@��@�D@z�@j@I�@(�@(�@�@(�@�@�@�@ƨ@S�@"�@�@��@J@�^@��@��@�7@x�@x�@X@7L@ ��@ Ĝ@ �u@ bN@ 1'@ b?��w?�;d?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BT�BT�BT�BS�BS�BS�BS�BS�BT�BS�BT�BT�BT�BT�BT�BT�BT�BS�BS�BT�BT�BW
BYBW
BT�BS�BN�BO�BN�B;dB0!B�BJB��B��B�B�fB��B�RB��B�\Bu�BO�BB�B;dB>wB:^B/B"�B�B�BbBB��B�fB�B��B��BB�XB��B�VBx�Bs�Bk�Bp�Bv�Bu�Br�Bn�BhsBR�B@�B=qB5?B'�B�BDB
�5B
��B
��B
��B
ǮB
�}B
�dB
�?B
�-B
�B
��B
�{B
�+B
v�B
s�B
hsB
`BB
ZB
W
B
J�B
;dB
5?B
0!B
&�B
"�B
�B
bB
  B	�B	�sB	�fB	�ZB	�NB	�5B	�B	��B	��B	ȴB	ÖB	�qB	�RB	�'B	�B	�B	��B	�{B	�B	r�B	p�B	o�B	r�B	p�B	n�B	k�B	gmB	_;B	[#B	VB	N�B	K�B	H�B	G�B	E�B	C�B	A�B	<jB	5?B	/B	-B	/B	-B	"�B	�B	�B	hB	PB	PB	PB		7B	+B	%B	%B	B	B	B	B	  B��B��B��B�B�`B�TB�HB�HB�BB�;B�;B�;B�;B�5B�;B�/B�B�
B�
B��BɺBƨB��B�jB�^B�LB�-B�B��B��B��B��B��B��B��B�hB�PB�DB�BaHB9XBs�Bp�Bm�BiyBhsBgmBe`BaHB_;B]/BYBW
BR�BL�BJ�BE�BE�BD�BC�BB�BA�B?}B=qB;dB:^B8RB7LB6FB49B33B-B,B+B(�B'�B'�B&�B&�B%�B$�B$�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B$�B"�B"�B"�B"�B%�B%�B!�B�B�B�B�B�B�B�B�B�B�B!�B'�B49B<jB;dB;dB=qBF�BG�BG�BH�BA�B?}BA�BB�BC�BF�BI�BM�BP�BQ�BR�BR�BQ�BR�BQ�BO�BO�BM�BK�BQ�BT�BXB\)BcTBl�Bs�By�B|�B~�B�B�+B�1B�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�dB�jBBBĜBŢBŢBƨBȴB��B��B�B�BB�HB�TB�ZB�`B�fB�mB�sB�sB�yB�B�B�B��B��B��B��B��B��B	B	B		7B		7B	
=B	DB	JB	VB	hB	�B	�B	�B	�B	�B	�B	"�B	!�B	$�B	#�B	#�B	"�B	"�B	&�B	+B	-B	/B	1'B	33B	6FB	9XB	<jB	=qB	A�B	A�B	@�B	B�B	B�B	C�B	B�B	C�B	B�B	C�B	D�B	E�B	F�B	H�B	I�B	J�B	K�B	O�B	W
B	\)B	^5B	_;B	`BB	bNB	cTB	cTB	cTB	cTB	e`B	ffB	iyB	p�B	q�B	r�B	r�B	s�B	w�B	z�B	� B	�B	�%B	�DB	�VB	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�FB	�FB	�FB	�LB	�XB	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
VB
\B
\B
\B
\B
\B
bB
\B
bB
bB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
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
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
A�B
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
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
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
H�B
H�B
H�B
H�B
H�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
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
M�B
M�B
M�B
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
P�B
R�B
R�B
S�B
S�B
S�B
S�B
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
W
B
W
B
XB
XB
YB
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
]/B
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
bNB
bNB
cTB
cTB
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
hsB
hsB
hsB
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
jB
jB
jB
jB
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
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
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
q�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BUBU2BU2BTBTBS�BTBS�BUBS�BUBT�BUBT�BUBUBU2BT,BTaBUMBUgBW�BY�BX�BX�BX�BY�B\�BZQBC�B8�B�BbB��B��B�B�B��B��B��B�gB|6BSuBD�B=B@OB="B1B$&B �B�B@BEB�B�B�)B�B�pB��B�6B�B�BzDBt�Bk�Bp�BwLBvzBs�Bp;Bk�BT�BAoB?B6�B)�B �BBB
��B
�YB
��B
�uB
ȴB
�OB
�jB
��B
�3B
��B
��B
��B
�7B
w�B
u�B
i�B
a�B
[�B
YKB
L�B
<�B
6FB
1vB
'�B
$B
!-B
�B
AB	��B	��B	��B	��B	�:B	�VB	��B	��B	��B	�	B	ĶB	��B	�rB	��B	�B	��B	�$B	��B	��B	shB	q�B	p;B	s�B	r-B	o�B	m�B	hsB	`\B	\�B	V�B	O�B	LdB	IRB	HfB	FYB	D�B	C-B	>(B	6�B	/�B	-�B	0�B	.}B	$&B	!�B	WB	�B	VB	�B	pB		�B	�B	�B	�B	�B	�B	�B	�B	;B	 �B��B��B�B�B��B��B�B��B�VB�pBߊBߊB��B�BB�5B��B�EBؓB��B��B��B�UB�<B��B�rB�TB�AB��B��B��B��B��B��B�+B��B��B��B�G�O�G�O�BuZBr-Bn�Bi�BiyBi�BfLBb�Ba-B^�BZ7BXyBUBO(BLdBF%BF?BESBDMBC�BB�B@iB>]B;�B;0B9	B88B72B5�B5�B.}B-]B,B)_B(sB(XB'mB'�B&�B&�B&LB"NB 'B�B�BOB�BxB�B�BB�B�B�BBSB�B�B�B�B�B�B�B \BjB�BB�B�BVBBBB5BOBjB�B�B �B%�B%`B#:B#�B$@B#�B&�B'mB#nBIBpB!B#BQB=B)B�B �B�B!�B'�B4�B=VB;�B;dB=VBG+BH1BH�BJ�BBuB@�BB�BCGBD3BF�BI�BM�BQBRTBS[BS&BRTBSuBR�BP�BP}BN<BK�BQ�BT�BW�B[�Bb�BlqBs�Bz�B~B� B��B��B��B��B��B��B�$B�#B��B��B�B�
B�B��B�/B�B�ZB�ZB�4B�NB��B��B��B�pB�BB��B�$B��B�-B��B��B��B��B��B��BżB��B��B�6BԕB�QB�B��B�B��B�B�B�B�B�B��B�B�/B�GB�+B�B�	B�*B�6B�qB	�B	�B		RB		lB	
rB	�B	�B	�B	�B	�B	�B	�B	�B	B	 BB	#:B	!�B	%B	$&B	$ZB	#nB	# B	'RB	+kB	-]B	/�B	1vB	3hB	6`B	9�B	<�B	=�B	BAB	A�B	@�B	B�B	B�B	C�B	B�B	C�B	B�B	C�B	EB	FB	G+B	IlB	J=B	K)B	LB	O�B	W?B	\]B	^OB	_VB	`vB	bhB	c�B	c�B	c�B	c�B	e�B	f�B	i�B	p�B	q�B	r�B	r�B	s�B	w�B	z�B	� B	�B	�%B	��B	��B	�jB	�hB	�{B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�*B	��B	�0B	�QB	�5B	�UB	�aB	�hB	�hB	�nB	�tB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	�B	�	B	��B	��B	��B	�B	�(B	�B	�:B	�B	�B	�B	�,B	�9B	�9B	�B	�9B	�$B	�$B	�?B	�YB	�+B	�eB	�QB	�QB	�qB	�]B	�dB	�~B	ݲB	޸B	�VB	�vB	�B	�B	�nB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�B	�B	�2B	�B	�B	�B	�6B	�"B	�"B	�(B	�(B	�B
 B
 B
 4B
UB
AB
-B
gB
SB
YB
?B
zB
�B
	lB
	�B

rB
xB
^B
^B
^B
~B
~B
~B
~B
�B
�B
�B
�B
vB
vB
vB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 B
!B
!�B
!�B
# B
#�B
$B
$�B
%B
&B
%�B
%�B
'B
'B
(
B
(
B
(
B
($B
($B
(
B
($B
($B
)DB
*0B
*0B
*0B
*0B
+6B
+6B
,=B
,=B
-CB
-CB
-CB
./B
.IB
/iB
/iB
0UB
0oB
1[B
1[B
1AB
1[B
1[B
2|B
2aB
3hB
3hB
4nB
4nB
4TB
5tB
5tB
5�B
6`B
6`B
6zB
6zB
7�B
7�B
7�B
7�B
8�B
8lB
8lB
8�B
8lB
9�B
9�B
:xB
:xB
:�B
:�B
;�B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
A�B
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
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
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
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
KB
KB
K�B
K�B
K�B
L�B
L�B
L�B
MB
MB
MB
MB
MB
MB
L�B
MB
MB
L�B
MB
MB
MB
L�B
NB
NB
M�B
M�B
NB
NB
M�B
NB
N"B
N�B
OB
OB
O(B
N�B
OB
PB
P.B
O�B
PB
O�B
PB
O�B
P.B
QNB
S&B
S&B
TB
TB
T,B
TB
TB
S�B
S�B
T,B
U2B
UB
U2B
U2B
V9B
VB
V9B
VSB
W?B
WYB
X_B
X_B
YeB
Z7B
Z7B
ZkB
[=B
[WB
[=B
\]B
\]B
\]B
\]B
]IB
]/B
]IB
]IB
]dB
]dB
^OB
^OB
^5B
^OB
^OB
^jB
^�B
_�B
_�B
`vB
a|B
a�B
a|B
abB
bhB
bNB
bNB
b�B
b�B
bhB
b�B
c�B
c�B
dtB
d�B
dtB
ezB
ezB
ezB
e�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
gmB
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
hsB
h�B
h�B
h�B
i�B
i�B
i�B
iyB
i�B
i�B
j�B
j�B
j�B
j�B
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
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
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
q�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</O<L��<5ު<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609160035312016091600353120160916003531201806221213502018062212135020180622121350201804050406272018040504062720180405040627  JA  ARFMdecpA19c                                                                20160912093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160912003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160912003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160912003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160912003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160912003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160912003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160912003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160912003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160912003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20160912012319                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160912153326  CV  JULD            G�O�G�O�F�Q�                JM  ARGQJMQC2.0                                                                 20160912153326  CV  JULD_LOCATION   G�O�G�O�F�Q�                JM  ARGQJMQC2.0                                                                 20160912153326  CV  LATITUDE        G�O�G�O�A�t�                JM  ARGQJMQC2.0                                                                 20160912153326  CV  LONGITUDE       G�O�G�O��#]�                JM  ARSQJMQC2.0                                                                 20160913000000  CF  PSAL_ADJUSTED_QCC�  C�  G�O�                JM  ARCAJMQC2.0                                                                 20160915153531  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160915153531  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190627  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031350  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                