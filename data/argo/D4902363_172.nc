CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-24T21:35:11Z creation;2017-10-24T21:35:14Z conversion to V3.1;2019-12-19T07:58:23Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171024213511  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_172                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�03�� 1   @�03��� @:���o�d~3���1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A��A   AA��A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�33@�ffA��A?33A]��A}��A���A���A���A���A���A���A���A���BffB  BffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B�� B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3C� CٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:p D:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D�� D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�x 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aͺ^Aʹ9Aʹ9AͲ-Aͩ�AͮA͸RAͺ^AͼjAͼjAͼjA���Aͺ^AͼjAͼjAͼjA�A�ȴA�ƨA�ƨA�ȴA���A�ȴA�ȴA���A���A���A�ȴA�ȴA�ȴA�ƨA���A;wA͟�A���A�I�A���A�$�A��FA��+A�
=A��-A���A��#A�
=A�~�A�A�p�A�dZA�7LA�jA��A��A�I�A��A�%A�hsA�l�A�
=A��#A�K�A��`A�JA��A��7A��`A��DA��/A�~�A�l�A�A���A��A�C�A���A�A�"�A��A�z�A��A���A�M�A�?}A��jA�A�A��9A�1'A�I�A��A��wA�=qA���A�VA�ĜA�&�A�dZA�A�|�A�(�A�A~r�A~{A|�`A{l�Az�DAy`BAx5?Aw�PAv�RAv{At�As��Ar�yAq��AqS�Ap�`Ao��Ao&�An��Am�Akx�Ai�TAi"�Ah�HAh~�Ag�FAf$�AehsAd��AcoAb��Ab(�Aa�-Aa%A`(�A_�FA_S�A^�A]7LA\~�A[O�AZ �AYl�AW�;AV�HAU�AU��ASARQ�AQ\)AP^5AO��AO\)AN��AM��AM�AL�DAK��AJ�RAJ�AI�AHn�AG�^AF$�AD��ADAC|�AB�ABZAA�AA?}A@��A@�+A@A�A>�/A=�A=��A=C�A=�A<��A<�`A<��A<��A<��A<jA;�mA:��A:{A9�
A9dZA8jA7��A7�A6�A6bA5S�A3&�A1�^A0z�A/�A.n�A-�A-�7A-%A,��A,ĜA,�!A,^5A,bA+ƨA+\)A*ĜA)�TA(��A($�A'�PA&~�A%��A%S�A%A#�^A#G�A"��A"��A"9XA!dZA bNA��AhsAVAI�A��AdZA�A�A33A�9Al�A��A��AS�AȴA �Al�A�A�mA��A��AA��A�TA^5A%A
  A��A��A=qA%Ap�AffAM�A=qA$�A�A�
A��A �/A �!A �D@��
@��w@��@���@���@�S�@���@�=q@�9@��@�$�@�?}@�@�%@�(�@�1@�P@�=q@�x�@�@��@��/@�|�@�ȴ@�G�@߾w@޸R@��#@���@��m@ۍP@�;d@��y@ڗ�@��T@�bN@ָR@թ�@�7L@Լj@�33@�  @͉7@��@�+@§�@�G�@��u@��m@�@�&�@�z�@��R@���@�p�@��@�n�@���@���@��@�bN@���@��\@��@���@�@��-@�A�@�5?@�z�@��@�M�@���@��@��@��u@���@���@�~�@�/@��u@�A�@�@�1@�l�@�^5@�J@��^@�O�@���@�(�@�|�@�S�@�+@�@�ff@��@�&�@���@���@�j@�  @�\)@�o@��R@�5?@��@�&�@�V@���@��`@��9@�ƨ@�;d@�
=@�v�@��-@�G�@��@��@���@�z�@�A�@��@���@�o@�n�@�5?@���@��-@�x�@�&�@���@���@�r�@�A�@��m@���@�S�@�K�@�C�@�+@�o@�@��H@��+@�-@��T@��#@���@�`B@�/@��@���@��D@�I�@��@���@�dZ@�o@��+@�$�@���@��@��/@��@��@�r�@�j@�j@�j@�I�@��@��@��
@�ƨ@��@��P@�S�@��y@��R@��R@���@���@�v�@�^5@�M�@�E�@���@���@�x�@�&�@�%@���@��@�1'@�@�w@�P@\)@~�y@~��@}p�@|�@{��@{S�@{33@{33@{33@{33@{33@{33@{33@{"�@z�@z��@z�!@y��@x��@x �@w��@v��@u�h@u/@uV@t��@t�/@t�D@tI�@t9X@s�
@s"�@r�!@rM�@q�@qhs@q�@q�@p�@oK�@n�R@nE�@m��@m@m�-@m��@mO�@lz�@lj@l�/@lI�@k��@kC�@k@i��@h��@h�@g�;@g�P@g\)@g
=@f��@fȴ@f��@fV@e�@e�h@ep�@e`B@eO�@e?}@e/@e�@dZ@cC�@b��@b�\@bM�@a��@a��@a��@a�7@a��@a��@a��@a��@a��@a�^@a�^@a�@b��@b��@bJ@a��@a�^@a��@ax�@a7L@`�`@`��@_��@_�P@^��@^�@_�@^��@^ȴ@^v�@^E�@]�T@]�-@^ff@^v�@^$�@^{@]@\��@\j@[��@[�F@[dZ@["�@Z�@Z��@Zn�@ZJ@Y�#@Yx�@Y7L@Y�@XĜ@X��@X�u@W�;@Wl�@W�@V��@Vff@V{@U�h@T�D@TZ@T(�@S�F@S�@SdZ@SS�@SS�@S33@SS�@S��@S�F@S�F@S��@S��@So@Q�@R�@Q�7@QG�@P��@PĜ@P�9@PĜ@PĜ@Pr�@N�y@M�@M/@M�@L��@LZ@L9X@L(�@L1@K�
@K33@J�\@JM�@J�@JJ@I�@I��@I��@I7L@I�@H��@H1'@H1'@Hb@G�;@G�w@Gl�@F��@FE�@F$�@E@E�@E`B@E?}@E�@D�/@D��@Dj@DI�@D(�@CC�@Bn�@B^5@B=q@Ax�@A%@@��@@�`@@��@@r�@@  @?�@?\)@?�@>��@>5?@>@=�-@<�D@:M�@;@9�7@8  @7��@7�;@7�@7�@7K�@7�@6�y@6�@6ȴ@65?@5�@5O�@4�j@4��@4z�@4j@49X@4�@41@3�m@3C�@3@2�@2�H@2��@2^5@2=q@1�#@1hs@1�@0bN@/�@/�w@/�@/��@/��@/|�@/;d@.�R@-�@-O�@,�j@,Z@,�@+�m@+�@+S�@+S�@*��@*�\@*~�@*~�@*M�@*�@)�@)�#@)�7@(�`@(��@(�u@(A�@'�;@'�w@'�@'��@'|�@'|�@'K�@';d@'\)@&ȴ@&v�@&v�@&5?@%�-@%`B@%/@%/@%/@%/@%V@$��@$j@$�@#��@#��@#��@#��@#C�@#S�@"�H@"��@"=q@"-@"=q@!��@!��@!hs@ A�@ 1'@ A�@ A�@  �@ b@   @�@�@�@�;@l�@��@ȴ@v�@E�@�T@�-@�-@p�@�@V@�@z�@I�@(�@��@@�@��@^5@J@J@��@��@��@�@�7@X@�@�`@Ĝ@Ĝ@�9@��@��@r�@b@�;@��@�@�P@�P@|�@\)@+@��@�y@�y@ȴ@�R@��@V@E�@5?@$�@$�@�@@��@O�@�@�D@j@�@1@�m@ƨ@ƨ@ƨ@��@��@�@dZ@C�@33@33@33@"�@�@�!@^5@�@�7@G�@7L@&�@��@�9@bN@A�@1'@b@�w@l�@+@
=@�y@�R@�+@V@�@@�@?}@/@/@�@�@V@��@�/@�D@9X@(�@(�@1@��@�m@��@C�@33@"�@@
��@
n�@	��@	��@	��@	x�@	hs@	7L@	�@	%@	%@	%@��@�`@�u@�@r�@A�@  @��@+@
=@��@��@��@�@ȴ@ȴ@��@V@E�@{@��@��@�-@�h@`B@�/@�j@��@��@�D@z�@z�@j@Z@Z@I�@9X@��@��@��@��@�@dZ@C�@"�@"�@@�@�@��@M�@��@�#@�@�#@�^@��@��@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aͺ^Aʹ9Aʹ9AͲ-Aͩ�AͮA͸RAͺ^AͼjAͼjAͼjA���Aͺ^AͼjAͼjAͼjA�A�ȴA�ƨA�ƨA�ȴA���A�ȴA�ȴA���A���A���A�ȴA�ȴA�ȴA�ƨA���A;wA͟�A���A�I�A���A�$�A��FA��+A�
=A��-A���A��#A�
=A�~�A�A�p�A�dZA�7LA�jA��A��A�I�A��A�%A�hsA�l�A�
=A��#A�K�A��`A�JA��A��7A��`A��DA��/A�~�A�l�A�A���A��A�C�A���A�A�"�A��A�z�A��A���A�M�A�?}A��jA�A�A��9A�1'A�I�A��A��wA�=qA���A�VA�ĜA�&�A�dZA�A�|�A�(�A�A~r�A~{A|�`A{l�Az�DAy`BAx5?Aw�PAv�RAv{At�As��Ar�yAq��AqS�Ap�`Ao��Ao&�An��Am�Akx�Ai�TAi"�Ah�HAh~�Ag�FAf$�AehsAd��AcoAb��Ab(�Aa�-Aa%A`(�A_�FA_S�A^�A]7LA\~�A[O�AZ �AYl�AW�;AV�HAU�AU��ASARQ�AQ\)AP^5AO��AO\)AN��AM��AM�AL�DAK��AJ�RAJ�AI�AHn�AG�^AF$�AD��ADAC|�AB�ABZAA�AA?}A@��A@�+A@A�A>�/A=�A=��A=C�A=�A<��A<�`A<��A<��A<��A<jA;�mA:��A:{A9�
A9dZA8jA7��A7�A6�A6bA5S�A3&�A1�^A0z�A/�A.n�A-�A-�7A-%A,��A,ĜA,�!A,^5A,bA+ƨA+\)A*ĜA)�TA(��A($�A'�PA&~�A%��A%S�A%A#�^A#G�A"��A"��A"9XA!dZA bNA��AhsAVAI�A��AdZA�A�A33A�9Al�A��A��AS�AȴA �Al�A�A�mA��A��AA��A�TA^5A%A
  A��A��A=qA%Ap�AffAM�A=qA$�A�A�
A��A �/A �!A �D@��
@��w@��@���@���@�S�@���@�=q@�9@��@�$�@�?}@�@�%@�(�@�1@�P@�=q@�x�@�@��@��/@�|�@�ȴ@�G�@߾w@޸R@��#@���@��m@ۍP@�;d@��y@ڗ�@��T@�bN@ָR@թ�@�7L@Լj@�33@�  @͉7@��@�+@§�@�G�@��u@��m@�@�&�@�z�@��R@���@�p�@��@�n�@���@���@��@�bN@���@��\@��@���@�@��-@�A�@�5?@�z�@��@�M�@���@��@��@��u@���@���@�~�@�/@��u@�A�@�@�1@�l�@�^5@�J@��^@�O�@���@�(�@�|�@�S�@�+@�@�ff@��@�&�@���@���@�j@�  @�\)@�o@��R@�5?@��@�&�@�V@���@��`@��9@�ƨ@�;d@�
=@�v�@��-@�G�@��@��@���@�z�@�A�@��@���@�o@�n�@�5?@���@��-@�x�@�&�@���@���@�r�@�A�@��m@���@�S�@�K�@�C�@�+@�o@�@��H@��+@�-@��T@��#@���@�`B@�/@��@���@��D@�I�@��@���@�dZ@�o@��+@�$�@���@��@��/@��@��@�r�@�j@�j@�j@�I�@��@��@��
@�ƨ@��@��P@�S�@��y@��R@��R@���@���@�v�@�^5@�M�@�E�@���@���@�x�@�&�@�%@���@��@�1'@�@�w@�P@\)@~�y@~��@}p�@|�@{��@{S�@{33@{33@{33@{33@{33@{33@{33@{"�@z�@z��@z�!@y��@x��@x �@w��@v��@u�h@u/@uV@t��@t�/@t�D@tI�@t9X@s�
@s"�@r�!@rM�@q�@qhs@q�@q�@p�@oK�@n�R@nE�@m��@m@m�-@m��@mO�@lz�@lj@l�/@lI�@k��@kC�@k@i��@h��@h�@g�;@g�P@g\)@g
=@f��@fȴ@f��@fV@e�@e�h@ep�@e`B@eO�@e?}@e/@e�@dZ@cC�@b��@b�\@bM�@a��@a��@a��@a�7@a��@a��@a��@a��@a��@a�^@a�^@a�@b��@b��@bJ@a��@a�^@a��@ax�@a7L@`�`@`��@_��@_�P@^��@^�@_�@^��@^ȴ@^v�@^E�@]�T@]�-@^ff@^v�@^$�@^{@]@\��@\j@[��@[�F@[dZ@["�@Z�@Z��@Zn�@ZJ@Y�#@Yx�@Y7L@Y�@XĜ@X��@X�u@W�;@Wl�@W�@V��@Vff@V{@U�h@T�D@TZ@T(�@S�F@S�@SdZ@SS�@SS�@S33@SS�@S��@S�F@S�F@S��@S��@So@Q�@R�@Q�7@QG�@P��@PĜ@P�9@PĜ@PĜ@Pr�@N�y@M�@M/@M�@L��@LZ@L9X@L(�@L1@K�
@K33@J�\@JM�@J�@JJ@I�@I��@I��@I7L@I�@H��@H1'@H1'@Hb@G�;@G�w@Gl�@F��@FE�@F$�@E@E�@E`B@E?}@E�@D�/@D��@Dj@DI�@D(�@CC�@Bn�@B^5@B=q@Ax�@A%@@��@@�`@@��@@r�@@  @?�@?\)@?�@>��@>5?@>@=�-@<�D@:M�@;@9�7@8  @7��@7�;@7�@7�@7K�@7�@6�y@6�@6ȴ@65?@5�@5O�@4�j@4��@4z�@4j@49X@4�@41@3�m@3C�@3@2�@2�H@2��@2^5@2=q@1�#@1hs@1�@0bN@/�@/�w@/�@/��@/��@/|�@/;d@.�R@-�@-O�@,�j@,Z@,�@+�m@+�@+S�@+S�@*��@*�\@*~�@*~�@*M�@*�@)�@)�#@)�7@(�`@(��@(�u@(A�@'�;@'�w@'�@'��@'|�@'|�@'K�@';d@'\)@&ȴ@&v�@&v�@&5?@%�-@%`B@%/@%/@%/@%/@%V@$��@$j@$�@#��@#��@#��@#��@#C�@#S�@"�H@"��@"=q@"-@"=q@!��@!��@!hs@ A�@ 1'@ A�@ A�@  �@ b@   @�@�@�@�;@l�@��@ȴ@v�@E�@�T@�-@�-@p�@�@V@�@z�@I�@(�@��@@�@��@^5@J@J@��@��@��@�@�7@X@�@�`@Ĝ@Ĝ@�9@��@��@r�@b@�;@��@�@�P@�P@|�@\)@+@��@�y@�y@ȴ@�R@��@V@E�@5?@$�@$�@�@@��@O�@�@�D@j@�@1@�m@ƨ@ƨ@ƨ@��@��@�@dZ@C�@33@33@33@"�@�@�!@^5@�@�7@G�@7L@&�@��@�9@bN@A�@1'@b@�w@l�@+@
=@�y@�R@�+@V@�@@�@?}@/@/@�@�@V@��@�/@�D@9X@(�@(�@1@��@�m@��@C�@33@"�@@
��@
n�@	��@	��@	��@	x�@	hs@	7L@	�@	%@	%@	%@��@�`@�u@�@r�@A�@  @��@+@
=@��@��@��@�@ȴ@ȴ@��@V@E�@{@��@��@�-@�h@`B@�/@�j@��@��@�D@z�@z�@j@Z@Z@I�@9X@��@��@��@��@�@dZ@C�@"�@"�@@�@�@��@M�@��@�#@�@�#@�^@��@��@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�
B�B�B��Bk�Br�B]/BS�B7LB)�B)�B'�B!�B9XB;dB'�B�B%B��B��B�B�;B��B�B��B��B��BĜB�3B�LB�-B�B��B��B��B��B��B�%Bm�BgmBhsB`BBVBI�B8RB5?B33B/B&�B{BhBDBB
��B
�B
��B
�B
�B
�NB
��B
�FB
�?B
�B
��B
��B
��B
��B
�VB
�1B
�B
w�B
n�B
ffB
\)B
ZB
VB
P�B
J�B
D�B
@�B
9XB
7LB
2-B
+B
$�B
!�B
�B
JB
%B
1B
%B
B	��B	��B	�B	�B	�mB	�ZB	�NB	�/B	�B	��B	��B	��B	B	�jB	�?B	�!B	��B	��B	��B	��B	�VB	�JB	�B	y�B	w�B	q�B	o�B	n�B	iyB	ffB	aHB	^5B	[#B	S�B	S�B	Q�B	J�B	H�B	?}B	9XB	8RB	5?B	33B	2-B	0!B	.B	+B	)�B	&�B	 �B	�B	�B	�B	�B	�B	�B	�B	{B	uB	oB	VB	+B	B	B	B��B��B��B�B�B�sB�/B��B��B��BŢBƨBƨBĜBĜBŢBĜBB��B�}B�jB�XB�3B�!B�B��B��B��B��B��B��B��B��B��B�{B�\B�DB�JB�JB�DB�%B�B�%B�B{�B{�Bz�Bt�Bp�Bo�Bo�Bl�Bl�Bk�BgmBcTB\)BT�BW
BS�BN�BI�BD�BB�B>wB<jB7LB49B49B5?B?}B?}B?}B=qB=qB;dB7LB9XB6FB(�B'�B/B49B33B33B33B1'B-B)�B)�B'�B"�B�B%�B-B+B(�B,B33B6FB7LB;dB=qB>wBC�BG�BH�BI�BI�BL�BL�BK�BK�BJ�BI�BK�BO�BN�BM�BH�B@�BC�B=qB33B%�B+B.B.B.B+B1'B/B33B9XB8RB9XB;dB;dB<jB>wB<jB9XB?}BB�BC�BA�B;dB:^B?}B@�BG�BI�BJ�BJ�BJ�BK�BN�BR�BR�BZB\)BYBW
BhsBjBs�Bu�Bv�Bx�Bx�Bz�B� B�B�B~�B�B�B�+B�1B�7B�1B�1B�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B�!B�3B�9B�9B�FB�FB�RB�XB�^B�jB��B��BBŢBƨBȴBɺB��B��B��B��B��B�
B�
B�B�B�B�B�B�B�#B�;B�HB�NB�TB�ZB�`B�mB�mB�sB�yB�B�B�B�B��B��B	  B	B	B	%B	+B	+B	+B	+B	
=B	PB	VB	\B	bB	bB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	&�B	,B	.B	0!B	33B	6FB	9XB	:^B	;dB	;dB	=qB	<jB	>wB	E�B	G�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	K�B	L�B	L�B	K�B	O�B	Q�B	S�B	S�B	W
B	\)B	^5B	_;B	_;B	_;B	`BB	aHB	aHB	aHB	dZB	gmB	hsB	jB	k�B	l�B	k�B	k�B	p�B	r�B	s�B	u�B	u�B	u�B	t�B	t�B	x�B	{�B	|�B	|�B	}�B	}�B	}�B	�B	�B	�%B	�7B	�DB	�DB	�PB	�PB	�VB	�VB	�\B	�bB	�oB	�oB	�uB	�uB	�oB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�'B	�-B	�3B	�?B	�FB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	�wB	�}B	��B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�NB	�NB	�NB	�ZB	�`B	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B

=B
DB
JB
JB
JB
JB
PB
PB
VB
VB
VB
\B
oB
oB
hB
oB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
 �B
"�B
$�B
%�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
(�B
+B
,B
-B
-B
-B
-B
-B
-B
.B
.B
-B
-B
.B
/B
.B
/B
0!B
1'B
1'B
1'B
33B
33B
33B
33B
33B
33B
33B
5?B
5?B
6FB
6FB
7LB
8RB
9XB
:^B
;dB
;dB
;dB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
:^B
:^B
;dB
:^B
;dB
;dB
=qB
>wB
>wB
?}B
?}B
>wB
A�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
C�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
I�B
J�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
M�B
N�B
N�B
O�B
O�B
P�B
O�B
O�B
O�B
O�B
O�B
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
S�B
R�B
S�B
S�B
R�B
S�B
T�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
S�B
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
XB
XB
XB
XB
YB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
cTB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
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
dZB
e`B
e`B
ffB
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
gmB
iyB
iyB
hsB
hsB
iyB
iyB
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
l�B
l�B
l�B
l�B
k�B
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
n�B
n�B
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
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�+B�1B�1B�1B�B�B�1B�B�1B�B�B�1B�B�B�1B�B�1B�1B�1B�B�B�B�B�1B�B�B�B�B�1B�1B�_BخB��B�=B�GB��Bq�BxBa�BW$B:DB,WB,B*0B%`B;B>]B+B�BKBB��B�vB�:B�BۦB�B��B͹B�tB�FB�>B��B�B�@B��B�bB��B�G�O�Bp�Bi�Bi�Ba�BW�BK�B:�B6�B49B0!B(XB�B�B�B�B
�B
�B
��B
�B
��B
�ZB
� B
��B
��B
��B
��B
�B
��B
��B
��B
�B
��B
y�B
o�B
g�B
]�B
[	B
W
B
RB
LJB
E�B
A�B
:�B
8B
2�B
,"B
%�B
"�B
#B
(B
�B
	B
�B
�B	�(B	��B	��B	��B	�B	��B	� B	��B	�B	�B	ϑB	˒B	�B	��B	�zB	��B	�yB	��B	��B	��B	��B	�6B	�GB	{�B	y$B	r�B	p�B	oOB	j�B	g�B	b4B	_!B	\B	U�B	UB	S@B	K�B	J	B	A�B	;B	9XB	6+B	4B	2�B	0�B	.�B	+�B	*B	'�B	"�B	�B	CB	B	�B	�B	�B	�B	�B	�B	�B	BB	�B	�B	�B	�B�B��B��B�B�B�B��B��BΊB��B�zB�_B�_B�SB�B��B�B�-B�'B�B�<B�^B��B�vB�CB�B�RB��B�ZB��B�)B�=B�#B�B�gB��B��B�6B��B��B�+B��B��B��B}VB}B|BvzBq�Bp�BpUBm�Bm�Bl�Bh�Bd�B^5BWsBXEBU�BP�BK�BFtBDMB@OB>(B9>B6+B6+B6�B?�B?�B?�B=�B=�B<B8lB:B7fB,qB*B0;B5B4B3�B3�B2B./B+6B*�B(�B$@BjB&�B-]B+�B)�B,�B3MB6zB8lB<PB>BB?�BD�BH�BI�BJrBJrBM6BM6BL0BLJBK�BJ�BMBP�BO\BN�BJ=BB�BE�B?�B5�B(�B,B.�B.�B.�B,WB1�B0;B3�B9�B9rB:^B<B<B<�B>�B=B:^B?�BB�BC�BBB<�B<B@�BA�BH1BJ#BK)BKDBK^BL~BO�BS�BS�BZ�B\�BZkBX�Bh�Bk6BtBv+BwLByXBy�B{B�4B�;B�oB�B��B��B�zB�fB��B��B��B��B��B��B��B�1B��B�B�B�B�jB�TB�XB�sB��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�B�#B�B�B�HB�@B�,B�?B�?B�+B�EB�1B�_B�yB�B�qB�VB�|B�B�B�B�B�B�B�B��B�B� B�!B�B�2B�rB	 4B	AB	MB	YB	EB	_B	_B	_B	
�B	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!B	$&B	%FB	'8B	,=B	.cB	0�B	3�B	6zB	9�B	:xB	;�B	;�B	=�B	=B	?B	E�B	G�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	K�B	MB	MB	LJB	PbB	R:B	TaB	T{B	W�B	\xB	^jB	_VB	_pB	_�B	`vB	a|B	a|B	a�B	d�B	g�B	h�B	j�B	k�B	l�B	k�B	lB	p�B	r�B	tB	u�B	u�B	u�B	t�B	u%B	x�B	|B	}"B	}VB	~BB	~(B	~�B	�[B	�SB	��B	�lB	�xB	�^B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�=B	�CB	�AB	�[B	�[B	�[B	�GB	��B	�tB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�tB	ȴB	��B	�B	�B	�6B	�B	�4B	�B	�&B	�&B	�,B	�,B	�FB	�MB	�9B	�?B	�EB	�EB	�EB	�KB	�1B	�yB	�eB	�WB	�qB	�CB	�~B	�~B	ݘB	�B	�B	�B	�tB	�B	�B	�sB	�B	�B	�B	�B	�B	��B	��B	� B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�0B	�rB	�LB	�B	��B	�	B	�$B	�B	�B	�(B	�(B	�"B	�HB
'B
GB
GB
MB
MB
MB
gB
SB
MB
MB
9B
SB
SB
YB
tB
�B
EB
	RB

rB
xB
dB
~B
~B
~B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
G�O�G�O�B
+B
$B
�B
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
B
 B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 B
!�B
!�B
!�B
!�B
!�B
!�B
"B
!B
"B
!B
# B
%B
%�B
'B
&�B
'B
(>B
'8B
(>B
(XB
)DB
+6B
,"B
-CB
-CB
-CB
-CB
-]B
-)B
./B
./B
-CB
-CB
.IB
/5B
.cB
/iB
0UB
1AB
1[B
1[B
3MB
3MB
3MB
3hB
33B
3hB
3hB
5ZB
5�B
6`B
6zB
7�B
8�B
9�B
:�B
;B
;B
;dB
:xB
:xB
:�B
;�B
;B
;B
;B
:�B
:�B
;B
:�B
;�B
;B
=�B
>�B
>�B
?�B
?�B
>�B
A�B
B�B
B�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
C�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J	B
J�B
J�B
I�B
J�B
L�B
MB
MB
NB
N�B
N�B
N�B
N�B
OB
NB
N�B
N�B
PB
PB
Q B
O�B
O�B
O�B
PB
PB
QB
Q�B
R B
R B
Q�B
RB
R B
R B
S&B
SB
TB
S&B
TB
T,B
SB
TB
UB
T�B
UB
T,B
U2B
U2B
U2B
UB
TFB
V9B
V9B
W$B
W?B
W?B
W$B
W$B
W?B
X+B
XB
XEB
X+B
X+B
X+B
X+B
XEB
X+B
X+B
XEB
XEB
YKB
ZQB
[=B
[WB
[WB
[WB
\CB
]dB
]dB
]IB
]dB
]dB
^OB
_pB
_pB
_VB
_VB
_VB
`\B
`\B
abB
a|B
bNB
cnB
bhB
bhB
bNB
b�B
b�B
b�B
c�B
dZB
dtB
d�B
dtB
d�B
dtB
d�B
ezB
e�B
ezB
d�B
e�B
e�B
f�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
h�B
g�B
iyB
i�B
h�B
h�B
i�B
i�B
j�B
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
k�B
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
n�B
n�B
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
q�11111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<֡�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710290036082017102900360820171029003608201806221232352018062212323520180622123235201804050428152018040504281520180405042815  JA  ARFMdecpA19c                                                                20171025063509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171024213511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171024213512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171024213513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171024213513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171024213513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171024213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171024213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171024213514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171024213514                      G�O�G�O�G�O�                JA  ARUP                                                                        20171024215544                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171025153222  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171026000000  CF  PSAL_ADJUSTED_QCC  D�  G�O�                JM  ARCAJMQC2.0                                                                 20171028153608  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171028153608  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192815  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033235  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                