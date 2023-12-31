CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-01T21:35:11Z creation;2016-07-01T21:35:13Z conversion to V3.1;2019-12-19T08:36:45Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20160701213511  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_012                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׸3x�� 1   @׸4'�} @<��IQ��dw�i�B�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|��@�33@�33A  A=��A]��A}��A���A���A���A���A���A�  A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/�3C1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}� CٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0��D1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�fD��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��fD�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�K�A�K�A�M�A�O�A�M�A�M�A�K�A�K�A�K�A�O�A�M�A�E�A�E�A�A�A�=qA�-Aġ�A�ffA�ƨA�l�A��A� �A�ZA���A��
A��mA�;dA��A��A�(�A��jA���A�(�A�1A�v�A�=qA�ȴA���A�%A�^5A��#A�t�A��A���A��A���A�hsA�jA�ZA�`BA�-A�5?A�  A��^A�G�A���A��A��A�M�A��+A�C�A���A�(�A��A�&�A���A�=qA���A���A�hsA���A���A�hsA�bNA��;A�-A��\A��A��HA�;dA�~�A��A~��A}��A|bNAz��AyAy33Aw�TAw7LAv�Au�AtȴAs��Ar~�AqhsAp5?An��Al��Ak�hAkS�Ajr�Ai�Ai?}Ah^5Ag��Af�/Af1'Af(�Af-Af �Ae��Ad�Ab9XAa\)AaVA`=qA_p�A]�mA\��AZ�jAX�jAXn�AX=qAWVAU`BAT��AT5?AQp�AP�\AN��AM|�AL��AK\)AJ-AIt�AH1AH{AG�FAF��AFA�AE��AE/ADA�AChsAB$�AA��AAt�A@�A@ffA?�TA?;dA>^5A<�A<�\A<=qA;A;A:JA9|�A8�yA8ffA7�-A7&�A6��A6��A6M�A5"�A3dZA21'A0~�A/7LA.jA-��A-�FA-?}A+�FA*��A)%A(�9A(E�A'A'x�A&��A&��A&n�A&M�A&JA%�^A%%A#�A"n�A"1'A!A!7LA jAoA�
A��A��A�yAt�A/A�A  A?}A=qA�A�/A�jA��A~�AffA(�A�-A�jAhsAQ�A��A�A-A�PA��A�
A
��A
(�A�A��A|�A/A�`AA�A��A��AhsA%A-A ��A @��H@�/@� �@�t�@���@���@�{@�+@���@�O�@�7L@�/@���@�  @�ȴ@��T@���@��@�1'@���@�n�@�/@�1'@���@��#@��@�R@��T@�^@�p�@�7L@���@�D@�F@�E�@��@�A�@�=q@�hs@�z�@���@�|�@�C�@��@�n�@�%@�Z@� �@�b@���@׮@�=q@�7L@ԣ�@ӍP@�;d@�ȴ@��@�%@϶F@��@Ώ\@���@��;@ʗ�@ɑh@��@ȴ9@� �@�+@�E�@�X@�Ĝ@��@���@��@��@���@�dZ@��@�=q@��D@�  @��w@��+@�=q@���@�9X@��@�5?@�@��u@��;@��H@�n�@�n�@�@���@�Q�@�|�@���@�7L@��u@�b@���@��@��/@� �@��;@�t�@��@�{@�x�@��@�+@���@�$�@�%@��@��D@�j@�1@�S�@�+@�+@�"�@��R@���@���@�%@�A�@��@�C�@�^5@��#@�&�@��D@��@�33@���@�M�@��-@��@�%@���@��/@���@�A�@�b@��m@��P@�33@�{@�x�@��`@��9@�Z@��m@���@�\)@�@��!@���@��+@�M�@�@�7L@��`@�Z@�A�@��@��@��
@���@�K�@��y@�v�@�@��-@�`B@�G�@��@��D@� �@�l�@�;d@�"�@��y@���@�v�@�{@���@�X@�%@��D@��F@�l�@�S�@�C�@�"�@��@�~�@���@��-@���@�x�@�X@��@�j@��@�w@K�@�@~�R@~{@}?}@|j@|I�@|(�@|�@{�m@{"�@z��@z~�@z=q@zJ@y7L@xbN@wl�@wK�@w
=@v$�@v@u�@u�@u�T@u@u?}@t��@tz�@t�@t1@s��@t�@s�
@st�@s"�@s@r�H@r�!@rn�@rM�@rM�@q�@q�7@q&�@pQ�@pA�@pbN@p�@q%@p��@p  @oK�@n�y@n��@n��@m�T@mO�@m?}@l�@m�@m@m�@k�F@j��@j��@j�\@j=q@i�^@i�^@i�^@iX@i%@h�`@h�u@hbN@h �@g�;@g\)@g�@fȴ@fv�@fV@fE�@f5?@f$�@e�@e@e��@eO�@d��@dj@dI�@d9X@d9X@d�@c��@c��@ct�@cC�@cC�@cC�@c33@c33@c33@co@b^5@`r�@` �@_��@_;d@_;d@_K�@_K�@_\)@_\)@_l�@_l�@_l�@_l�@_l�@_
=@^�R@^�R@^��@^ff@^5?@]�T@]O�@\��@\�@\z�@\9X@\�@[�m@[�
@[ƨ@[ƨ@[�F@[o@Y��@Y�7@Y7L@X��@XbN@W��@W;d@W+@V�y@V�@V�@V��@VV@U��@U?}@UV@T�/@T�D@T�D@T�D@Tz�@T9X@SdZ@S@R��@R��@R��@R��@R��@R�\@Q�#@Q��@Q�^@Q�^@Qx�@QG�@QG�@QG�@Q7L@P��@P �@O�@O�@O�@O\)@OK�@N�R@N{@M�@MV@L�/@L�/@L�j@Lj@K�m@K��@K"�@J�!@J~�@J~�@Jn�@J^5@J^5@J=q@J�@I�@Ix�@I�@H��@HĜ@H��@G�w@G�P@G�P@G|�@G;d@F�R@Fv�@Fff@FV@F$�@E��@E`B@EO�@D�@D�j@D�@D��@D�D@Dz�@Dj@Dj@DI�@D(�@Cƨ@C�@CC�@B��@Bn�@B-@A�#@A��@A�7@Ax�@A7L@@��@@A�@@ �@?�@?l�@>�R@>V@>E�@=�-@=`B@=?}@=�@=V@<�@<��@<�j@<9X@;�F@;�@;C�@;33@;"�@:�H@:~�@9�#@9��@9x�@9%@8Ĝ@8�u@81'@7�;@7�w@7\)@7
=@6�y@6ȴ@6��@6v�@6V@6$�@6@5�@5�-@5p�@5/@4��@4I�@41@3��@3t�@2��@2n�@2^5@2�@1�@1�#@1�#@1��@1��@1&�@0��@0�u@0A�@/�@/�w@/�P@/|�@/K�@/
=@.�y@.��@.$�@-��@-@-@-�-@-O�@-V@,�@,�/@,�j@,��@,z�@,j@,I�@,�@+��@+�
@+��@+dZ@+33@+@*�@*�H@*��@*��@*n�@*^5@*�@)�@)�#@)��@)��@)hs@(�u@(r�@(A�@'�w@'�@'�@'K�@'
=@&�@&v�@&5?@&{@&@%��@%?}@%V@$��@$�@$��@$��@$�D@$j@#�F@#dZ@#o@"��@"��@"n�@"=q@!��@!�^@!x�@!7L@ �`@ �u@ bN@ Q�@  �@�@�;@�w@\)@�y@�@ȴ@��@�+@v�@{@��@O�@O�@O�@?}@/@�@�@��@�j@��@9X@�@1@��@��@��@dZ@dZ@dZ@dZ@33@@@�@��@�\@n�@^5@^5@��@&�@��@�u@ �@�w@l�@�@�@��@5?@@�T@��@`B@/@��@��@�@Z@1@ƨ@�@33@o@@��@~�@=q@��@�@��@hs@7L@�@��@r�@A�@  @��@�w@��@��@�P@l�@K�@+@�@�@��@��@v�@V@$�@@�@�@�T@��@�@?}@?}@�@V@��@�j@��@Z@�@�m@�m@�m@�
@�F@��@��@��@��@��@t�@33@"�@"�@o@
�@
�H@
�!@
�\@
�\@
n�@
J@	��@	�7@	hs@	7L@	�@�`@�u@r�@bN@A�@  @�@�@��@�w@�@l�@;d@+@�@�@�@�R@��@V@$�@@{@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�S�A�K�A�K�A�M�A�O�A�M�A�M�A�K�A�K�A�K�A�O�A�M�A�E�A�E�A�A�A�=qA�-Aġ�A�ffA�ƨA�l�A��A� �A�ZA���A��
A��mA�;dA��A��A�(�A��jA���A�(�A�1A�v�A�=qA�ȴA���A�%A�^5A��#A�t�A��A���A��A���A�hsA�jA�ZA�`BA�-A�5?A�  A��^A�G�A���A��A��A�M�A��+A�C�A���A�(�A��A�&�A���A�=qA���A���A�hsA���A���A�hsA�bNA��;A�-A��\A��A��HA�;dA�~�A��A~��A}��A|bNAz��AyAy33Aw�TAw7LAv�Au�AtȴAs��Ar~�AqhsAp5?An��Al��Ak�hAkS�Ajr�Ai�Ai?}Ah^5Ag��Af�/Af1'Af(�Af-Af �Ae��Ad�Ab9XAa\)AaVA`=qA_p�A]�mA\��AZ�jAX�jAXn�AX=qAWVAU`BAT��AT5?AQp�AP�\AN��AM|�AL��AK\)AJ-AIt�AH1AH{AG�FAF��AFA�AE��AE/ADA�AChsAB$�AA��AAt�A@�A@ffA?�TA?;dA>^5A<�A<�\A<=qA;A;A:JA9|�A8�yA8ffA7�-A7&�A6��A6��A6M�A5"�A3dZA21'A0~�A/7LA.jA-��A-�FA-?}A+�FA*��A)%A(�9A(E�A'A'x�A&��A&��A&n�A&M�A&JA%�^A%%A#�A"n�A"1'A!A!7LA jAoA�
A��A��A�yAt�A/A�A  A?}A=qA�A�/A�jA��A~�AffA(�A�-A�jAhsAQ�A��A�A-A�PA��A�
A
��A
(�A�A��A|�A/A�`AA�A��A��AhsA%A-A ��A @��H@�/@� �@�t�@���@���@�{@�+@���@�O�@�7L@�/@���@�  @�ȴ@��T@���@��@�1'@���@�n�@�/@�1'@���@��#@��@�R@��T@�^@�p�@�7L@���@�D@�F@�E�@��@�A�@�=q@�hs@�z�@���@�|�@�C�@��@�n�@�%@�Z@� �@�b@���@׮@�=q@�7L@ԣ�@ӍP@�;d@�ȴ@��@�%@϶F@��@Ώ\@���@��;@ʗ�@ɑh@��@ȴ9@� �@�+@�E�@�X@�Ĝ@��@���@��@��@���@�dZ@��@�=q@��D@�  @��w@��+@�=q@���@�9X@��@�5?@�@��u@��;@��H@�n�@�n�@�@���@�Q�@�|�@���@�7L@��u@�b@���@��@��/@� �@��;@�t�@��@�{@�x�@��@�+@���@�$�@�%@��@��D@�j@�1@�S�@�+@�+@�"�@��R@���@���@�%@�A�@��@�C�@�^5@��#@�&�@��D@��@�33@���@�M�@��-@��@�%@���@��/@���@�A�@�b@��m@��P@�33@�{@�x�@��`@��9@�Z@��m@���@�\)@�@��!@���@��+@�M�@�@�7L@��`@�Z@�A�@��@��@��
@���@�K�@��y@�v�@�@��-@�`B@�G�@��@��D@� �@�l�@�;d@�"�@��y@���@�v�@�{@���@�X@�%@��D@��F@�l�@�S�@�C�@�"�@��@�~�@���@��-@���@�x�@�X@��@�j@��@�w@K�@�@~�R@~{@}?}@|j@|I�@|(�@|�@{�m@{"�@z��@z~�@z=q@zJ@y7L@xbN@wl�@wK�@w
=@v$�@v@u�@u�@u�T@u@u?}@t��@tz�@t�@t1@s��@t�@s�
@st�@s"�@s@r�H@r�!@rn�@rM�@rM�@q�@q�7@q&�@pQ�@pA�@pbN@p�@q%@p��@p  @oK�@n�y@n��@n��@m�T@mO�@m?}@l�@m�@m@m�@k�F@j��@j��@j�\@j=q@i�^@i�^@i�^@iX@i%@h�`@h�u@hbN@h �@g�;@g\)@g�@fȴ@fv�@fV@fE�@f5?@f$�@e�@e@e��@eO�@d��@dj@dI�@d9X@d9X@d�@c��@c��@ct�@cC�@cC�@cC�@c33@c33@c33@co@b^5@`r�@` �@_��@_;d@_;d@_K�@_K�@_\)@_\)@_l�@_l�@_l�@_l�@_l�@_
=@^�R@^�R@^��@^ff@^5?@]�T@]O�@\��@\�@\z�@\9X@\�@[�m@[�
@[ƨ@[ƨ@[�F@[o@Y��@Y�7@Y7L@X��@XbN@W��@W;d@W+@V�y@V�@V�@V��@VV@U��@U?}@UV@T�/@T�D@T�D@T�D@Tz�@T9X@SdZ@S@R��@R��@R��@R��@R��@R�\@Q�#@Q��@Q�^@Q�^@Qx�@QG�@QG�@QG�@Q7L@P��@P �@O�@O�@O�@O\)@OK�@N�R@N{@M�@MV@L�/@L�/@L�j@Lj@K�m@K��@K"�@J�!@J~�@J~�@Jn�@J^5@J^5@J=q@J�@I�@Ix�@I�@H��@HĜ@H��@G�w@G�P@G�P@G|�@G;d@F�R@Fv�@Fff@FV@F$�@E��@E`B@EO�@D�@D�j@D�@D��@D�D@Dz�@Dj@Dj@DI�@D(�@Cƨ@C�@CC�@B��@Bn�@B-@A�#@A��@A�7@Ax�@A7L@@��@@A�@@ �@?�@?l�@>�R@>V@>E�@=�-@=`B@=?}@=�@=V@<�@<��@<�j@<9X@;�F@;�@;C�@;33@;"�@:�H@:~�@9�#@9��@9x�@9%@8Ĝ@8�u@81'@7�;@7�w@7\)@7
=@6�y@6ȴ@6��@6v�@6V@6$�@6@5�@5�-@5p�@5/@4��@4I�@41@3��@3t�@2��@2n�@2^5@2�@1�@1�#@1�#@1��@1��@1&�@0��@0�u@0A�@/�@/�w@/�P@/|�@/K�@/
=@.�y@.��@.$�@-��@-@-@-�-@-O�@-V@,�@,�/@,�j@,��@,z�@,j@,I�@,�@+��@+�
@+��@+dZ@+33@+@*�@*�H@*��@*��@*n�@*^5@*�@)�@)�#@)��@)��@)hs@(�u@(r�@(A�@'�w@'�@'�@'K�@'
=@&�@&v�@&5?@&{@&@%��@%?}@%V@$��@$�@$��@$��@$�D@$j@#�F@#dZ@#o@"��@"��@"n�@"=q@!��@!�^@!x�@!7L@ �`@ �u@ bN@ Q�@  �@�@�;@�w@\)@�y@�@ȴ@��@�+@v�@{@��@O�@O�@O�@?}@/@�@�@��@�j@��@9X@�@1@��@��@��@dZ@dZ@dZ@dZ@33@@@�@��@�\@n�@^5@^5@��@&�@��@�u@ �@�w@l�@�@�@��@5?@@�T@��@`B@/@��@��@�@Z@1@ƨ@�@33@o@@��@~�@=q@��@�@��@hs@7L@�@��@r�@A�@  @��@�w@��@��@�P@l�@K�@+@�@�@��@��@v�@V@$�@@�@�@�T@��@�@?}@?}@�@V@��@�j@��@Z@�@�m@�m@�m@�
@�F@��@��@��@��@��@t�@33@"�@"�@o@
�@
�H@
�!@
�\@
�\@
n�@
J@	��@	�7@	hs@	7L@	�@�`@�u@r�@bN@A�@  @�@�@��@�w@�@l�@;d@+@�@�@�@�R@��@V@$�@@{@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B��BĜBƨB�qB��Bt�BB�B+B�B�B{BVBB�B�B�wB�RB��B��B�JB|�B�+B}�By�BaHBVBO�BD�B5?B'�B$�B�BDB��B�B�
BB�qB�LB�B�B��B��B�DB�%B�Bu�BhsBcTB[#BW
BL�B@�B6FB+B�BB
�B
�mB
�)B
��B
ŢB
�?B
�B
��B
��B
�hB
�7B
�B
x�B
p�B
r�B
hsB
aHB
_;B
T�B
M�B
F�B
9XB
/B
$�B
�B
hB
+B
+B
B
B	��B	��B	��B	�B	�B	�B	�B	�B	�yB	�TB	��B	��B	��B	ƨB	��B	�RB	�B	��B	�{B	�hB	�VB	�7B	|�B	{�B	v�B	\)B	P�B	G�B	C�B	;dB	49B	.B	,B	&�B	,B	.B	&�B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	{B	�B	{B	\B	JB	
=B	+B	B	B��B��B��B��B�B�B�B�B�sB�#B��BȴB��B�wB�dB�^B�RB�-B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�JB�1B�B�B� Bv�Bt�Bt�Br�Bo�Bo�Bk�BjBiyBhsBhsBgmBffBffBe`BcTB^5B]/B[#BXBVBR�BO�BJ�BG�BB�B;dB7LB5?B49B49B33B2-B1'B0!B0!B-B+B+B)�B(�B'�B&�B&�B%�B$�B!�B!�B!�B!�B"�B"�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B!�B"�B"�B!�B �B!�B�B�B!�B"�B#�B#�B(�B,B-B-B-B/B/B/B0!B1'B0!B0!B/B/B1'B0!B/B/B/B0!B0!B1'B2-B5?B6FB9XB:^B:^B<jBA�BB�BD�BE�BG�BJ�BK�BM�BO�BYBZBZB[#B]/B`BBaHBcTBffBhsBiyBp�Br�Bs�Bt�Bv�Bz�Bz�Bz�Bz�B|�B� B�B�B�B�B�7B�VB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�FB�XB�jB�jB�wB��B��BÖBŢBǮBȴBȴB��B��B�
B�B�)B�)B�/B�/B�5B�5B�HB�ZB�fB�yB�B�B�B�B�B��B��B��B��B��B	  B	B	B	%B	1B	
=B	PB	uB	{B	{B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	%�B	,B	.B	0!B	2-B	2-B	49B	6FB	8RB	<jB	=qB	>wB	?}B	@�B	B�B	C�B	D�B	E�B	G�B	I�B	M�B	VB	YB	ZB	]/B	]/B	^5B	]/B	^5B	^5B	aHB	cTB	e`B	ffB	ffB	ffB	gmB	iyB	k�B	l�B	m�B	n�B	o�B	r�B	s�B	s�B	u�B	v�B	y�B	� B	�B	�B	�B	�7B	�DB	�JB	�\B	�bB	�bB	�bB	�bB	�bB	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�RB	�XB	�XB	�XB	�XB	�^B	�}B	��B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�BB	�TB	�TB	�`B	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
JB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
bB
hB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
%�B
&�B
'�B
&�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
.B
.B
.B
/B
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
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
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
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
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
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
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
P�B
P�B
P�B
P�B
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
S�B
S�B
S�B
S�B
T�B
T�B
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
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
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
cTB
cTB
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
dZB
e`B
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
ffB
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
n�B
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
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�;B� B� B�B� B�B� B� B�B�B�;B�'B�-B�GB�GB��B�B�qB��B�xB�B�yB.BF�B.�B �B!B�B�B_B�2B�pB��B��B�_B�B�B~(B��B� B|�Bb�BXEBRoBGB7B)*B'RBVBB[B��B�eB�{B��B�8B�UB��B�!B�+B�B�EB��BwLBi�Bd�B\CBX�BNpBB'B8RB-�BWBMB
�?B
�DB
��B
ҽB
��B
�B
��B
�hB
�B
��B
��B
��B
zB
q�B
s�B
iDB
a�B
`�B
VB
OBB
HfB
:�B
0�B
&�B
�B
�B
�B
KB
�B
B	�B	��B	��B	�GB	��B	��B	��B	�]B	�B	��B	�B	̘B	��B	��B	B	�*B	�oB	��B	�2B	�:B	�B	�B	}�B	}qB	yrB	]�B	R�B	I�B	D�B	=B	5�B	/OB	-]B	'RB	,�B	/5B	'�B	$�B	!�B	B	�B	)B	]B	B	�B	_B	SB	�B	�B	B	�B	�B	B	fB	SB	�B��B��B��B��B�GB�'B�B�OB�B�B�B�XB��B�cB��B�dB�*B��B��B��B��B��B��B��B�zB�FB�@B�ZB��B��B�HB�IB�B�2B��B��B� B��B��B�9B�3B��BwfButBu�BtBq'Bp�Bk�Bj�Bi�Bh�Bh�Bh$BgmBh
Bg8Bd�B_B^�B\�BY1BWsBT,BQ4BL0BI�BESB<�B7�B5�B5%B4�B3�B2�B2-B1�B2B./B,"B,"B*�B)�B(sB'RB'�B'�B%�B"4B"B!�B"hB#�B#�B"�B"�B!HB!HB!bB �B �B�B�B�B�B�BOBBBBBIB�B�B �B �B!B �BpBBBBB~B�BIBBBB5B�B�BpB�B"4B"hB"�B#�B#�B"�B!|B"�B �B �B"�B#nB$@B$�B)�B,�B-�B-�B.cB/�B/�B/�B0�B1�B0�B0�B0!B/�B1�B1B/�B/�B0UB1'B0�B1�B3B5�B7B9�B:�B:�B=VBBABC{BE�BF%BHKBKDBLdBN�BQNBY�BZ�BZ�B[�B]�B`�BbhBd&Bf�Bi*BjKBp�Br�Bs�Bu?BwfB{B{B{0B{JB}qB��B��B��B��B��B��B��B��B�B��B�+B�)B�OB�bB�ZB�B�B�8B�XB�DB�6B�WB��B��B�B��B��B��B��B��B��B��B��B��B��B�B�B�PB�uB�sBٚB�]B�xB�~B�~B�jBޞB�B��B��B��B��B��B�B�B�9B�LB�B�<B�BB�.B	 OB	�B	�B	�B	�B	
�B	B	�B	�B	�B	�B	�B	B	#B	�B	 �B	"B	#B	$@B	&�B	,WB	.IB	0UB	2aB	2aB	4�B	6�B	8�B	<�B	=�B	>�B	?�B	@�B	B�B	C�B	D�B	E�B	G�B	J=B	N<B	V9B	YeB	Z�B	]dB	]dB	^OB	]dB	^OB	^�B	a�B	c�B	e�B	f�B	f�B	f�B	g�B	i�B	k�B	l�B	m�B	n�B	o�B	r�B	s�B	s�B	vB	wB	z*B	�B	�B	�'B	�B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	��B	��B	�$B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�,B	�
B	�0B	�6B	�CB	�B	�IB	�OB	�OB	�;B	�GB	��B	��B	�tB	�lB	�XB	�rB	��B	�rB	��B	��B	��B	ðB	ðB	��B	żB	��B	��B	�+B	�fB	�B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�FB	�2B	�B	�?B	�1B	�QB	�qB	�~B	�\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	�B	�B	�PB	�(B	�.B	�B	��B	�B	�B	�.B
 4B
'B
'B
'B
AB
AB
B
-B
GB
aB
gB
SB
SB
9B
YB
YB
zB
fB
�B
fB
	lB

XB

XB
�B
�B
�B
�B
�B
�B
pB
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
# B
$B
%B
%,B
&B
'B
($B
'8B
)*B
)*B
)B
)*B
)*B
*0B
*0B
*KB
+QB
+6B
,"B
,=B
,=B
,=B
,WB
-]B
.IB
.IB
.cB
/5B
0UB
0oB
1[B
1[B
1[B
2GB
2aB
3hB
3hB
3hB
3hB
3MB
3MB
4nB
4nB
4TB
4nB
5tB
5�B
6zB
6zB
6zB
7�B
8�B
8�B
8lB
9�B
9rB
9XB
9�B
9rB
9�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
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
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
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
H�B
H�B
H�B
H�B
IB
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K�B
K�B
K�B
MB
MB
MB
M�B
NB
NB
N�B
OB
OB
PB
PB
O�B
O�B
O�B
PB
P.B
QB
Q B
Q B
QB
R B
RB
R:B
R:B
S&B
R�B
SB
SB
R�B
S&B
SB
TB
T,B
T,B
TB
T�B
UB
UB
U2B
U2B
U2B
VB
VB
VB
V9B
V9B
VB
V9B
VB
V9B
W$B
W?B
W?B
WsB
XEB
X+B
YeB
YKB
ZQB
ZQB
[=B
[WB
[WB
\CB
\CB
\CB
]IB
]dB
]dB
^jB
^jB
^jB
^jB
^OB
_pB
_VB
_VB
`\B
`vB
`\B
`vB
a|B
abB
a|B
a|B
a|B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
dtB
dtB
dtB
dZB
d�B
dtB
d�B
d�B
d�B
ezB
e`B
e�B
e�B
ezB
e�B
f�B
ffB
ffB
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
hsB
hsB
hsB
h�B
i�B
i�B
iyB
i�B
iyB
i�B
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
k�B
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*f�<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607060036522016070600365220160706003652201806221210242018062212102420180622121024201804050402412018040504024120180405040241  JA  ARFMdecpA19c                                                                20160702063504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160701213511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160701213511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160701213512  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160701213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160701213512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160701213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160701213513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160701213513  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160701213513                      G�O�G�O�G�O�                JA  ARUP                                                                        20160701222516                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160702153600  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160705153652  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160705153652  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190241  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                