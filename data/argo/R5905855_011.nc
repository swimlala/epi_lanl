CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:12:34Z creation;2022-06-04T19:12:34Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191234  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���U�l1   @��=�/h@/���l�D�d����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@���A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B���B�  B�  B�  B���B�ffB�  B�  B�  B���B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�  @�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��B�� B�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fBǀ B˳3Bπ Bӳ3B׳3B۳3B߀ B��B�3B�3B�3B� B��3B��3B��3CٚCٚCٚCٚC	�4C�4CٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!� C#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;�4C=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCS� CUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD��DvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{��D|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�x D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��Aկ�A�|A�
�A���AԹ$A԰�AԨXAԣ�AԞOAԙ1AԚ7AԘ�AԘ�AԠ�A��TA�'A���A���Aԯ�Aԃ�A�LdA��AӚ7A�6�AҪ�A�K�A�rA��oA���A�iDA�o5A���A�k�A���A̲�A�]�Aˆ�A�.�A���A�iDAɼ�A�#nA�4�AǜA���A�o�A��A�JXA�LdA�dZA���A��A��A���A�G�A�\A���A�CaA��JA�	lA�8�A��$A��A��$A�XA��A�d�A�
=A��
A���A�%�A��A���A��?A���A���A�	�A���A�w�A��rA�ѷA��cA��A���A�q�A�5�A�VA���A�!-A��A�:�A��oA��!A}9�Aw��As
=Aq��Ao�UAmv�Akd�AgU�AbߤA_q�AZE9AVk�AT�`ARMjAOg�AMXyAH��AE��AD�AB�)AA�A?�A?�A>��A<�UA;zA:�A: iA8�aA6�)A5 �A3��A2�5A2\�A1�4A1-�A0��A/�{A-p�A*��A)�A)��A(�/A(4�A'��A&�}A'TaA(DgA'|�A&�QA%5�A#�A ��Ap�A_pAN�A�.A�A&A�ACAȴA�hA��AqvA�hAu%A�UACA:�A�bA7LA�5AS�A=A6�A{�A��A��A"�A�\A
}VA	�VA	:*A�A�4AOA�	Ak�A��A�vA��A&�Al"A��A��A_pA3�A��A�	A0�A!A�[A �@���@���@�=�@�4@��&@�N<@���@���@�%@�&�@��@�S@�]�@�[W@�j�@��@�A @�_@�Z�@��p@�"@@��@�!@��@�|�@��s@�a@컙@���@�!-@�4@���@�A @�g�@�S&@��v@�	@�H�@�oi@��)@�|�@���@���@䭬@���@�.I@�@@���@�@ⅈ@�6�@�/�@�a@�z�@�,=@��@��@���@��+@�@�@�33@�&�@��@ہ�@���@�j�@�\�@�C�@�J�@�c @ۍP@ڰ!@�6�@�u�@�d�@�6�@�~�@ּj@�J�@�@�ݘ@՟V@�g�@���@�U2@��Q@�~�@�ߤ@�Q�@��z@ѕ�@��H@�>B@ϙ�@��@ζ�@�Ĝ@���@έ�@΄�@�q�@�8�@���@�rG@�{J@�RT@�@��8@̏\@ˠ�@ʕ@�?}@�`�@��Q@ǝ�@���@�q�@�@�N<@ė�@�{�@�l�@�6@Ó�@�%@´9@��Z@��@�K�@��I@�.�@��T@��@���@�?}@���@��#@��@@���@�(�@��e@�J�@� �@��]@���@��F@�s�@�&�@���@���@��T@���@�P�@���@��O@�Q@���@�a�@���@�9X@��@�f�@��@��@��j@���@�kQ@��@�4�@��@�҉@��9@�.�@��F@�O@�ں@�H�@�ϫ@�qv@�%F@��@���@��@�hs@�C@�S@���@���@��'@���@�/�@�)�@��.@���@�hs@�@@���@���@��7@�o@��@��@��|@��M@���@��\@�;�@��@���@�j@��@�%@���@�h
@��@��$@�Y�@�/�@��@�ȴ@���@�h�@���@���@�K�@� \@��@���@�p;@�M@�x@��@�k�@���@�W�@�~@��w@�'�@��H@���@�|�@��@��^@��f@�W?@��y@�}V@�#:@���@�J�@��@�ی@��j@��@��o@�&�@�e@�4@���@���@�m]@�=@��@��v@�e�@��@��@��p@�B[@��@��o@��9@���@��n@�|�@�N<@�L�@�"�@��6@�&�@��d@��@�T�@�8@��@��@�ff@�S�@�:�@�@���@��@���@�dZ@��@�_�@�;�@���@��#@���@�qv@�'�@���@��@�l�@�/�@��)@���@�k�@�Dg@�+@���@��@�xl@�S�@�*�@�
�@��A@��S@�@��B@���@�tT@�9X@��@��6@���@�j�@�*0@��@�@��@��@��\@�M�@�#:@��
@��@�b�@��M@��m@��O@���@��Y@�y>@�r�@�h
@�R�@�=q@��@��@���@��F@���@�j�@��@��@��p@��h@�xl@�;�@�@���@��#@��d@��M@�2a@�	l@��]@���@�ff@��@�a@X�@~�x@}��@|�@|tT@{�m@{�k@{iD@z��@y��@y��@yA @x�5@x�@x��@x2�@w�A@wy�@w
=@v@�@uV@t�|@t�5@t�5@t�e@s��@s\)@s!-@s�@r��@r�r@r��@r��@q��@qY�@q^�@q(�@pی@o�&@oH�@o,�@o�@o@n�R@n�@m��@mG�@l�@l��@lN�@l �@k�;@k�@@kqv@k@j}V@j�@i��@i?}@h]d@h1@g��@gO@f�y@f�@f� @fJ�@e�@eF@d�P@d[�@c�a@cX�@c33@c�@b�!@bv�@bQ@b�@a#�@`�@`�e@`~@_�@_��@_�4@_e�@_=@_&@_�@^�R@^_�@^-@]�j@]�@]��@]m]@\��@\l"@\4n@\M@[�+@[˒@[dZ@[�@Z�@Z�@Zn�@Ze@Y�Z@Y�@Y�'@Y@X�u@X�@Ws@V��@V3�@U��@Uk�@U	l@T�@T��@Tb@S~�@S6z@SC@R�]@R��@R@�@Q��@Q&�@P�U@P�@P~(@PC-@O�@NV@N!�@M��@M�@Lz�@K�+@K�@K��@J��@J#:@Ij@I(�@H�e@H�@G˒@G��@G�f@G=@GC@F�s@F��@F��@FH�@Ec@D�9@D�D@D�@D~@C��@C��@Cj�@C"�@B��@Bs�@B	@A�d@ArG@A!�@@��@@_@@C-@@G@?�	@>�h@>ff@>_@=�t@=5�@<֡@<��@<$@;qv@:��@:��@:Ta@:J@9�@9��@98�@8�K@8�z@8��@8z�@8%�@7��@7Z�@7/�@6�2@6d�@6�@5��@5�-@5Dg@5@4�@4	�@3��@3��@3�V@3n/@3H�@3S@2�B@2��@2@�@1�^@1e,@1(�@0�@0��@0l"@0>B@/�+@/��@/��@/dZ@/Mj@/E9@/>�@.�]@.� @.W�@-ԕ@-`B@-*0@-V@-@-�@,�P@,��@,w�@,A�@,'R@+��@+�	@*��@*M�@*4@)�@)s�@)%F@(�`@(z�@(<�@(!@'��@'v`@'4�@'@&��@&kQ@&Ov@%��@%��@%�@%�M@%�@%hs@%0�@$�j@$��@$~@#�a@#�f@#@"��@"8�@"$�@!��@!�@!k�@!!�@ ��@ ֡@ ��@ Z@ b@��@\)@/�@�@�'@�@�@Ov@@�#@�S@k�@(�@��@��@�u@tT@M@A�@1'@%�@	�@�]@�&@��@�*@�k@a@�@��@�}@�r@}V@kQ@GE@3�@-@�@�3@|@!�@�f@��@�@��@�?@�@~(@N�@G@� @��@Mj@;d@�@�x@��@8�@��@��@��@ �@�>@��@��@Dg@ \@�|@�@�.@7�@��@��@��@iD@(@�@��@{�@i�@C�@�t@rG@<6@�@�K@Ɇ@~(@bN@?�@b@��@�4@x@o�@U�@&@�"@�2@�h@s�@?@J@�@zx@Vm@Dg@+@��@�j@w�@[�@C-@ �@�@�@ݘ@�0@�@t�@C@
��@
�m@
�\@
~�@
p;@
_�@
8�@
�@	�j@	��@	N<@	q@�[@�@��@oi@V�@$@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��Aկ�A�|A�
�A���AԹ$A԰�AԨXAԣ�AԞOAԙ1AԚ7AԘ�AԘ�AԠ�A��TA�'A���A���Aԯ�Aԃ�A�LdA��AӚ7A�6�AҪ�A�K�A�rA��oA���A�iDA�o5A���A�k�A���A̲�A�]�Aˆ�A�.�A���A�iDAɼ�A�#nA�4�AǜA���A�o�A��A�JXA�LdA�dZA���A��A��A���A�G�A�\A���A�CaA��JA�	lA�8�A��$A��A��$A�XA��A�d�A�
=A��
A���A�%�A��A���A��?A���A���A�	�A���A�w�A��rA�ѷA��cA��A���A�q�A�5�A�VA���A�!-A��A�:�A��oA��!A}9�Aw��As
=Aq��Ao�UAmv�Akd�AgU�AbߤA_q�AZE9AVk�AT�`ARMjAOg�AMXyAH��AE��AD�AB�)AA�A?�A?�A>��A<�UA;zA:�A: iA8�aA6�)A5 �A3��A2�5A2\�A1�4A1-�A0��A/�{A-p�A*��A)�A)��A(�/A(4�A'��A&�}A'TaA(DgA'|�A&�QA%5�A#�A ��Ap�A_pAN�A�.A�A&A�ACAȴA�hA��AqvA�hAu%A�UACA:�A�bA7LA�5AS�A=A6�A{�A��A��A"�A�\A
}VA	�VA	:*A�A�4AOA�	Ak�A��A�vA��A&�Al"A��A��A_pA3�A��A�	A0�A!A�[A �@���@���@�=�@�4@��&@�N<@���@���@�%@�&�@��@�S@�]�@�[W@�j�@��@�A @�_@�Z�@��p@�"@@��@�!@��@�|�@��s@�a@컙@���@�!-@�4@���@�A @�g�@�S&@��v@�	@�H�@�oi@��)@�|�@���@���@䭬@���@�.I@�@@���@�@ⅈ@�6�@�/�@�a@�z�@�,=@��@��@���@��+@�@�@�33@�&�@��@ہ�@���@�j�@�\�@�C�@�J�@�c @ۍP@ڰ!@�6�@�u�@�d�@�6�@�~�@ּj@�J�@�@�ݘ@՟V@�g�@���@�U2@��Q@�~�@�ߤ@�Q�@��z@ѕ�@��H@�>B@ϙ�@��@ζ�@�Ĝ@���@έ�@΄�@�q�@�8�@���@�rG@�{J@�RT@�@��8@̏\@ˠ�@ʕ@�?}@�`�@��Q@ǝ�@���@�q�@�@�N<@ė�@�{�@�l�@�6@Ó�@�%@´9@��Z@��@�K�@��I@�.�@��T@��@���@�?}@���@��#@��@@���@�(�@��e@�J�@� �@��]@���@��F@�s�@�&�@���@���@��T@���@�P�@���@��O@�Q@���@�a�@���@�9X@��@�f�@��@��@��j@���@�kQ@��@�4�@��@�҉@��9@�.�@��F@�O@�ں@�H�@�ϫ@�qv@�%F@��@���@��@�hs@�C@�S@���@���@��'@���@�/�@�)�@��.@���@�hs@�@@���@���@��7@�o@��@��@��|@��M@���@��\@�;�@��@���@�j@��@�%@���@�h
@��@��$@�Y�@�/�@��@�ȴ@���@�h�@���@���@�K�@� \@��@���@�p;@�M@�x@��@�k�@���@�W�@�~@��w@�'�@��H@���@�|�@��@��^@��f@�W?@��y@�}V@�#:@���@�J�@��@�ی@��j@��@��o@�&�@�e@�4@���@���@�m]@�=@��@��v@�e�@��@��@��p@�B[@��@��o@��9@���@��n@�|�@�N<@�L�@�"�@��6@�&�@��d@��@�T�@�8@��@��@�ff@�S�@�:�@�@���@��@���@�dZ@��@�_�@�;�@���@��#@���@�qv@�'�@���@��@�l�@�/�@��)@���@�k�@�Dg@�+@���@��@�xl@�S�@�*�@�
�@��A@��S@�@��B@���@�tT@�9X@��@��6@���@�j�@�*0@��@�@��@��@��\@�M�@�#:@��
@��@�b�@��M@��m@��O@���@��Y@�y>@�r�@�h
@�R�@�=q@��@��@���@��F@���@�j�@��@��@��p@��h@�xl@�;�@�@���@��#@��d@��M@�2a@�	l@��]@���@�ff@��@�a@X�@~�x@}��@|�@|tT@{�m@{�k@{iD@z��@y��@y��@yA @x�5@x�@x��@x2�@w�A@wy�@w
=@v@�@uV@t�|@t�5@t�5@t�e@s��@s\)@s!-@s�@r��@r�r@r��@r��@q��@qY�@q^�@q(�@pی@o�&@oH�@o,�@o�@o@n�R@n�@m��@mG�@l�@l��@lN�@l �@k�;@k�@@kqv@k@j}V@j�@i��@i?}@h]d@h1@g��@gO@f�y@f�@f� @fJ�@e�@eF@d�P@d[�@c�a@cX�@c33@c�@b�!@bv�@bQ@b�@a#�@`�@`�e@`~@_�@_��@_�4@_e�@_=@_&@_�@^�R@^_�@^-@]�j@]�@]��@]m]@\��@\l"@\4n@\M@[�+@[˒@[dZ@[�@Z�@Z�@Zn�@Ze@Y�Z@Y�@Y�'@Y@X�u@X�@Ws@V��@V3�@U��@Uk�@U	l@T�@T��@Tb@S~�@S6z@SC@R�]@R��@R@�@Q��@Q&�@P�U@P�@P~(@PC-@O�@NV@N!�@M��@M�@Lz�@K�+@K�@K��@J��@J#:@Ij@I(�@H�e@H�@G˒@G��@G�f@G=@GC@F�s@F��@F��@FH�@Ec@D�9@D�D@D�@D~@C��@C��@Cj�@C"�@B��@Bs�@B	@A�d@ArG@A!�@@��@@_@@C-@@G@?�	@>�h@>ff@>_@=�t@=5�@<֡@<��@<$@;qv@:��@:��@:Ta@:J@9�@9��@98�@8�K@8�z@8��@8z�@8%�@7��@7Z�@7/�@6�2@6d�@6�@5��@5�-@5Dg@5@4�@4	�@3��@3��@3�V@3n/@3H�@3S@2�B@2��@2@�@1�^@1e,@1(�@0�@0��@0l"@0>B@/�+@/��@/��@/dZ@/Mj@/E9@/>�@.�]@.� @.W�@-ԕ@-`B@-*0@-V@-@-�@,�P@,��@,w�@,A�@,'R@+��@+�	@*��@*M�@*4@)�@)s�@)%F@(�`@(z�@(<�@(!@'��@'v`@'4�@'@&��@&kQ@&Ov@%��@%��@%�@%�M@%�@%hs@%0�@$�j@$��@$~@#�a@#�f@#@"��@"8�@"$�@!��@!�@!k�@!!�@ ��@ ֡@ ��@ Z@ b@��@\)@/�@�@�'@�@�@Ov@@�#@�S@k�@(�@��@��@�u@tT@M@A�@1'@%�@	�@�]@�&@��@�*@�k@a@�@��@�}@�r@}V@kQ@GE@3�@-@�@�3@|@!�@�f@��@�@��@�?@�@~(@N�@G@� @��@Mj@;d@�@�x@��@8�@��@��@��@ �@�>@��@��@Dg@ \@�|@�@�.@7�@��@��@��@iD@(@�@��@{�@i�@C�@�t@rG@<6@�@�K@Ɇ@~(@bN@?�@b@��@�4@x@o�@U�@&@�"@�2@�h@s�@?@J@�@zx@Vm@Dg@+@��@�j@w�@[�@C-@ �@�@�@ݘ@�0@�@t�@C@
��@
�m@
�\@
~�@
p;@
_�@
8�@
�@	�j@	��@	N<@	q@�[@�@��@oi@V�@$@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	$�B	#nB	#TB	"�B	 �B	pB	!B	�B	�B	 �B	 �B	!bB	"B	#B	%B	*eB	MB	�oB	~�B	}�B	zxB	v�B	r�B	jB	ezB	_B	Z�B	Z�B	U2B	oOB	��B	��B	�:B	��B	� B	��B	��B	��B	�2B	�1B	��B	��B	��B	��B	��B
�B
)�B
~�B
�jB
ˬB
�jB+B4BW�B}B��B�?B�B�B�-B��B�hB�$B%B�BdB�vB�=B�@B��B�$B�;B�
B��B��B}"BzxBx8B`�BU�BGzB<�B2�B-�B)�BYB
�B
ޞB
ðB
��B
�_B
o�B
UB
G�B
7B
	B	�VB	�>B	��B	�?B	�vB	�B	kB	K�B	7�B	~B	
	B	�B��B�B��B�7B�B��B��B�hB�B��B�=B�B��B��B�HB�'B�qB��B�xB�7B�B�B��B��B��B��B��B��B��B�B��B��B��B�+B	 B	2B	2B	B	�B��B�'B��B��B�XB�>B��B�B�B��B�$B�B��B�6B�}B	xB	YB	B	�B	�B	�B	9B	�B	TB	�B	$B	�B	B	_B	�B	7B	�B	�B	B	�B	&B	&fB	'B	%`B	$B	$&B	%FB	%zB	$�B	&LB	+QB	/B	7�B	B[B	J�B	J�B	I�B	G�B	E�B	F�B	IlB	L�B	V�B	[�B	_VB	_B	\�B	[�B	YB	d&B	h�B	p;B	p�B	m�B	lB	q�B	n/B	j�B	m�B	w�B	{JB	��B	��B	�xB	�KB	��B	�BB	��B	��B	��B	��B	�QB	��B	��B	��B	��B	�#B	��B	��B	�B	�B	��B	��B	��B	��B	�_B	�yB	�]B	��B	�	B	�B	�^B	�xB	��B	�^B	��B	��B	ЗB	̈́B	�B	�%B	��B	�(B	��B	��B	�B	�&B	�B	өB	� B	��B	��B	҉B	�oB	�:B	��B	�:B	҉B	��B	�,B	��B	��B	֡B	�9B	��B	��B	��B	��B	�
B	��B	��B	��B	ּB	�+B	�B	ںB	��B	ݲB	�;B	�\B	�B	��B	�B	�&B	�tB	�B	�B	�&B	�B	��B	�B	�4B	�4B	�B	�4B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�@B	�tB	��B	�,B	�B	�B	�FB	��B	�B	�B	�RB	�RB	�B	��B	�
B	�$B	��B	�_B	�B	�0B	��B	��B	��B	�6B	�6B	�B	�"B	�WB	��B	�)B	�]B	��B	�B	�B	��B	��B	�5B	�B	�5B	��B	��B	��B	��B	�GB	��B	�-B	�aB	�|B	�B	�B	�TB	�B	�nB	�nB	��B	�+B	�zB	�fB	�fB	�RB	��B	��B	��B	�^B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	�"B	�"B	�B	��B	�]B	�.B	�}B	��B
 OB
�B
oB
UB
�B
�B
GB
B
9B
�B
�B
KB
	7B
	7B
	lB
	7B
	�B
	�B
	7B
KB
B
fB
KB
�B
�B
B
�B
�B
_B
zB
+B
�B
zB
�B
�B
	lB
	�B

	B

	B

	B

=B

�B

�B

�B
B
xB
�B
0B
B
�B
~B
�B
PB
(B
�B
B
}B
}B
�B
�B
NB
 B
:B
B
�B
{B
MB
B
B
�B
YB
+B
�B
�B
eB
B
�B
�B
	B
	B
�B
�B
)B
]B
xB
B
IB
�B
OB
�B
�B
B
�B
�B
�B
�B
�B
B
!B
�B
�B
�B
jB
�B
B
 'B
 'B
�B
 BB
 �B
!B
!bB
!|B
!�B
"NB
"NB
"NB
"hB
"hB
"�B
#TB
#nB
#�B
#�B
$&B
%FB
&LB
&�B
&�B
'B
'B
'B
'B
'RB
'RB
'�B
'�B
'�B
(
B
($B
(XB
(�B
)*B
)*B
)DB
)�B
*B
*B
*�B
*�B
*�B
+�B
+�B
,B
,B
,�B
-]B
-]B
-�B
-�B
-�B
-�B
.B
.cB
/ B
.�B
.�B
/�B
0!B
0B
0;B
0�B
1vB
2B
1�B
2B
2aB
2|B
3B
4B
4nB
4nB
4�B
4TB
5%B
5?B
5?B
5%B
5ZB
5�B
7�B
7�B
7fB
7�B
9�B
9�B
9�B
9�B
:�B
;dB
;dB
;dB
;�B
<PB
<�B
<�B
<jB
<�B
="B
=<B
=qB
=qB
=�B
=�B
=�B
=�B
=qB
="B
<�B
<�B
<�B
=<B
=�B
=�B
=�B
>B
>B
>B
=�B
=�B
=�B
>B
=�B
>BB
>]B
>wB
>wB
>wB
>�B
?B
?B
?HB
?cB
?}B
?�B
?�B
?�B
?�B
@ B
@OB
@OB
AB
A;B
A;B
A;B
AB
@�B
@�B
@�B
@�B
@�B
AB
A�B
A�B
A�B
B'B
BuB
CGB
C-B
C{B
C�B
C�B
DgB
D�B
D�B
ESB
E�B
E�B
E�B
FYB
F�B
G+B
G�B
HB
H1B
HKB
H�B
H�B
IB
I�B
I�B
J#B
J	B
J	B
J	B
J�B
KDB
KxB
K�B
K�B
L�B
MPB
M6B
MB
MPB
M6B
MPB
MjB
M�B
N<B
N�B
N�B
N�B
N�B
OB
OBB
O(B
OBB
O\B
PB
P�B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
R:B
RTB
R�B
R�B
S�B
S�B
TB
T{B
T{B
T�B
U2B
VB
U�B
VB
VSB
V�B
W
B
W$B
WsB
X+B
X+B
XyB
XyB
X�B
X�B
YB
YeB
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
\)B
\]B
\]B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^5B
^OB
^OB
^�B
_!B
_!B
_�B
_�B
_�B
_�B
`B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
a-B
a-B
abB
a�B
b4B
bhB
bhB
bhB
bhB
bhB
b�B
b�B
b�B
cB
cB
cTB
c�B
dB
d@B
dZB
d�B
d�B
d�B
e`B
e`B
e`B
e�B
e�B
f2B
f2B
f�B
f�B
f�B
gB
g8B
gRB
gRB
gRB
gRB
gmB
g�B
g�B
h$B
hsB
hsB
h�B
iDB
i�B
i�B
i�B
i�B
j0B
jeB
jB
j�B
j�B
kB
kQB
k�B
lB
l"B
lqB
l�B
lqB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
nB
n/B
nIB
n}B
n}B
n}B
n}B
n�B
n�B
n�B
n�B
o B
n�B
oB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pB
p!B
pB
poB
p�B
p�B
p�B
qAB
q'B
q'B
qB
qB
qvB
q�B
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
s3B
shB
shB
shB
sMB
shB
s�B
s�B
s�B
s�B
tB
t9B
tTB
t�B
t�B
uB
uB
u?B
u�B
u�B
vB
u�B
u�B
vB
v�B
v�B
v�B
w2B
wLB
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y>B
yrB
y�B
y�B
zB
z^B
zxB
zxB
z�B
z�B
{0B
{B
{B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|PB
|�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~wB
~�B
~�B
B
B
.B
.B
cB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	$�B	#nB	#TB	"�B	 �B	pB	!B	�B	�B	 �B	 �B	!bB	"B	#B	%B	*eB	MB	�oB	~�B	}�B	zxB	v�B	r�B	jB	ezB	_B	Z�B	Z�B	U2B	oOB	��B	��B	�:B	��B	� B	��B	��B	��B	�2B	�1B	��B	��B	��B	��B	��B
�B
)�B
~�B
�jB
ˬB
�jB+B4BW�B}B��B�?B�B�B�-B��B�hB�$B%B�BdB�vB�=B�@B��B�$B�;B�
B��B��B}"BzxBx8B`�BU�BGzB<�B2�B-�B)�BYB
�B
ޞB
ðB
��B
�_B
o�B
UB
G�B
7B
	B	�VB	�>B	��B	�?B	�vB	�B	kB	K�B	7�B	~B	
	B	�B��B�B��B�7B�B��B��B�hB�B��B�=B�B��B��B�HB�'B�qB��B�xB�7B�B�B��B��B��B��B��B��B��B�B��B��B��B�+B	 B	2B	2B	B	�B��B�'B��B��B�XB�>B��B�B�B��B�$B�B��B�6B�}B	xB	YB	B	�B	�B	�B	9B	�B	TB	�B	$B	�B	B	_B	�B	7B	�B	�B	B	�B	&B	&fB	'B	%`B	$B	$&B	%FB	%zB	$�B	&LB	+QB	/B	7�B	B[B	J�B	J�B	I�B	G�B	E�B	F�B	IlB	L�B	V�B	[�B	_VB	_B	\�B	[�B	YB	d&B	h�B	p;B	p�B	m�B	lB	q�B	n/B	j�B	m�B	w�B	{JB	��B	��B	�xB	�KB	��B	�BB	��B	��B	��B	��B	�QB	��B	��B	��B	��B	�#B	��B	��B	�B	�B	��B	��B	��B	��B	�_B	�yB	�]B	��B	�	B	�B	�^B	�xB	��B	�^B	��B	��B	ЗB	̈́B	�B	�%B	��B	�(B	��B	��B	�B	�&B	�B	өB	� B	��B	��B	҉B	�oB	�:B	��B	�:B	҉B	��B	�,B	��B	��B	֡B	�9B	��B	��B	��B	��B	�
B	��B	��B	��B	ּB	�+B	�B	ںB	��B	ݲB	�;B	�\B	�B	��B	�B	�&B	�tB	�B	�B	�&B	�B	��B	�B	�4B	�4B	�B	�4B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�@B	�tB	��B	�,B	�B	�B	�FB	��B	�B	�B	�RB	�RB	�B	��B	�
B	�$B	��B	�_B	�B	�0B	��B	��B	��B	�6B	�6B	�B	�"B	�WB	��B	�)B	�]B	��B	�B	�B	��B	��B	�5B	�B	�5B	��B	��B	��B	��B	�GB	��B	�-B	�aB	�|B	�B	�B	�TB	�B	�nB	�nB	��B	�+B	�zB	�fB	�fB	�RB	��B	��B	��B	�^B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	�"B	�"B	�B	��B	�]B	�.B	�}B	��B
 OB
�B
oB
UB
�B
�B
GB
B
9B
�B
�B
KB
	7B
	7B
	lB
	7B
	�B
	�B
	7B
KB
B
fB
KB
�B
�B
B
�B
�B
_B
zB
+B
�B
zB
�B
�B
	lB
	�B

	B

	B

	B

=B

�B

�B

�B
B
xB
�B
0B
B
�B
~B
�B
PB
(B
�B
B
}B
}B
�B
�B
NB
 B
:B
B
�B
{B
MB
B
B
�B
YB
+B
�B
�B
eB
B
�B
�B
	B
	B
�B
�B
)B
]B
xB
B
IB
�B
OB
�B
�B
B
�B
�B
�B
�B
�B
B
!B
�B
�B
�B
jB
�B
B
 'B
 'B
�B
 BB
 �B
!B
!bB
!|B
!�B
"NB
"NB
"NB
"hB
"hB
"�B
#TB
#nB
#�B
#�B
$&B
%FB
&LB
&�B
&�B
'B
'B
'B
'B
'RB
'RB
'�B
'�B
'�B
(
B
($B
(XB
(�B
)*B
)*B
)DB
)�B
*B
*B
*�B
*�B
*�B
+�B
+�B
,B
,B
,�B
-]B
-]B
-�B
-�B
-�B
-�B
.B
.cB
/ B
.�B
.�B
/�B
0!B
0B
0;B
0�B
1vB
2B
1�B
2B
2aB
2|B
3B
4B
4nB
4nB
4�B
4TB
5%B
5?B
5?B
5%B
5ZB
5�B
7�B
7�B
7fB
7�B
9�B
9�B
9�B
9�B
:�B
;dB
;dB
;dB
;�B
<PB
<�B
<�B
<jB
<�B
="B
=<B
=qB
=qB
=�B
=�B
=�B
=�B
=qB
="B
<�B
<�B
<�B
=<B
=�B
=�B
=�B
>B
>B
>B
=�B
=�B
=�B
>B
=�B
>BB
>]B
>wB
>wB
>wB
>�B
?B
?B
?HB
?cB
?}B
?�B
?�B
?�B
?�B
@ B
@OB
@OB
AB
A;B
A;B
A;B
AB
@�B
@�B
@�B
@�B
@�B
AB
A�B
A�B
A�B
B'B
BuB
CGB
C-B
C{B
C�B
C�B
DgB
D�B
D�B
ESB
E�B
E�B
E�B
FYB
F�B
G+B
G�B
HB
H1B
HKB
H�B
H�B
IB
I�B
I�B
J#B
J	B
J	B
J	B
J�B
KDB
KxB
K�B
K�B
L�B
MPB
M6B
MB
MPB
M6B
MPB
MjB
M�B
N<B
N�B
N�B
N�B
N�B
OB
OBB
O(B
OBB
O\B
PB
P�B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
R:B
RTB
R�B
R�B
S�B
S�B
TB
T{B
T{B
T�B
U2B
VB
U�B
VB
VSB
V�B
W
B
W$B
WsB
X+B
X+B
XyB
XyB
X�B
X�B
YB
YeB
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
\)B
\]B
\]B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^5B
^OB
^OB
^�B
_!B
_!B
_�B
_�B
_�B
_�B
`B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
a-B
a-B
abB
a�B
b4B
bhB
bhB
bhB
bhB
bhB
b�B
b�B
b�B
cB
cB
cTB
c�B
dB
d@B
dZB
d�B
d�B
d�B
e`B
e`B
e`B
e�B
e�B
f2B
f2B
f�B
f�B
f�B
gB
g8B
gRB
gRB
gRB
gRB
gmB
g�B
g�B
h$B
hsB
hsB
h�B
iDB
i�B
i�B
i�B
i�B
j0B
jeB
jB
j�B
j�B
kB
kQB
k�B
lB
l"B
lqB
l�B
lqB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
nB
n/B
nIB
n}B
n}B
n}B
n}B
n�B
n�B
n�B
n�B
o B
n�B
oB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pB
p!B
pB
poB
p�B
p�B
p�B
qAB
q'B
q'B
qB
qB
qvB
q�B
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
s3B
shB
shB
shB
sMB
shB
s�B
s�B
s�B
s�B
tB
t9B
tTB
t�B
t�B
uB
uB
u?B
u�B
u�B
vB
u�B
u�B
vB
v�B
v�B
v�B
w2B
wLB
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y>B
yrB
y�B
y�B
zB
z^B
zxB
zxB
z�B
z�B
{0B
{B
{B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|PB
|�B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~wB
~�B
~�B
B
B
.B
.B
cB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105228  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191234  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191234  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191234                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041241  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041241  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                