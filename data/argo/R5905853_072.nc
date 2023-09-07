CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:35:33Z creation;2022-06-04T17:35:33Z conversion to V3.1      
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
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20220604173533  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�Y?�#E1   @�Y@<M^p@.ؓt�j�c�fffff1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�ffB�  B˙�B�  B�  B�  B�  B���B�  B�  B�33B�B�  B�  B�  B���C�fC  C  C�fC	�fC  C�C33C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @	��@vff@�33@�33A��A=��A]��A}��A�  A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B�L�B�L�B�L�B��Bȳ3B�L�Bϳ3Bӳ3B׳3B۳3B߀ B�3B�3B��fB�L�B�3B��3B��3B�� C� CٚCٚC� C	� CٚC�4C�C� CٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9�4C;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#|�D#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDF|�DF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D�� D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�x D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�^f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7�A�6�A�8A�ZQA�^�Aʍ�Aʘ�Aʝ~Aʝ�Aʟ�Aʤ�Aʦ�Aʧ�Aʩ�AʮAʱ�Aʰ�Aʱ'AʳhAʳ�Aʺ^Aʻ�AʽAʼjAʿ�A��pA��HA�ߤA���A��A��A��]A�	�A�,�A�7�A�C�A�H�A�MjA�]dA�m�AˁoA˓uA˜CA˦�A��A�RTAÇ�A�_�A�"A�VA���A�]�A�-wA�
�A��A�#A��)A�s�A���A��A�<6A�!A���A�N�A��A�DA|�+Aw��ApX�AlqAi�mAd��A`�8A\�FAY�hAV iAR�AN�.AL�}AJ�AG��AD֡AC��AA�	AB'RAB9�AB;�AB$AA^�A@	A>�4A<�A:�)A9`BA8��A8W?A7�A6�A5JA4�A3��A2�]A2�3A2=A1�A0�cA/6�A-��A,�&A,\�A+t�A*�4A*?A)��A)5�A(�|A(q�A'�HA&Z�A%�A#��A#�A#�{A"QA"�A!��A!�A �A &�A��A�sArGAϫA�LA�A�A��A1A{�A	�A��AY�A��A^�AA�A�A��AS�A��AxA�PA�gAH�A�`A|�A�^A�[Ar�Ap;A�QA�A�A\�A~A�Am�A�_A�]A�qAh�AV�AI�A�AA A	�HAݘAv�A�A�hA�A� A;�Am�AϫA(�AC�A�+AA�A�&A�.A}VAaA2aA�]A��A�6A��Ag�AuA ��A�fA?A ��A �A ]d@��@�|@�q�@���@�{�@��P@�V�@���@���@�?@�@�/�@��@�7@�Dg@���@��@�A@�@�K^@��a@�/�@�c�@�7L@@�=q@�j@�hs@� \@��@ꄶ@���@��@�]�@��+@��@�PH@�U2@��|@���@�֡@���@��@��@�:@�+@�O�@�}�@��9@⠐@�Q�@�v`@��`@�r�@�4n@���@�B�@���@�i�@�@�@��;@ݓ�@�dZ@ݠ�@�n/@��]@�]�@ڿ�@ڂA@�h�@���@׺^@�;�@�!�@��W@�.I@Թ$@ԃ�@�tT@��@�ϫ@ӎ�@�}�@�O@�{�@�($@��@Ѱ�@��2@�_�@�1�@��@���@ϥ�@�qv@��K@�{�@�h
@�`�@�*�@ͳ�@͓@�
=@��Z@�/@ʣ�@�8�@�a�@��"@�Ĝ@�Xy@��D@�|�@��)@ƞ@�~(@�($@Œ:@�hs@���@�Ĝ@ĂA@��@�!-@��@�A�@�
=@���@��s@���@�M@�!@��9@�s�@��@�?�@��3@�_p@�)_@���@���@�j@�@�@�0U@��a@�C�@��@��)@��.@�4n@���@�u�@�?}@�	l@���@�	@�ƨ@��@���@�H�@�-w@��@���@�i�@�R�@�Ft@�.�@���@��@�6z@�s�@�C-@�$�@� �@���@���@���@��~@�]�@�=@���@���@�/�@���@�a@���@��o@���@�.I@��"@���@���@��@���@��o@�w�@��@�L�@�8�@�+�@�V@�p;@��@��Q@�}�@�	l@�C-@��h@��	@���@�kQ@�@�@�1@���@��~@�+@��E@��b@�~(@�Ov@�&�@�'�@��r@�-�@��@�YK@�J@� �@��
@���@�Z�@��@��!@�q�@�{�@�n�@�:*@��@�n/@���@��@�:�@��@��*@�f�@���@��1@�@��V@��P@��?@��Y@�@���@��	@�=�@�ȴ@���@���@�kQ@�+k@���@��;@���@�Y�@�C@��m@�R�@�O@���@���@���@�s@��@��)@���@�Ft@���@��)@���@��@��@��E@��?@��@��\@���@�tT@�:*@�1@�ԕ@���@���@��8@��F@�Xy@�7�@��.@��=@�j@�ȴ@�c @�7�@���@��K@���@�]�@��@��@���@��o@�Q@��g@�s@�Vm@�&@�C@��@��y@��@��@��Y@��D@��L@�Z�@���@���@��6@���@���@��f@�T�@�@��B@�n�@�&�@��@���@�H�@�/�@�@�҉@��4@�p;@�PH@�<�@��@��z@�{J@�C@��@��,@��@�l"@�8�@��}@�~�@�8@��@��@��@��'@���@�j@�J�@�	@�;@�f@33@~�,@~ �@}�@}V@|�@|ѷ@|�O@|PH@{�@{��@z��@z��@y�z@y��@y��@y��@y[W@x�?@w��@w�{@wX�@w�@v��@u:�@t�?@t��@t�@th�@t]d@t>B@s�+@s)_@rں@r��@r�@rM�@r_@q��@qe,@q0�@pQ�@p7@o�@@n��@n�F@n)�@m��@m��@m��@m�@mzx@m5�@m;@l�?@l>B@kl�@k�@j�@j�,@j��@j\�@i�@h�`@hC-@h�@g�&@g��@gl�@g'�@f��@f͟@f��@fz@f�@ehs@d��@d�O@c��@c6z@b��@bOv@b#:@a[W@`��@`PH@`"h@`�@_~�@_\)@_�@^�@]��@]s�@]S&@\�	@\�v@\�v@\�@\�[@\�.@\H@\7�@[�	@Zu%@Y�@Y�n@Y@@XG@W��@W�0@Wv`@W1�@Vz@V�@U��@U:�@T�5@T��@Tѷ@T��@T��@Tz�@S�Q@S�*@S~�@Sb�@S�@R��@R��@ROv@Q�'@P��@P��@P��@PɆ@P�e@P��@P�4@P|�@P�@O��@Og�@O"�@Na|@N)�@M�d@M2a@L�@L(�@K��@K�@K.I@J�\@J@I��@IL�@I�@H��@G��@GE9@G>�@G�@F^5@E��@Eo @DĜ@C�&@C��@Cg�@C'�@B�y@B��@BV@B:*@B�@A��@A�C@Am]@A%F@A�@@�@@�p@@��@@tT@@,=@?�	@?A�@?/�@?�@>ff@>+k@>	@=��@=&�@<%�@;�@;��@;��@:�@:Ov@9��@9�@9��@9hs@90�@8��@8]d@8PH@8-�@7�@7{J@7�@7S@6��@6�X@6� @6YK@66�@6$�@6
�@5�N@5��@5Q�@58�@5@4��@4w�@3�g@3��@3˒@3�@3�a@3�@3�@3��@3�F@3��@3l�@2�@2��@2i�@2e@2 �@1�@1��@0�|@0��@0`�@0Xy@0>B@0~@/�@/��@/|�@/qv@/A�@.�@.��@.$�@-ϫ@-�H@-O�@,�@,m�@+�{@+b�@+>�@*�@*xl@*M�@*#:@)�z@)��@)^�@(|�@(M@'�@'g�@'+@&�2@&{�@&6�@&&�@&�@&�@%��@%��@%�H@%�@$ی@$��@$C-@#��@#��@#�@#�@#�g@#��@#�@@#��@#��@#�f@#W?@#6z@#/�@#�@#Y@#�@"��@"u%@"@�@!��@!�h@!q@ ��@ 1@��@��@�:@v`@o�@g�@O@6z@��@_�@Ta@J�@&�@u@�@��@�C@+�@��@�@K^@�m@��@{J@dZ@��@Z�@��@��@+�@�f@�O@��@K^@�@�a@�w@��@�w@�0@�w@��@��@g�@�@�@�F@�@ϫ@�=@O�@!�@�@��@�`@�p@Z@�@��@�@�	@�c@��@�+@q�@c @@�@�o@�S@p�@^�@F@4@��@�O@c�@�A@H�@+@�@�y@��@C�@ �@�-@c@p�@c�@�@ی@��@�)@�$@��@[�@9X@x@�&@�
@��@6z@�@�@(@ i@
�s@
#:@	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7�A�6�A�8A�ZQA�^�Aʍ�Aʘ�Aʝ~Aʝ�Aʟ�Aʤ�Aʦ�Aʧ�Aʩ�AʮAʱ�Aʰ�Aʱ'AʳhAʳ�Aʺ^Aʻ�AʽAʼjAʿ�A��pA��HA�ߤA���A��A��A��]A�	�A�,�A�7�A�C�A�H�A�MjA�]dA�m�AˁoA˓uA˜CA˦�A��A�RTAÇ�A�_�A�"A�VA���A�]�A�-wA�
�A��A�#A��)A�s�A���A��A�<6A�!A���A�N�A��A�DA|�+Aw��ApX�AlqAi�mAd��A`�8A\�FAY�hAV iAR�AN�.AL�}AJ�AG��AD֡AC��AA�	AB'RAB9�AB;�AB$AA^�A@	A>�4A<�A:�)A9`BA8��A8W?A7�A6�A5JA4�A3��A2�]A2�3A2=A1�A0�cA/6�A-��A,�&A,\�A+t�A*�4A*?A)��A)5�A(�|A(q�A'�HA&Z�A%�A#��A#�A#�{A"QA"�A!��A!�A �A &�A��A�sArGAϫA�LA�A�A��A1A{�A	�A��AY�A��A^�AA�A�A��AS�A��AxA�PA�gAH�A�`A|�A�^A�[Ar�Ap;A�QA�A�A\�A~A�Am�A�_A�]A�qAh�AV�AI�A�AA A	�HAݘAv�A�A�hA�A� A;�Am�AϫA(�AC�A�+AA�A�&A�.A}VAaA2aA�]A��A�6A��Ag�AuA ��A�fA?A ��A �A ]d@��@�|@�q�@���@�{�@��P@�V�@���@���@�?@�@�/�@��@�7@�Dg@���@��@�A@�@�K^@��a@�/�@�c�@�7L@@�=q@�j@�hs@� \@��@ꄶ@���@��@�]�@��+@��@�PH@�U2@��|@���@�֡@���@��@��@�:@�+@�O�@�}�@��9@⠐@�Q�@�v`@��`@�r�@�4n@���@�B�@���@�i�@�@�@��;@ݓ�@�dZ@ݠ�@�n/@��]@�]�@ڿ�@ڂA@�h�@���@׺^@�;�@�!�@��W@�.I@Թ$@ԃ�@�tT@��@�ϫ@ӎ�@�}�@�O@�{�@�($@��@Ѱ�@��2@�_�@�1�@��@���@ϥ�@�qv@��K@�{�@�h
@�`�@�*�@ͳ�@͓@�
=@��Z@�/@ʣ�@�8�@�a�@��"@�Ĝ@�Xy@��D@�|�@��)@ƞ@�~(@�($@Œ:@�hs@���@�Ĝ@ĂA@��@�!-@��@�A�@�
=@���@��s@���@�M@�!@��9@�s�@��@�?�@��3@�_p@�)_@���@���@�j@�@�@�0U@��a@�C�@��@��)@��.@�4n@���@�u�@�?}@�	l@���@�	@�ƨ@��@���@�H�@�-w@��@���@�i�@�R�@�Ft@�.�@���@��@�6z@�s�@�C-@�$�@� �@���@���@���@��~@�]�@�=@���@���@�/�@���@�a@���@��o@���@�.I@��"@���@���@��@���@��o@�w�@��@�L�@�8�@�+�@�V@�p;@��@��Q@�}�@�	l@�C-@��h@��	@���@�kQ@�@�@�1@���@��~@�+@��E@��b@�~(@�Ov@�&�@�'�@��r@�-�@��@�YK@�J@� �@��
@���@�Z�@��@��!@�q�@�{�@�n�@�:*@��@�n/@���@��@�:�@��@��*@�f�@���@��1@�@��V@��P@��?@��Y@�@���@��	@�=�@�ȴ@���@���@�kQ@�+k@���@��;@���@�Y�@�C@��m@�R�@�O@���@���@���@�s@��@��)@���@�Ft@���@��)@���@��@��@��E@��?@��@��\@���@�tT@�:*@�1@�ԕ@���@���@��8@��F@�Xy@�7�@��.@��=@�j@�ȴ@�c @�7�@���@��K@���@�]�@��@��@���@��o@�Q@��g@�s@�Vm@�&@�C@��@��y@��@��@��Y@��D@��L@�Z�@���@���@��6@���@���@��f@�T�@�@��B@�n�@�&�@��@���@�H�@�/�@�@�҉@��4@�p;@�PH@�<�@��@��z@�{J@�C@��@��,@��@�l"@�8�@��}@�~�@�8@��@��@��@��'@���@�j@�J�@�	@�;@�f@33@~�,@~ �@}�@}V@|�@|ѷ@|�O@|PH@{�@{��@z��@z��@y�z@y��@y��@y��@y[W@x�?@w��@w�{@wX�@w�@v��@u:�@t�?@t��@t�@th�@t]d@t>B@s�+@s)_@rں@r��@r�@rM�@r_@q��@qe,@q0�@pQ�@p7@o�@@n��@n�F@n)�@m��@m��@m��@m�@mzx@m5�@m;@l�?@l>B@kl�@k�@j�@j�,@j��@j\�@i�@h�`@hC-@h�@g�&@g��@gl�@g'�@f��@f͟@f��@fz@f�@ehs@d��@d�O@c��@c6z@b��@bOv@b#:@a[W@`��@`PH@`"h@`�@_~�@_\)@_�@^�@]��@]s�@]S&@\�	@\�v@\�v@\�@\�[@\�.@\H@\7�@[�	@Zu%@Y�@Y�n@Y@@XG@W��@W�0@Wv`@W1�@Vz@V�@U��@U:�@T�5@T��@Tѷ@T��@T��@Tz�@S�Q@S�*@S~�@Sb�@S�@R��@R��@ROv@Q�'@P��@P��@P��@PɆ@P�e@P��@P�4@P|�@P�@O��@Og�@O"�@Na|@N)�@M�d@M2a@L�@L(�@K��@K�@K.I@J�\@J@I��@IL�@I�@H��@G��@GE9@G>�@G�@F^5@E��@Eo @DĜ@C�&@C��@Cg�@C'�@B�y@B��@BV@B:*@B�@A��@A�C@Am]@A%F@A�@@�@@�p@@��@@tT@@,=@?�	@?A�@?/�@?�@>ff@>+k@>	@=��@=&�@<%�@;�@;��@;��@:�@:Ov@9��@9�@9��@9hs@90�@8��@8]d@8PH@8-�@7�@7{J@7�@7S@6��@6�X@6� @6YK@66�@6$�@6
�@5�N@5��@5Q�@58�@5@4��@4w�@3�g@3��@3˒@3�@3�a@3�@3�@3��@3�F@3��@3l�@2�@2��@2i�@2e@2 �@1�@1��@0�|@0��@0`�@0Xy@0>B@0~@/�@/��@/|�@/qv@/A�@.�@.��@.$�@-ϫ@-�H@-O�@,�@,m�@+�{@+b�@+>�@*�@*xl@*M�@*#:@)�z@)��@)^�@(|�@(M@'�@'g�@'+@&�2@&{�@&6�@&&�@&�@&�@%��@%��@%�H@%�@$ی@$��@$C-@#��@#��@#�@#�@#�g@#��@#�@@#��@#��@#�f@#W?@#6z@#/�@#�@#Y@#�@"��@"u%@"@�@!��@!�h@!q@ ��@ 1@��@��@�:@v`@o�@g�@O@6z@��@_�@Ta@J�@&�@u@�@��@�C@+�@��@�@K^@�m@��@{J@dZ@��@Z�@��@��@+�@�f@�O@��@K^@�@�a@�w@��@�w@�0@�w@��@��@g�@�@�@�F@�@ϫ@�=@O�@!�@�@��@�`@�p@Z@�@��@�@�	@�c@��@�+@q�@c @@�@�o@�S@p�@^�@F@4@��@�O@c�@�A@H�@+@�@�y@��@C�@ �@�-@c@p�@c�@�@ی@��@�)@�$@��@[�@9X@x@�&@�
@��@6z@�@�@(@ i@
�s@
#:@	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�BuB�B�B+B2�B5%B4�B5%B7�B7�B8RB8�B:*B;B:�B:xB;B:�B<6B<�B<�B<�B=<BESBFBE9BBuBF�BH�BLdBR�B`'Bc�BiyBl�Br�B��B�\B��B��B��B�0B�DB�B	�B	�uB
(B
B
B
aB
�B	��B	�B	�*B	�&B	��B	�TB	ϫB	�~B	�GB	��B	�B	��B	��B	�7B	� B	e�B	X_B	MjB	@OB	8lB	4nB	1AB	/�B	*�B	'B	!�B	&�B	*�B	;�B	RB	X�B	f2B	i�B	lWB	l�B	o�B	t�B	{�B	�B	��B	�B	�HB	�1B	�:B	�pB	�AB	��B
�B
[B
�B
eB
B
%,B
*�B
$&B
!|B
 �B
$�B
%�B
&�B
)yB
)�B
*KB
)�B
(�B
$ZB
�B
�B
�B
 �B
�B
�B
�B
$B
B
�B
oB
B
B

�B

	B
IB
-CB
72B
5�B
5�B
3�B
2B
/�B
+�B
*�B
+kB
,qB
*�B
(�B
%�B
!�B
�B
	B
+B
�B
oB
�B
fB
�B	�qB	�dB	��B	��B	�2B	�2B	�rB	�8B	�|B	�;B	�5B	��B	�)B	��B	�6B	��B	�B	�B	�B	�ZB	�B	�,B	�,B	�`B	��B	�B	��B	�BB	޸B	�VB	�pB	�'B	��B	�!B	ބB	��B	�B	�\B	�&B	��B	�B	�B	��B
�B
 �B
 �B
�B	�cB	��B	��B	��B
�B
 OB	��B	�B	�B	�B	�mB	�2B	�B	��B	��B	�B	��B	�iB	�B	�9B	��B	�9B	�hB	�B	��B	��B	�wB	��B	��B	��B	�B	�qB	�B	�zB	��B	�;B	�B	�B	�tB	�FB	�B	��B
 OB
�B
 4B	��B
 B
�B	��B	��B	�RB	��B	�hB	��B	�B	�B	�IB	��B	�}B	� B	�B	�B	��B	��B	�DB	�xB	��B	�0B	�B	��B	��B	��B	�B	�$B	�VB	��B	�cB	�}B	��B
 OB
 OB
 iB
 B	��B
 �B
  B	��B
  B
 �B
 OB
 B
 OB
 iB
 �B
 �B
 �B
oB
;B
 B
 B
 �B
 iB
 �B
 iB
 OB	��B	��B	��B	��B	��B
  B	�}B	��B	�HB	�HB	�.B	�B	��B	�B	��B	�qB	�(B	��B	��B
 �B
�B
oB
B
 �B
 B	�}B	��B	�wB	�qB	��B
  B
;B
 B
B
B
 �B
B
 �B
 B
MB
�B
�B
�B
{B
GB
-B
�B
�B
�B
�B
aB
-B
�B
�B
B
B
�B
�B
mB
�B
?B
tB
�B
�B
�B
	�B
	RB
	�B
	�B

#B

=B

#B

#B

rB

rB

�B

�B
�B
xB
�B
6B
�B
VB
VB
VB
pB
�B
�B
�B
�B
�B
vB
B
�B
�B
vB
.B
vB
vB
�B
 B
�B
hB
�B
@B
@B
@B
@B
uB
[B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
(B
�B
B
}B
oB
�B
[B
FB
�B
�B
�B

B
mB
�B
B
MB
MB
�B
$B
SB
�B
gB
�B
9B
�B
�B
sB
�B
�B
�B
yB
�B
�B
�B
�B
�B
1B
B
1B
�B
�B
7B
�B
	B
WB
�B
�B
)B
IB
IB
IB
B
�B
B
�B
�B
"hB
"�B
#:B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%FB
&2B
&2B
&B
%�B
%�B
%zB
%zB
$�B
$�B
$ZB
$&B
#�B
$ZB
$�B
$�B
$�B
$�B
$�B
%FB
%zB
%�B
&�B
'�B
)B
)yB
*0B
*�B
+B
+kB
,�B
-�B
-�B
-�B
.cB
/ B
.�B
.}B
.�B
/5B
/�B
/�B
0;B
0�B
0�B
1[B
1[B
1vB
1�B
1�B
2B
2-B
2-B
2-B
2�B
2�B
3hB
3MB
3hB
3�B
3�B
3�B
4TB
4�B
5B
5?B
5B
5ZB
5ZB
5ZB
5�B
5�B
5�B
6+B
6`B
6`B
6�B
7B
7LB
7�B
7�B
7�B
7�B
7�B
8B
8RB
88B
8�B
9>B
9$B
9$B
9	B
9>B
9�B
:*B
:DB
:xB
:�B
:�B
<B
<B
<6B
<6B
<6B
<6B
<6B
<PB
<�B
=B
=<B
=VB
=VB
=�B
=�B
=�B
=�B
>�B
>wB
>�B
?B
>�B
?HB
?HB
?}B
?cB
?cB
?�B
@ B
@ B
?�B
@4B
@OB
@OB
@OB
@4B
@4B
@ B
@4B
@4B
@OB
@ B
?�B
@B
?�B
@�B
@�B
@�B
@�B
@�B
A B
AoB
A�B
AoB
B'B
B'B
BuB
B[B
BB
B�B
B�B
BuB
B�B
B�B
CaB
CaB
CaB
C�B
D�B
D�B
D�B
EB
EB
EB
EB
EB
EmB
E�B
E�B
FYB
FtB
GB
G�B
H1B
H�B
H�B
H�B
H�B
H�B
J	B
JXB
J�B
K�B
K�B
L0B
LB
L�B
L0B
L~B
MjB
M6B
MjB
MPB
M�B
M�B
MjB
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N<B
N�B
OBB
O�B
O�B
O�B
PHB
P.B
PHB
P�B
QB
Q�B
Q�B
Q�B
R:B
RTB
RoB
RTB
RoB
RTB
RTB
R�B
S&B
S&B
SB
S�B
S�B
TFB
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VSB
VSB
V�B
V�B
W$B
W$B
W$B
WYB
WsB
W�B
W�B
X_B
X_B
XEB
X+B
Y1B
X�B
YB
X�B
Y�B
Z�B
Z�B
Z�B
Z�B
[qB
[qB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\CB
\CB
\]B
\xB
\�B
\�B
\�B
]B
]/B
]�B
^5B
^OB
^�B
^�B
_;B
`'B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a|B
a�B
a�B
a�B
a�B
bB
b�B
cB
cB
cB
c B
c B
c B
cnB
c�B
c�B
c�B
c�B
dZB
dtB
d�B
d�B
e,B
eFB
e�B
e�B
e�B
ezB
eFB
d�B
eB
eB
e`B
e`B
e,B
fB
f2B
f�B
f�B
f�B
g8B
g�B
h$B
h>B
hXB
h>B
hsB
hsB
hXB
iDB
iDB
iyB
i�B
jB
jB
j0B
jB
jB
jeB
jB
jeB
j0B
j0B
jB
jeB
jB
jB
jeB
jB
j�B
j�B
j�B
j�B
k6B
k�B
k�B
l=B
l=B
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
nB
n/B
nB
n/B
n�B
n�B
oB
o�B
o�B
p!B
p!B
pB
poB
qB
q'B
q�B
q�B
q�B
raB
raB
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
u%B
uZB
u�B
utB
u�B
vFB
vzB
vzB
v`B
v`B
v+B
vB
vFB
v+B
vFB
v�B
v�B
v�B
v�B
v�B
x8B
xB
xRB
x�B
x�B
yrB
y�B
y�B
zDB
zxB
z�B
z�B
z^B
{0B
{�B
|B
{�B
{B
{�B
{�B
{B
{dB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�BuB�B�B+B2�B5%B4�B5%B7�B7�B8RB8�B:*B;B:�B:xB;B:�B<6B<�B<�B<�B=<BESBFBE9BBuBF�BH�BLdBR�B`'Bc�BiyBl�Br�B��B�\B��B��B��B�0B�DB�B	�B	�uB
(B
B
B
aB
�B	��B	�B	�*B	�&B	��B	�TB	ϫB	�~B	�GB	��B	�B	��B	��B	�7B	� B	e�B	X_B	MjB	@OB	8lB	4nB	1AB	/�B	*�B	'B	!�B	&�B	*�B	;�B	RB	X�B	f2B	i�B	lWB	l�B	o�B	t�B	{�B	�B	��B	�B	�HB	�1B	�:B	�pB	�AB	��B
�B
[B
�B
eB
B
%,B
*�B
$&B
!|B
 �B
$�B
%�B
&�B
)yB
)�B
*KB
)�B
(�B
$ZB
�B
�B
�B
 �B
�B
�B
�B
$B
B
�B
oB
B
B

�B

	B
IB
-CB
72B
5�B
5�B
3�B
2B
/�B
+�B
*�B
+kB
,qB
*�B
(�B
%�B
!�B
�B
	B
+B
�B
oB
�B
fB
�B	�qB	�dB	��B	��B	�2B	�2B	�rB	�8B	�|B	�;B	�5B	��B	�)B	��B	�6B	��B	�B	�B	�B	�ZB	�B	�,B	�,B	�`B	��B	�B	��B	�BB	޸B	�VB	�pB	�'B	��B	�!B	ބB	��B	�B	�\B	�&B	��B	�B	�B	��B
�B
 �B
 �B
�B	�cB	��B	��B	��B
�B
 OB	��B	�B	�B	�B	�mB	�2B	�B	��B	��B	�B	��B	�iB	�B	�9B	��B	�9B	�hB	�B	��B	��B	�wB	��B	��B	��B	�B	�qB	�B	�zB	��B	�;B	�B	�B	�tB	�FB	�B	��B
 OB
�B
 4B	��B
 B
�B	��B	��B	�RB	��B	�hB	��B	�B	�B	�IB	��B	�}B	� B	�B	�B	��B	��B	�DB	�xB	��B	�0B	�B	��B	��B	��B	�B	�$B	�VB	��B	�cB	�}B	��B
 OB
 OB
 iB
 B	��B
 �B
  B	��B
  B
 �B
 OB
 B
 OB
 iB
 �B
 �B
 �B
oB
;B
 B
 B
 �B
 iB
 �B
 iB
 OB	��B	��B	��B	��B	��B
  B	�}B	��B	�HB	�HB	�.B	�B	��B	�B	��B	�qB	�(B	��B	��B
 �B
�B
oB
B
 �B
 B	�}B	��B	�wB	�qB	��B
  B
;B
 B
B
B
 �B
B
 �B
 B
MB
�B
�B
�B
{B
GB
-B
�B
�B
�B
�B
aB
-B
�B
�B
B
B
�B
�B
mB
�B
?B
tB
�B
�B
�B
	�B
	RB
	�B
	�B

#B

=B

#B

#B

rB

rB

�B

�B
�B
xB
�B
6B
�B
VB
VB
VB
pB
�B
�B
�B
�B
�B
vB
B
�B
�B
vB
.B
vB
vB
�B
 B
�B
hB
�B
@B
@B
@B
@B
uB
[B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
(B
�B
B
}B
oB
�B
[B
FB
�B
�B
�B

B
mB
�B
B
MB
MB
�B
$B
SB
�B
gB
�B
9B
�B
�B
sB
�B
�B
�B
yB
�B
�B
�B
�B
�B
1B
B
1B
�B
�B
7B
�B
	B
WB
�B
�B
)B
IB
IB
IB
B
�B
B
�B
�B
"hB
"�B
#:B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%FB
&2B
&2B
&B
%�B
%�B
%zB
%zB
$�B
$�B
$ZB
$&B
#�B
$ZB
$�B
$�B
$�B
$�B
$�B
%FB
%zB
%�B
&�B
'�B
)B
)yB
*0B
*�B
+B
+kB
,�B
-�B
-�B
-�B
.cB
/ B
.�B
.}B
.�B
/5B
/�B
/�B
0;B
0�B
0�B
1[B
1[B
1vB
1�B
1�B
2B
2-B
2-B
2-B
2�B
2�B
3hB
3MB
3hB
3�B
3�B
3�B
4TB
4�B
5B
5?B
5B
5ZB
5ZB
5ZB
5�B
5�B
5�B
6+B
6`B
6`B
6�B
7B
7LB
7�B
7�B
7�B
7�B
7�B
8B
8RB
88B
8�B
9>B
9$B
9$B
9	B
9>B
9�B
:*B
:DB
:xB
:�B
:�B
<B
<B
<6B
<6B
<6B
<6B
<6B
<PB
<�B
=B
=<B
=VB
=VB
=�B
=�B
=�B
=�B
>�B
>wB
>�B
?B
>�B
?HB
?HB
?}B
?cB
?cB
?�B
@ B
@ B
?�B
@4B
@OB
@OB
@OB
@4B
@4B
@ B
@4B
@4B
@OB
@ B
?�B
@B
?�B
@�B
@�B
@�B
@�B
@�B
A B
AoB
A�B
AoB
B'B
B'B
BuB
B[B
BB
B�B
B�B
BuB
B�B
B�B
CaB
CaB
CaB
C�B
D�B
D�B
D�B
EB
EB
EB
EB
EB
EmB
E�B
E�B
FYB
FtB
GB
G�B
H1B
H�B
H�B
H�B
H�B
H�B
J	B
JXB
J�B
K�B
K�B
L0B
LB
L�B
L0B
L~B
MjB
M6B
MjB
MPB
M�B
M�B
MjB
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N<B
N�B
OBB
O�B
O�B
O�B
PHB
P.B
PHB
P�B
QB
Q�B
Q�B
Q�B
R:B
RTB
RoB
RTB
RoB
RTB
RTB
R�B
S&B
S&B
SB
S�B
S�B
TFB
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VSB
VSB
V�B
V�B
W$B
W$B
W$B
WYB
WsB
W�B
W�B
X_B
X_B
XEB
X+B
Y1B
X�B
YB
X�B
Y�B
Z�B
Z�B
Z�B
Z�B
[qB
[qB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\CB
\CB
\]B
\xB
\�B
\�B
\�B
]B
]/B
]�B
^5B
^OB
^�B
^�B
_;B
`'B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a|B
a�B
a�B
a�B
a�B
bB
b�B
cB
cB
cB
c B
c B
c B
cnB
c�B
c�B
c�B
c�B
dZB
dtB
d�B
d�B
e,B
eFB
e�B
e�B
e�B
ezB
eFB
d�B
eB
eB
e`B
e`B
e,B
fB
f2B
f�B
f�B
f�B
g8B
g�B
h$B
h>B
hXB
h>B
hsB
hsB
hXB
iDB
iDB
iyB
i�B
jB
jB
j0B
jB
jB
jeB
jB
jeB
j0B
j0B
jB
jeB
jB
jB
jeB
jB
j�B
j�B
j�B
j�B
k6B
k�B
k�B
l=B
l=B
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
nB
n/B
nB
n/B
n�B
n�B
oB
o�B
o�B
p!B
p!B
pB
poB
qB
q'B
q�B
q�B
q�B
raB
raB
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
u%B
uZB
u�B
utB
u�B
vFB
vzB
vzB
v`B
v`B
v+B
vB
vFB
v+B
vFB
v�B
v�B
v�B
v�B
v�B
x8B
xB
xRB
x�B
x�B
yrB
y�B
y�B
zDB
zxB
z�B
z�B
z^B
{0B
{�B
|B
{�B
{B
{�B
{�B
{B
{dB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104913  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173533  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173533                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023541  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023541  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                