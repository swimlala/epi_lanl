CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:35:08Z creation;2022-06-04T17:35:09Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173508  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�T7�JU�1   @�T7��?@-q&�x���cnfffff1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@ffBH  BP  BX  Ba��Bh  Bo��Bv  B�  B�33B���B���B���B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D~��D� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @0  @|��@�33@�  A��A=��A]��A}��A���A���A���A���A���A���A���A���B��BffBffBffB'ffB/ffB7ffB?��BGffBOffBWffBa  BgffBo  BuffBffB��fB�� B�� B�� B��3B��3B��3B��fB��3B��3B��3B�� B��3B��3B��3B��3B��Bǳ3Bˀ Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚC�4C�4CٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7�4C9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL��DMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD||�D|�fD}vfD}�fD~vfD~� DvfD�fD�;3D�{3D��fD��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��fD�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��fD�;3D�{3Dڻ3D��3D�;3D�~fDۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̸�A̺^A�ȴA���A���A̸RA̷�A̷LA̵�A̱�A̴A̵tA̶�A̱�A̬�A̯�A̪�A̦�A̤ḀzA̤Ḁ�A̦�A̦ḀFA̜�A�ffA��0A�F�A��A�uA�2�A�,�A�]/A�"�A�S�A��A��A��A��@A��|A�:�At�AoS&Ak�}Ae�$A_�A^33A\�AX�]AW��AV��AR��AO'�ALMAJ@�AH�eAH�AG�FAF�	AE�?AD�@AD	lABj�A@8A>�IA=cA<u�A;U2A:�NA9ϫA8�/A7��A7Z�A7�A6 iA5��A5'�A5�A4��A3�A2��A2�A0��A.��A-MA,˒A,sA+��A+J�A*�oA*�zA*�A)M�A(9�A'�}A'�}A'�!A&�A&�.A%�"A%HA#��A#*0A"SA!GA sA QA �A��A$�A�,AOA@AB[A�A��A[�Ai�Au�AZ�A(�A~�A/�A�jA�,A��At�A/A��AOAYKA1'A�.A;�AXyA�qAU2A��Ag�A�A�2Ao�AFtA
��A
g�A
�A	4A�^A��A��A`BA;A�A��A_A�
AU�A�MAxA2aAAA�vAR�A	A ]�@��7@���@�_�@�$@��@�iD@�"�@��@��&@���@���@��#@��#@�ƨ@��@��[@�`B@��u@�&�@��`@��}@�Z@�($@�	@�
�@��@�\)@��X@��r@�YK@�	�@���@��2@�@�j@�@�  @�rG@��Z@� i@��`@�@�E�@�2a@鰊@���@罥@��@�C�@��@��@�y>@�Xy@䖼@�|�@�H�@���@�P�@�@��@�xl@��@��m@���@���@޽<@�Ov@��@�Ov@��3@���@�'R@۸�@�!�@���@�iD@�N<@��M@�n�@�Ft@��@١�@�ݘ@��@��@��P@�5?@�4�@�d�@�L0@�($@Ѯ@�:�@�&@ο�@Ώ\@�M�@�:*@�3�@�@��@͚k@�j@�P�@�6z@�&@̱�@�-@���@�^�@�kQ@ʿ�@�ی@ʟ�@��@�/@�@��U@ȍ�@��@Ơ�@�9X@�i�@ƥz@ƻ�@�b@�5�@ĩ�@đ�@ă�@�H@��r@á�@��[@�@��K@�x�@�IR@��@���@�\�@��o@��P@� i@��'@�z@�~@���@��@�tT@��+@�a@��X@�خ@�<6@��j@�خ@�O@��H@���@�(�@��Z@�@��z@��r@��@��T@�'�@�ں@���@�q�@�!@��m@�n/@��@�q@��K@�G�@�`�@���@�m]@�+@���@�M@�C-@�0U@��@�B�@��@��@�c�@��@���@�j�@�L�@�9�@��@�Z@���@�o�@��@���@��f@��y@���@��@�ݘ@� \@���@���@�J�@�1@���@�4@�@���@���@�c�@���@��t@�Vm@���@��!@�u%@�1'@�'R@�M@��Z@���@���@�|@�Z�@�Dg@��K@�r�@�GE@�4@��>@��-@�|�@�:�@�
=@���@��!@�YK@��&@�� @��^@��@�7L@���@�m�@�I�@�1'@���@�m]@��.@�,=@���@���@���@�'�@�ی@�|�@�M@���@�dZ@�$t@��L@�L0@���@��@�� @�� @��@���@��^@��X@���@���@�4@�q@���@��@���@�Xy@�2�@��@��@��'@�}�@�(�@�҉@�r�@�~@��@�	�@��@���@�J#@��[@���@���@�?�@���@���@���@���@�v`@�4@���@�YK@� �@��@��S@�;d@��8@��@�\�@��@��@��@��@�oi@�C�@��@���@�S&@�q@�q@�W�@�u�@�9X@�M@��@���@�f�@��5@��O@��.@�g8@�7@��)@���@�;d@��@��@��|@��@���@���@��@�[�@��o@�x�@�?}@�#�@�%@���@�1'@���@���@���@�T�@��@��@���@�~�@�l"@�Ta@�6@�@~�@~�\@~��@~{�@~h
@~_�@~0U@}��@}S&@|��@|�@{qv@z�,@z{�@z$�@y�@y�~@yJ�@y-w@x�/@x?�@w�@wa@w1�@v�L@u��@ua�@t��@t��@ty>@t�@s�P@s\)@s8@r��@rxl@q�@q��@q�~@p��@p'R@o��@os@n�}@n=q@n	@m�#@m��@mq@l�9@l��@lbN@lD�@l:�@k�@k_p@k/�@j�c@jl�@i�^@im]@iDg@i0�@i%@h��@g�*@g�@fl�@e@eQ�@d��@dj@cݘ@c��@cn/@c=@co@bQ@aVm@`�9@_�@_��@_W?@^� @^u@]��@\�5@\��@[خ@Z��@Zu%@Z	@YF@Xl"@W��@W��@W��@W�@V�}@V	@Uϫ@U��@U5�@T��@Ty>@T6@T/�@S��@S�k@Sa@S>�@S4�@R�@RTa@Q�#@Q�@Q�-@Q%F@P��@P��@P�@P?�@O�4@O@N��@N3�@L�@L�@K��@K4�@J�y@J��@J^5@I�D@Iϫ@I�h@Ic@Is�@I^�@IIR@I8�@H�	@HC-@G�@G�K@G��@GF�@G�@F��@F��@F�1@F6�@E�~@E0�@D�@C��@C>�@C$t@C!-@C@C(@C@B��@Bl�@A��@@�e@@4n@?�w@?@O@>��@>��@>�A@>�@=�@=��@=rG@=?}@=@=�@<��@<C-@<9X@<"h@;��@;x@;K�@:��@:=q@9�d@9�=@9c�@9Vm@9�@8��@8�.@8S�@8M@7��@7��@7Z�@6�]@6a|@6W�@6M�@6!�@5�o@5ԕ@5��@5N<@5�@4��@4'R@3y�@2��@2l�@2($@2�@1�D@1�Z@1�o@1�@1�z@0�@0g8@/�&@/E9@.��@.�]@.��@.�R@.�F@.~�@.d�@.c @.u@-Y�@,�5@,�@,��@,��@,9X@+�g@+j�@+6z@*�]@*u%@*c @*#:@)�j@)o @)X@)G�@(�?@(,=@'� @'�4@'b�@'@O@&��@&s�@%�@%�'@%�@$�@$�@#x@#S@"��@"ȴ@"�@"�L@"��@"u%@"3�@!�#@!��@!��@!�d@!��@!��@!x�@!Vm@ �	@ g8@�}@+@��@Ta@	@�>@�=@/@��@�@��@��@�@��@�@S�@,=@@�g@��@|�@A�@(@��@��@Q@J�@6�@)�@{@�@�j@��@f�@+@�@_@/�@$@~@@��@�q@�@a@O@4�@!-@�"@�]@��@�@u%@Ta@0U@�@_@��@��@�n@\�@&�@�@��@�e@��@|�@U2@b@�
@��@dZ@�@҉@�x@W�@=q@�@�"@x�@f�@c�@a�@J�@�@ѷ@�@�@��@�@��@|�@e�@=@4�@,�@)_@�@ i@�"@��@��@��@u%@#:@ϫ@��@�=@zx@hs@0�@�/@��@e�@>B@ �@�@�6@��@��@�:@v`@b�@W?@'�@
�@
�x@
{�@
^5@
)�@
�@
@
@	��@	�o@	�@	�@	��@	�@		l@��@��@�Y@z�@u�@Z@9X@M@��@��@~�@6z@��@�1@YK@E�@+k@	@��@o @Dg@2a@+@�`@��@�@�Y@*�@خ@��@b�@F�@9�@�@�@��@�b@��@��@��@��@��@�x@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̸�A̺^A�ȴA���A���A̸RA̷�A̷LA̵�A̱�A̴A̵tA̶�A̱�A̬�A̯�A̪�A̦�A̤ḀzA̤Ḁ�A̦�A̦ḀFA̜�A�ffA��0A�F�A��A�uA�2�A�,�A�]/A�"�A�S�A��A��A��A��@A��|A�:�At�AoS&Ak�}Ae�$A_�A^33A\�AX�]AW��AV��AR��AO'�ALMAJ@�AH�eAH�AG�FAF�	AE�?AD�@AD	lABj�A@8A>�IA=cA<u�A;U2A:�NA9ϫA8�/A7��A7Z�A7�A6 iA5��A5'�A5�A4��A3�A2��A2�A0��A.��A-MA,˒A,sA+��A+J�A*�oA*�zA*�A)M�A(9�A'�}A'�}A'�!A&�A&�.A%�"A%HA#��A#*0A"SA!GA sA QA �A��A$�A�,AOA@AB[A�A��A[�Ai�Au�AZ�A(�A~�A/�A�jA�,A��At�A/A��AOAYKA1'A�.A;�AXyA�qAU2A��Ag�A�A�2Ao�AFtA
��A
g�A
�A	4A�^A��A��A`BA;A�A��A_A�
AU�A�MAxA2aAAA�vAR�A	A ]�@��7@���@�_�@�$@��@�iD@�"�@��@��&@���@���@��#@��#@�ƨ@��@��[@�`B@��u@�&�@��`@��}@�Z@�($@�	@�
�@��@�\)@��X@��r@�YK@�	�@���@��2@�@�j@�@�  @�rG@��Z@� i@��`@�@�E�@�2a@鰊@���@罥@��@�C�@��@��@�y>@�Xy@䖼@�|�@�H�@���@�P�@�@��@�xl@��@��m@���@���@޽<@�Ov@��@�Ov@��3@���@�'R@۸�@�!�@���@�iD@�N<@��M@�n�@�Ft@��@١�@�ݘ@��@��@��P@�5?@�4�@�d�@�L0@�($@Ѯ@�:�@�&@ο�@Ώ\@�M�@�:*@�3�@�@��@͚k@�j@�P�@�6z@�&@̱�@�-@���@�^�@�kQ@ʿ�@�ی@ʟ�@��@�/@�@��U@ȍ�@��@Ơ�@�9X@�i�@ƥz@ƻ�@�b@�5�@ĩ�@đ�@ă�@�H@��r@á�@��[@�@��K@�x�@�IR@��@���@�\�@��o@��P@� i@��'@�z@�~@���@��@�tT@��+@�a@��X@�خ@�<6@��j@�خ@�O@��H@���@�(�@��Z@�@��z@��r@��@��T@�'�@�ں@���@�q�@�!@��m@�n/@��@�q@��K@�G�@�`�@���@�m]@�+@���@�M@�C-@�0U@��@�B�@��@��@�c�@��@���@�j�@�L�@�9�@��@�Z@���@�o�@��@���@��f@��y@���@��@�ݘ@� \@���@���@�J�@�1@���@�4@�@���@���@�c�@���@��t@�Vm@���@��!@�u%@�1'@�'R@�M@��Z@���@���@�|@�Z�@�Dg@��K@�r�@�GE@�4@��>@��-@�|�@�:�@�
=@���@��!@�YK@��&@�� @��^@��@�7L@���@�m�@�I�@�1'@���@�m]@��.@�,=@���@���@���@�'�@�ی@�|�@�M@���@�dZ@�$t@��L@�L0@���@��@�� @�� @��@���@��^@��X@���@���@�4@�q@���@��@���@�Xy@�2�@��@��@��'@�}�@�(�@�҉@�r�@�~@��@�	�@��@���@�J#@��[@���@���@�?�@���@���@���@���@�v`@�4@���@�YK@� �@��@��S@�;d@��8@��@�\�@��@��@��@��@�oi@�C�@��@���@�S&@�q@�q@�W�@�u�@�9X@�M@��@���@�f�@��5@��O@��.@�g8@�7@��)@���@�;d@��@��@��|@��@���@���@��@�[�@��o@�x�@�?}@�#�@�%@���@�1'@���@���@���@�T�@��@��@���@�~�@�l"@�Ta@�6@�@~�@~�\@~��@~{�@~h
@~_�@~0U@}��@}S&@|��@|�@{qv@z�,@z{�@z$�@y�@y�~@yJ�@y-w@x�/@x?�@w�@wa@w1�@v�L@u��@ua�@t��@t��@ty>@t�@s�P@s\)@s8@r��@rxl@q�@q��@q�~@p��@p'R@o��@os@n�}@n=q@n	@m�#@m��@mq@l�9@l��@lbN@lD�@l:�@k�@k_p@k/�@j�c@jl�@i�^@im]@iDg@i0�@i%@h��@g�*@g�@fl�@e@eQ�@d��@dj@cݘ@c��@cn/@c=@co@bQ@aVm@`�9@_�@_��@_W?@^� @^u@]��@\�5@\��@[خ@Z��@Zu%@Z	@YF@Xl"@W��@W��@W��@W�@V�}@V	@Uϫ@U��@U5�@T��@Ty>@T6@T/�@S��@S�k@Sa@S>�@S4�@R�@RTa@Q�#@Q�@Q�-@Q%F@P��@P��@P�@P?�@O�4@O@N��@N3�@L�@L�@K��@K4�@J�y@J��@J^5@I�D@Iϫ@I�h@Ic@Is�@I^�@IIR@I8�@H�	@HC-@G�@G�K@G��@GF�@G�@F��@F��@F�1@F6�@E�~@E0�@D�@C��@C>�@C$t@C!-@C@C(@C@B��@Bl�@A��@@�e@@4n@?�w@?@O@>��@>��@>�A@>�@=�@=��@=rG@=?}@=@=�@<��@<C-@<9X@<"h@;��@;x@;K�@:��@:=q@9�d@9�=@9c�@9Vm@9�@8��@8�.@8S�@8M@7��@7��@7Z�@6�]@6a|@6W�@6M�@6!�@5�o@5ԕ@5��@5N<@5�@4��@4'R@3y�@2��@2l�@2($@2�@1�D@1�Z@1�o@1�@1�z@0�@0g8@/�&@/E9@.��@.�]@.��@.�R@.�F@.~�@.d�@.c @.u@-Y�@,�5@,�@,��@,��@,9X@+�g@+j�@+6z@*�]@*u%@*c @*#:@)�j@)o @)X@)G�@(�?@(,=@'� @'�4@'b�@'@O@&��@&s�@%�@%�'@%�@$�@$�@#x@#S@"��@"ȴ@"�@"�L@"��@"u%@"3�@!�#@!��@!��@!�d@!��@!��@!x�@!Vm@ �	@ g8@�}@+@��@Ta@	@�>@�=@/@��@�@��@��@�@��@�@S�@,=@@�g@��@|�@A�@(@��@��@Q@J�@6�@)�@{@�@�j@��@f�@+@�@_@/�@$@~@@��@�q@�@a@O@4�@!-@�"@�]@��@�@u%@Ta@0U@�@_@��@��@�n@\�@&�@�@��@�e@��@|�@U2@b@�
@��@dZ@�@҉@�x@W�@=q@�@�"@x�@f�@c�@a�@J�@�@ѷ@�@�@��@�@��@|�@e�@=@4�@,�@)_@�@ i@�"@��@��@��@u%@#:@ϫ@��@�=@zx@hs@0�@�/@��@e�@>B@ �@�@�6@��@��@�:@v`@b�@W?@'�@
�@
�x@
{�@
^5@
)�@
�@
@
@	��@	�o@	�@	�@	��@	�@		l@��@��@�Y@z�@u�@Z@9X@M@��@��@~�@6z@��@�1@YK@E�@+k@	@��@o @Dg@2a@+@�`@��@�@�Y@*�@خ@��@b�@F�@9�@�@�@��@�b@��@��@��@��@��@�x@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�"B�"B��B��B��B��B��B��B��B��B��B��B�jB�jB�PB�B��B�DB�B��B�B��B��B��B�B3�BO�B��B	�B	�B	�yB	�[B	��B	�qB	��B	�*B	��B	��B	��B	��B	jB	\�B	OvB	E9B	2-B	-wB	%�B	�B	�B	�B	�B	�B	�B	�B	B	�B	)B	"�B	6+B	:�B	=B	F?B	T�B	_�B	f2B	iyB	v�B	|6B	�EB	�vB	�?B	�dB	��B	�B	�mB	��B
�B
�B
�B
($B
*0B
)�B
*0B
-�B
-�B
.}B
-�B
.�B
.}B
.IB
0oB
3�B
6zB
5�B
<�B
K^B
H�B
FYB
A�B
A�B
?B
>�B
=<B
9XB
;B
>B
AoB
B�B
B[B
A�B
?.B
=�B
>�B
=B
?}B
C-B
D�B
F%B
I�B
J�B
O�B
L�B
J�B
EB
@�B
=qB
;�B
7�B
6`B
/�B
-B
)�B
"�B
dB
KB
B
uB
�B
 �B	�dB	�-B	�9B	�B	�B	�IB	��B	�B	�B	�hB	�B	�BB	޸B	��B	ݲB	ܒB	��B	�dB	�VB	��B	�|B	��B	ޞB	�B	�~B	�-B	�B	�B	�B	�B	�B	�nB	�nB	�FB	�,B	�B	�FB	�FB	�,B	�zB	�FB	�`B	�B	�B	�B	�DB	�B	�5B	��B	�B	��B	��B	�fB	�fB	�lB	�rB	�jB	�B	��B	��B	�B	�MB	�B	�oB	�B	�WB	��B	�B	��B	�KB	�>B	�RB	�LB	�B	�B	�sB	�WB	�B	�)B	��B	�B	��B	��B	��B	�fB	��B	��B	�fB	��B	�.B
 OB
UB
�B
�B
�B
�B
gB
�B
 B
 4B
�B
�B
�B
B

�B

�B

#B
	lB
�B
jB
�B	��B	�jB	�dB	�rB	��B	�$B	�lB	��B	�zB	�`B	��B	�fB	��B	��B	��B	�	B	��B	��B	�B	�"B	��B
aB
GB
B
 B
  B
[B
�B
�B
[B
 B
B
�B
�B
[B	��B
 �B
�B
B
�B
fB
�B
�B
fB
KB
�B
	B
�B
	7B
	7B
	B
	RB
	7B
�B
�B
�B
�B
	B
�B
�B
�B
KB
�B
YB
�B
%B
B
�B
%B
9B
MB
[B
 �B
 B
�B
 B
AB
3B
�B
%B
B
1B
1B
�B
EB
B
�B
B
9B
�B
B
B
B
 B	��B	�cB
 �B	�.B	�cB	��B	�}B	�HB	��B
  B
 OB
 �B
 �B
 B
-B
�B
-B
�B
B
uB
�B
B
�B
�B
�B
�B
�B
�B
MB
�B
B
B
�B
�B
�B
�B
B
�B
	RB
�B
	�B
�B
B
6B
PB
B
"B
<B
pB
�B
B
�B
�B
oB
�B
�B
B
B
�B
�B
hB
�B
�B
:B
oB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
gB
MB
MB
gB
�B
�B
SB

B
YB
+B
�B
�B
eB
B
�B
�B
	B
�B
	B
qB
B
�B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
!|B
!bB
!HB
!HB
!-B
!bB
!�B
!|B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"hB
"NB
"�B
"�B
"�B
"�B
#B
#B
#B
#B
#TB
#�B
#�B
#�B
#�B
#�B
#�B
$B
$ZB
#�B
$ZB
$�B
$�B
$�B
$�B
%`B
%�B
%`B
%B
&�B
'�B
*B
+6B
+�B
,B
-�B
./B
.�B
/ B
.�B
/5B
/iB
/OB
0oB
0�B
0�B
1B
1B
1'B
1'B
1[B
1[B
1vB
1�B
2�B
2�B
2�B
2�B
3B
4B
4�B
4�B
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7�B
8RB
8�B
9$B
9rB
9�B
9�B
9�B
9�B
:*B
:�B
:�B
;B
;0B
;B
;�B
;dB
;�B
;�B
;�B
<B
<�B
<�B
=B
=�B
=qB
=�B
=qB
=�B
=�B
>�B
>�B
>�B
?�B
?}B
?�B
?�B
?�B
@OB
@�B
@�B
@�B
@�B
@�B
AUB
AoB
A�B
A�B
BB
B�B
B�B
B�B
B�B
B�B
B�B
DMB
D3B
D�B
EB
ESB
E�B
FB
F�B
F�B
F�B
F�B
F�B
G�B
H1B
HfB
I7B
IlB
I�B
JXB
J=B
J=B
I�B
IB
I7B
IRB
H�B
IRB
I7B
I�B
JrB
JrB
JXB
J�B
J�B
K)B
KDB
K�B
K�B
K�B
LJB
LdB
LdB
L�B
L�B
MB
L�B
L�B
MB
MjB
M�B
M�B
MPB
M�B
M�B
M�B
M�B
M�B
NpB
NpB
N�B
N�B
P.B
P}B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
SB
T,B
T�B
T�B
T�B
UB
T�B
T�B
U2B
UMB
U�B
VB
V9B
V�B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
Y1B
YB
Y�B
ZQB
ZQB
Z�B
Z�B
[#B
[qB
[�B
[�B
[�B
\B
\B
\xB
\�B
\�B
\�B
]/B
]/B
]IB
]dB
]�B
^B
^B
^jB
^jB
^�B
^�B
_B
_;B
_VB
_VB
_�B
_�B
`'B
`�B
`\B
`vB
`�B
`�B
`�B
`�B
a-B
aB
aHB
abB
a�B
bhB
bhB
b�B
b�B
b�B
b�B
b�B
bhB
bhB
cTB
c�B
c�B
d&B
d&B
d@B
dB
d@B
d&B
d&B
d@B
c�B
dZB
d�B
d�B
eB
d�B
d�B
eB
eFB
e�B
e�B
e�B
fLB
f2B
f�B
f�B
g8B
gB
gB
g�B
h$B
hXB
h�B
h�B
iB
iDB
i�B
jKB
j�B
kB
j�B
k�B
k�B
lWB
lqB
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
n�B
o B
o�B
p;B
poB
p�B
p�B
p�B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rB
rB
r-B
r|B
r�B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
uB
utB
u�B
utB
utB
utB
u�B
u�B
vB
v+B
vFB
vzB
v`B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
wfB
wfB
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
x�B
x�B
y>B
y>B
yXB
y�B
zB
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|jB
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
~(B
~(B
~(B
~(B
~]B
~wB
~�B
~�B
HB
cB
}B
�B
�B
�B
�OB
��B
��B
��B
��B
�B
�UB
�UB
��B
�UB
��B
��B
��B
��B
��B
�[B
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
�-B
��B
��B
�MB
�gB
�gB
�gB
��B
��B
��B
��B
�9B
�9B
��B
��B
�%B
�tB
�tB
�tB
��B
��B
�EB
�zB
�zB
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�XB
�=B
�XB
�#B
�XB
�=B
�=B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�"B�"B��B��B��B��B��B��B��B��B��B��B�jB�jB�PB�B��B�DB�B��B�B��B��B��B�B3�BO�B��B	�B	�B	�yB	�[B	��B	�qB	��B	�*B	��B	��B	��B	��B	jB	\�B	OvB	E9B	2-B	-wB	%�B	�B	�B	�B	�B	�B	�B	�B	B	�B	)B	"�B	6+B	:�B	=B	F?B	T�B	_�B	f2B	iyB	v�B	|6B	�EB	�vB	�?B	�dB	��B	�B	�mB	��B
�B
�B
�B
($B
*0B
)�B
*0B
-�B
-�B
.}B
-�B
.�B
.}B
.IB
0oB
3�B
6zB
5�B
<�B
K^B
H�B
FYB
A�B
A�B
?B
>�B
=<B
9XB
;B
>B
AoB
B�B
B[B
A�B
?.B
=�B
>�B
=B
?}B
C-B
D�B
F%B
I�B
J�B
O�B
L�B
J�B
EB
@�B
=qB
;�B
7�B
6`B
/�B
-B
)�B
"�B
dB
KB
B
uB
�B
 �B	�dB	�-B	�9B	�B	�B	�IB	��B	�B	�B	�hB	�B	�BB	޸B	��B	ݲB	ܒB	��B	�dB	�VB	��B	�|B	��B	ޞB	�B	�~B	�-B	�B	�B	�B	�B	�B	�nB	�nB	�FB	�,B	�B	�FB	�FB	�,B	�zB	�FB	�`B	�B	�B	�B	�DB	�B	�5B	��B	�B	��B	��B	�fB	�fB	�lB	�rB	�jB	�B	��B	��B	�B	�MB	�B	�oB	�B	�WB	��B	�B	��B	�KB	�>B	�RB	�LB	�B	�B	�sB	�WB	�B	�)B	��B	�B	��B	��B	��B	�fB	��B	��B	�fB	��B	�.B
 OB
UB
�B
�B
�B
�B
gB
�B
 B
 4B
�B
�B
�B
B

�B

�B

#B
	lB
�B
jB
�B	��B	�jB	�dB	�rB	��B	�$B	�lB	��B	�zB	�`B	��B	�fB	��B	��B	��B	�	B	��B	��B	�B	�"B	��B
aB
GB
B
 B
  B
[B
�B
�B
[B
 B
B
�B
�B
[B	��B
 �B
�B
B
�B
fB
�B
�B
fB
KB
�B
	B
�B
	7B
	7B
	B
	RB
	7B
�B
�B
�B
�B
	B
�B
�B
�B
KB
�B
YB
�B
%B
B
�B
%B
9B
MB
[B
 �B
 B
�B
 B
AB
3B
�B
%B
B
1B
1B
�B
EB
B
�B
B
9B
�B
B
B
B
 B	��B	�cB
 �B	�.B	�cB	��B	�}B	�HB	��B
  B
 OB
 �B
 �B
 B
-B
�B
-B
�B
B
uB
�B
B
�B
�B
�B
�B
�B
�B
MB
�B
B
B
�B
�B
�B
�B
B
�B
	RB
�B
	�B
�B
B
6B
PB
B
"B
<B
pB
�B
B
�B
�B
oB
�B
�B
B
B
�B
�B
hB
�B
�B
:B
oB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
gB
MB
MB
gB
�B
�B
SB

B
YB
+B
�B
�B
eB
B
�B
�B
	B
�B
	B
qB
B
�B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
!|B
!bB
!HB
!HB
!-B
!bB
!�B
!|B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"hB
"NB
"�B
"�B
"�B
"�B
#B
#B
#B
#B
#TB
#�B
#�B
#�B
#�B
#�B
#�B
$B
$ZB
#�B
$ZB
$�B
$�B
$�B
$�B
%`B
%�B
%`B
%B
&�B
'�B
*B
+6B
+�B
,B
-�B
./B
.�B
/ B
.�B
/5B
/iB
/OB
0oB
0�B
0�B
1B
1B
1'B
1'B
1[B
1[B
1vB
1�B
2�B
2�B
2�B
2�B
3B
4B
4�B
4�B
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7�B
8RB
8�B
9$B
9rB
9�B
9�B
9�B
9�B
:*B
:�B
:�B
;B
;0B
;B
;�B
;dB
;�B
;�B
;�B
<B
<�B
<�B
=B
=�B
=qB
=�B
=qB
=�B
=�B
>�B
>�B
>�B
?�B
?}B
?�B
?�B
?�B
@OB
@�B
@�B
@�B
@�B
@�B
AUB
AoB
A�B
A�B
BB
B�B
B�B
B�B
B�B
B�B
B�B
DMB
D3B
D�B
EB
ESB
E�B
FB
F�B
F�B
F�B
F�B
F�B
G�B
H1B
HfB
I7B
IlB
I�B
JXB
J=B
J=B
I�B
IB
I7B
IRB
H�B
IRB
I7B
I�B
JrB
JrB
JXB
J�B
J�B
K)B
KDB
K�B
K�B
K�B
LJB
LdB
LdB
L�B
L�B
MB
L�B
L�B
MB
MjB
M�B
M�B
MPB
M�B
M�B
M�B
M�B
M�B
NpB
NpB
N�B
N�B
P.B
P}B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
SB
T,B
T�B
T�B
T�B
UB
T�B
T�B
U2B
UMB
U�B
VB
V9B
V�B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
Y1B
YB
Y�B
ZQB
ZQB
Z�B
Z�B
[#B
[qB
[�B
[�B
[�B
\B
\B
\xB
\�B
\�B
\�B
]/B
]/B
]IB
]dB
]�B
^B
^B
^jB
^jB
^�B
^�B
_B
_;B
_VB
_VB
_�B
_�B
`'B
`�B
`\B
`vB
`�B
`�B
`�B
`�B
a-B
aB
aHB
abB
a�B
bhB
bhB
b�B
b�B
b�B
b�B
b�B
bhB
bhB
cTB
c�B
c�B
d&B
d&B
d@B
dB
d@B
d&B
d&B
d@B
c�B
dZB
d�B
d�B
eB
d�B
d�B
eB
eFB
e�B
e�B
e�B
fLB
f2B
f�B
f�B
g8B
gB
gB
g�B
h$B
hXB
h�B
h�B
iB
iDB
i�B
jKB
j�B
kB
j�B
k�B
k�B
lWB
lqB
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
n�B
o B
o�B
p;B
poB
p�B
p�B
p�B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rB
rB
r-B
r|B
r�B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
uB
utB
u�B
utB
utB
utB
u�B
u�B
vB
v+B
vFB
vzB
v`B
v�B
v�B
v�B
v�B
v�B
wB
wLB
wLB
wfB
wfB
w�B
w�B
w�B
xB
x8B
x�B
x�B
x�B
x�B
x�B
y>B
y>B
yXB
y�B
zB
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
{�B
{�B
|B
|jB
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
~(B
~(B
~(B
~(B
~]B
~wB
~�B
~�B
HB
cB
}B
�B
�B
�B
�OB
��B
��B
��B
��B
�B
�UB
�UB
��B
�UB
��B
��B
��B
��B
��B
�[B
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
�-B
��B
��B
�MB
�gB
�gB
�gB
��B
��B
��B
��B
�9B
�9B
��B
��B
�%B
�tB
�tB
�tB
��B
��B
�EB
�zB
�zB
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�XB
�=B
�XB
�#B
�XB
�=B
�=B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104912  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173509  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173509                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023517  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023517  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                