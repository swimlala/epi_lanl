CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-12T00:35:13Z creation;2018-08-12T00:35:18Z conversion to V3.1;2019-12-19T07:35:19Z update;     
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
resolution        =���   axis      Z        T  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  s    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180812003513  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_269                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�x��: 1   @�x��[�@9�-V�d_���v1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffB��BffB'ffB/  B7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B�� B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_�3CaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD��DvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�>fD�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��fD�>fD�t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A˩�A˟�A˝�A˗�A˗�A˕�A˓uA˓uAˑhAˑhAˑhAˑhAˏ\AˑhA˓uAˉ7A�v�A�\)A�1A�G�AƧ�A�{A�&�A�I�A�O�A���A���A�1'A�l�A�z�A�XA�  A��FA���A�;dA�A���A��+A�I�A��^A���A�O�A�oA�ĜA�ZA���A�^5A�1A���A�;dA�  A��`A���A�+A��A���A��PA�G�A���A�hsA��FA�n�A��A�Q�A�JA��A���A��A�jA��#A��PA�p�A�bNA��A�oA���A��hA�ȴA�%A���A��\A��mA��A��RA�^5A�O�A��A~��A|�A{33Ax(�Avz�Au�Au�^Au�Ar��Apn�Am�mAl-Akt�Ai�^Ah��Ag�#AfE�Ae��Ae�AdZAa�Aax�Aa��Aa+A`�jA_��A^n�A]��A\��A[��A[+AZJAY�7AY7LAX�HAX(�AW"�AV^5AU��AT��AT�/ATbNAR�AR�AP�HAOƨAO/ANI�AN{AM�AMp�AM%AL��AL�AJbNAH�!AGAF�HAE�AD�AD�ACt�ABAA��AAp�A?��A>9XA=��A=/A<�9A<Q�A;�A;��A;�PA;VA:ZA9"�A7|�A5XA4jA3|�A2ffA1�^A1XA0�!A01'A/�hA.-A-&�A,��A,1'A*~�A)��A(��A(1A&�\A%�A$M�A#�;A#K�A"��A"z�A!�wA!/A ��A n�A��AbNA/A-A��AO�A�hAM�Ap�A��A�wAȴA9XA�AAp�AȴA��A&�A�A?}A�A|�A�A�A��A�+A��A&�A
bA�!A�A�
At�A�Ar�A�TAA�A��A ��@���@��R@�p�@���@�(�@��F@���@�9X@��@���@�@���@�K�@��@� �@�V@��/@�Z@�+@�@�V@� �@�K�@��H@�-@���@�?}@ܣ�@܋D@��
@ڧ�@�=q@��#@���@��
@�@���@��
@�$�@��/@�t�@�/@ˍP@�v�@��@��
@ŉ7@�bN@�"�@�-@�x�@���@�C�@���@�$�@���@�O�@���@�bN@�ƨ@���@��@���@��H@���@�&�@�I�@�+@��@�-@���@���@���@��m@�;d@��y@��!@��@���@�r�@���@�J@�G�@�/@�V@���@���@�+@�n�@��@�V@��@�V@�bN@��H@�-@���@��/@�I�@���@�l�@�+@�=q@��T@��^@�O�@���@�I�@��@��P@�@�^5@���@�/@��u@�1'@���@���@��@���@��@���@�~�@�E�@�7L@��9@�j@�A�@�1'@�  @�|�@�+@��R@�M�@���@���@��@��@�p�@�`B@�/@��@�A�@�  @�ƨ@��P@�l�@�l�@�l�@�K�@�o@�ȴ@�v�@�5?@��-@�r�@�  @��@�S�@�"�@���@���@�@�X@�Ĝ@�9X@��m@���@�C�@��R@�J@��@�7L@�V@���@�Q�@��@��@�|�@�K�@�o@��\@�=q@�=q@�E�@�E�@�E�@�=q@��@��T@�x�@��`@��j@���@�bN@�(�@�b@�b@�1@�  @�  @�1@�  @�@�@�  @�w@�P@\)@�@~��@~v�@~$�@}�h@}V@|��@|I�@|(�@|�@{�m@{��@{o@z�\@z�\@z�\@z-@zJ@y�#@y�#@y��@yX@x��@xA�@w\)@v{@u@u�-@u�@uO�@u/@uV@t�/@t�j@tz�@tZ@t9X@t1@sƨ@r��@q��@q%@p�`@p�u@pQ�@p �@o�;@o��@o�P@o+@nE�@n@mp�@l�@l�@l�D@lZ@lI�@l1@kt�@kdZ@kS�@k@j��@kC�@kdZ@kdZ@j^5@i��@i��@ix�@h��@h�`@hĜ@h�@hA�@g�@g\)@f�@f�+@f@e��@e��@e�@d��@d��@d�j@d�j@d��@d(�@c�
@b�!@b=q@a�#@a�^@a�^@ahs@aG�@aG�@`��@`Q�@`A�@` �@_l�@^�@^v�@]�T@\�@\�@\I�@\9X@\�@[�m@[��@Z��@Z~�@Z^5@Z^5@Z-@Y��@Y�@X�`@X�@Xr�@X �@W|�@W
=@V��@Vv�@V5?@U�@U�-@U�-@U?}@T�@Tj@S��@S�F@S@R~�@R=q@Q��@Q��@Q&�@P��@P�u@PQ�@P  @O��@O|�@O�@N�R@N$�@N$�@N$�@N{@M�@M/@L�@L��@L1@Kt�@K@J�!@J�\@Jn�@J^5@I�@Ix�@IG�@I%@H��@Hr�@H1'@H1'@Hb@G�@Gl�@G+@F��@FV@FV@F5?@E�T@EV@D��@D�j@D9X@C��@C�@Co@B��@Bn�@BJ@A��@A7L@@��@@Q�@@1'@@b@?�w@?l�@?;d@>�y@>��@>$�@>@=��@=/@<��@<��@<(�@;ƨ@;�@;C�@;o@:��@:�\@:~�@:^5@:-@:�@9��@9��@9hs@9G�@97L@9�@9�@8��@8Q�@81'@81'@8b@8  @8  @7�@7��@7�@7l�@7+@6�@6�R@6��@6E�@5@5`B@5O�@5�@5V@5�@5V@4��@4�D@4z�@4z�@4Z@4(�@3�m@3��@3t�@3dZ@3C�@2�\@2n�@2�@1��@1hs@1�@0Ĝ@0�@0A�@/�@/��@/\)@/
=@.�@.�R@.V@.$�@-��@-/@-V@,�j@,j@,Z@,I�@,9X@+�m@+�@+dZ@+o@*�!@*�\@*n�@*M�@)�@)�#@)�^@)��@)hs@)7L@)%@(��@(Q�@(b@(  @'�@'|�@'|�@'\)@'�@&�y@&ȴ@&��@&��@&�+@&v�@&E�@%�T@%�h@%`B@$�@$�j@$��@$z�@$j@$(�@#ƨ@#�F@#��@#�@#33@"�@"�H@"��@"n�@!��@!�@!��@!�7@!G�@ ��@ bN@ 1'@�@��@;d@�@ȴ@��@v�@V@$�@{@�@�T@�h@`B@/@V@��@�/@�@z�@z�@Z@I�@9X@�m@��@S�@"�@�@��@��@^5@=q@J@�@��@�^@��@�7@X@G�@�@�`@��@Ĝ@�u@bN@1'@�@��@�P@\)@;d@
=@�@�@�R@��@v�@E�@{@��@�h@p�@��@�@�/@��@I�@��@��@�m@�m@�m@�
@ƨ@�@dZ@S�@C�@@�\@M�@-@�@x�@7L@%@�`@Ĝ@��@��@�u@�@�@A�@b@�@�@\)@
=@��@E�@@�@?}@/@��@�/@�j@Z@(�@��@��@S�@@
�H@
�!@
n�@	��@	�^@	��@	��@	�7@	G�@	&�@��@Ĝ@�u@�u@�u@�u@�@r�@Q�@b@�@�w@��@|�@+@�y@�@ȴ@�R@��@��@v�@V@V@E�@{@�-@�h@�h@�h@�h@�h@�h@�h@�@p�@O�@?}@�@�@�/@�/@�@�D@j@9X@�@�m@�
@��@dZ@"�@@�@�@�H@�H@��@��@�!@n�@=q@-@�@J@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A˩�A˟�A˝�A˗�A˗�A˕�A˓uA˓uAˑhAˑhAˑhAˑhAˏ\AˑhA˓uAˉ7A�v�A�\)A�1A�G�AƧ�A�{A�&�A�I�A�O�A���A���A�1'A�l�A�z�A�XA�  A��FA���A�;dA�A���A��+A�I�A��^A���A�O�A�oA�ĜA�ZA���A�^5A�1A���A�;dA�  A��`A���A�+A��A���A��PA�G�A���A�hsA��FA�n�A��A�Q�A�JA��A���A��A�jA��#A��PA�p�A�bNA��A�oA���A��hA�ȴA�%A���A��\A��mA��A��RA�^5A�O�A��A~��A|�A{33Ax(�Avz�Au�Au�^Au�Ar��Apn�Am�mAl-Akt�Ai�^Ah��Ag�#AfE�Ae��Ae�AdZAa�Aax�Aa��Aa+A`�jA_��A^n�A]��A\��A[��A[+AZJAY�7AY7LAX�HAX(�AW"�AV^5AU��AT��AT�/ATbNAR�AR�AP�HAOƨAO/ANI�AN{AM�AMp�AM%AL��AL�AJbNAH�!AGAF�HAE�AD�AD�ACt�ABAA��AAp�A?��A>9XA=��A=/A<�9A<Q�A;�A;��A;�PA;VA:ZA9"�A7|�A5XA4jA3|�A2ffA1�^A1XA0�!A01'A/�hA.-A-&�A,��A,1'A*~�A)��A(��A(1A&�\A%�A$M�A#�;A#K�A"��A"z�A!�wA!/A ��A n�A��AbNA/A-A��AO�A�hAM�Ap�A��A�wAȴA9XA�AAp�AȴA��A&�A�A?}A�A|�A�A�A��A�+A��A&�A
bA�!A�A�
At�A�Ar�A�TAA�A��A ��@���@��R@�p�@���@�(�@��F@���@�9X@��@���@�@���@�K�@��@� �@�V@��/@�Z@�+@�@�V@� �@�K�@��H@�-@���@�?}@ܣ�@܋D@��
@ڧ�@�=q@��#@���@��
@�@���@��
@�$�@��/@�t�@�/@ˍP@�v�@��@��
@ŉ7@�bN@�"�@�-@�x�@���@�C�@���@�$�@���@�O�@���@�bN@�ƨ@���@��@���@��H@���@�&�@�I�@�+@��@�-@���@���@���@��m@�;d@��y@��!@��@���@�r�@���@�J@�G�@�/@�V@���@���@�+@�n�@��@�V@��@�V@�bN@��H@�-@���@��/@�I�@���@�l�@�+@�=q@��T@��^@�O�@���@�I�@��@��P@�@�^5@���@�/@��u@�1'@���@���@��@���@��@���@�~�@�E�@�7L@��9@�j@�A�@�1'@�  @�|�@�+@��R@�M�@���@���@��@��@�p�@�`B@�/@��@�A�@�  @�ƨ@��P@�l�@�l�@�l�@�K�@�o@�ȴ@�v�@�5?@��-@�r�@�  @��@�S�@�"�@���@���@�@�X@�Ĝ@�9X@��m@���@�C�@��R@�J@��@�7L@�V@���@�Q�@��@��@�|�@�K�@�o@��\@�=q@�=q@�E�@�E�@�E�@�=q@��@��T@�x�@��`@��j@���@�bN@�(�@�b@�b@�1@�  @�  @�1@�  @�@�@�  @�w@�P@\)@�@~��@~v�@~$�@}�h@}V@|��@|I�@|(�@|�@{�m@{��@{o@z�\@z�\@z�\@z-@zJ@y�#@y�#@y��@yX@x��@xA�@w\)@v{@u@u�-@u�@uO�@u/@uV@t�/@t�j@tz�@tZ@t9X@t1@sƨ@r��@q��@q%@p�`@p�u@pQ�@p �@o�;@o��@o�P@o+@nE�@n@mp�@l�@l�@l�D@lZ@lI�@l1@kt�@kdZ@kS�@k@j��@kC�@kdZ@kdZ@j^5@i��@i��@ix�@h��@h�`@hĜ@h�@hA�@g�@g\)@f�@f�+@f@e��@e��@e�@d��@d��@d�j@d�j@d��@d(�@c�
@b�!@b=q@a�#@a�^@a�^@ahs@aG�@aG�@`��@`Q�@`A�@` �@_l�@^�@^v�@]�T@\�@\�@\I�@\9X@\�@[�m@[��@Z��@Z~�@Z^5@Z^5@Z-@Y��@Y�@X�`@X�@Xr�@X �@W|�@W
=@V��@Vv�@V5?@U�@U�-@U�-@U?}@T�@Tj@S��@S�F@S@R~�@R=q@Q��@Q��@Q&�@P��@P�u@PQ�@P  @O��@O|�@O�@N�R@N$�@N$�@N$�@N{@M�@M/@L�@L��@L1@Kt�@K@J�!@J�\@Jn�@J^5@I�@Ix�@IG�@I%@H��@Hr�@H1'@H1'@Hb@G�@Gl�@G+@F��@FV@FV@F5?@E�T@EV@D��@D�j@D9X@C��@C�@Co@B��@Bn�@BJ@A��@A7L@@��@@Q�@@1'@@b@?�w@?l�@?;d@>�y@>��@>$�@>@=��@=/@<��@<��@<(�@;ƨ@;�@;C�@;o@:��@:�\@:~�@:^5@:-@:�@9��@9��@9hs@9G�@97L@9�@9�@8��@8Q�@81'@81'@8b@8  @8  @7�@7��@7�@7l�@7+@6�@6�R@6��@6E�@5@5`B@5O�@5�@5V@5�@5V@4��@4�D@4z�@4z�@4Z@4(�@3�m@3��@3t�@3dZ@3C�@2�\@2n�@2�@1��@1hs@1�@0Ĝ@0�@0A�@/�@/��@/\)@/
=@.�@.�R@.V@.$�@-��@-/@-V@,�j@,j@,Z@,I�@,9X@+�m@+�@+dZ@+o@*�!@*�\@*n�@*M�@)�@)�#@)�^@)��@)hs@)7L@)%@(��@(Q�@(b@(  @'�@'|�@'|�@'\)@'�@&�y@&ȴ@&��@&��@&�+@&v�@&E�@%�T@%�h@%`B@$�@$�j@$��@$z�@$j@$(�@#ƨ@#�F@#��@#�@#33@"�@"�H@"��@"n�@!��@!�@!��@!�7@!G�@ ��@ bN@ 1'@�@��@;d@�@ȴ@��@v�@V@$�@{@�@�T@�h@`B@/@V@��@�/@�@z�@z�@Z@I�@9X@�m@��@S�@"�@�@��@��@^5@=q@J@�@��@�^@��@�7@X@G�@�@�`@��@Ĝ@�u@bN@1'@�@��@�P@\)@;d@
=@�@�@�R@��@v�@E�@{@��@�h@p�@��@�@�/@��@I�@��@��@�m@�m@�m@�
@ƨ@�@dZ@S�@C�@@�\@M�@-@�@x�@7L@%@�`@Ĝ@��@��@�u@�@�@A�@b@�@�@\)@
=@��@E�@@�@?}@/@��@�/@�j@Z@(�@��@��@S�@@
�H@
�!@
n�@	��@	�^@	��@	��@	�7@	G�@	&�@��@Ĝ@�u@�u@�u@�u@�@r�@Q�@b@�@�w@��@|�@+@�y@�@ȴ@�R@��@��@v�@V@V@E�@{@�-@�h@�h@�h@�h@�h@�h@�h@�@p�@O�@?}@�@�@�/@�/@�@�D@j@9X@�@�m@�
@��@dZ@"�@@�@�@�H@�H@��@��@�!@n�@=q@-@�@J@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   By�B{�Bz�B|�B|�B|�B}�B}�B}�B}�B}�B}�B}�B}�B{�By�Bx�Bt�BdZB;dBVB��B%BcTB"�Bl�B� B�Bo�B`BBH�B	7BN�BXB5?B2-B,B)�B8RBC�B>wB:^B5?B+B)�B�B!�B�B
=B�mBǮB��B��B��BB��B�uBy�B�DBx�BXB:^BL�BH�B:^B)�B)�B!�B�B�B�B�B1B
�B
�B
�
B
��B
��B
��B
��B
��B
�+B
u�B
�%B
�+B
z�B
gmB
L�B
@�B
%�B
(�B
1'B
0!B
 �B	��B	�yB	�;B	�fB	�NB	��B	�B	ǮB	�^B	�qB	�wB	�B	�{B	��B	�-B	��B	�B	��B	�uB	�bB	�DB	�B	�B	~�B	�%B	�%B	�B	w�B	m�B	m�B	l�B	ffB	iyB	`BB	L�B	L�B	H�B	?}B	F�B	A�B	G�B	F�B	@�B	>wB	;dB	2-B	�B	VB	oB	{B	hB	hB	hB	PB	B	%B	
=B��B�B��B��B��B��B��B��B��B�B�ZB��BŢB�9BB��B�wBÖBƨB�}B�dB�?B��B��B�B��B�hB��B��B�oB�B�B�DB�bB�PB�VB�1B�B�B�B}�Bo�BiyBdZBe`BjBdZBO�BN�BYBVBR�BN�BYB[#BZBR�BK�BB�BG�BH�B2-B49BF�BG�BJ�BG�BA�B7LB,B(�B�B�B-B49B.B)�B'�B�B#�B�B�BuB,B#�B&�B&�B#�B�B�B �B&�B �B�B\BVBbB\B\B{B1B��B%B{B�B�B\B%B	7B�B#�B�B�B"�B$�B�B �B{B!�B�B�B�B�BbB�B�B �B�BbB"�B!�B'�B'�B(�B"�B/B1'B2-B33B0!B/B,B!�B.B33B33B5?B6FB8RB8RBB�B@�BB�BC�BH�BC�BF�BL�BK�BG�BG�B@�BG�BD�BP�B\)B[#BW
BVBVBZB^5BffBo�Bn�BiyBffBn�Br�Bp�Bu�By�B|�B~�B{�B�B�1B�%B�%B�+B�=B�DB�DB�JB�\B��B��B��B��B��B��B��B�B��B�B�B��B�B�?B�LB�RB�LB�?B�XB�XB�^B�qBBŢBǮBƨBŢBĜBBŢBɺB��B��B��B��B��B��B��B��B��B��B��B��B�#B�5B�BB�NB�BB�NB�B�yB�B�B��B��B��B��B��B	B	DB	VB	VB	hB	{B	�B	�B	�B	�B	�B	$�B	.B	/B	/B	/B	/B	.B	/B	0!B	0!B	:^B	:^B	<jB	?}B	D�B	F�B	G�B	G�B	H�B	I�B	J�B	J�B	K�B	K�B	L�B	M�B	N�B	N�B	N�B	P�B	P�B	P�B	R�B	T�B	W
B	YB	[#B	[#B	[#B	_;B	hsB	m�B	n�B	n�B	q�B	r�B	s�B	t�B	r�B	r�B	r�B	r�B	s�B	y�B	|�B	{�B	|�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�B	� B	�%B	�7B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�'B	�B	�'B	�?B	�?B	�?B	�XB	�XB	�^B	�jB	�jB	�wB	��B	B	ÖB	ĜB	ŢB	ĜB	ÖB	ƨB	ȴB	ȴB	ǮB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	�
B	�B	�B	�B	�;B	�BB	�NB	�NB	�NB	�HB	�BB	�ZB	�`B	�fB	�`B	�ZB	�`B	�sB	�sB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B
B
B
  B
B
B
%B
+B
1B
1B
%B
+B
	7B
	7B
	7B
DB
DB
PB
JB
DB
JB
JB
JB
VB
\B
\B
VB
JB
hB
hB
bB
bB
uB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
#�B
$�B
&�B
'�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
'�B
&�B
(�B
,B
+B
,B
-B
,B
+B
+B
-B
-B
,B
,B
,B
-B
-B
.B
-B
+B
.B
-B
.B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
2-B
33B
33B
33B
6FB
5?B
5?B
7LB
7LB
7LB
6FB
6FB
7LB
7LB
7LB
9XB
9XB
9XB
8RB
;dB
;dB
:^B
:^B
:^B
:^B
;dB
:^B
;dB
=qB
<jB
=qB
>wB
>wB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
=qB
>wB
@�B
?}B
A�B
A�B
A�B
B�B
A�B
A�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
C�B
C�B
F�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
H�B
K�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
L�B
M�B
M�B
N�B
N�B
N�B
M�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
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
W
B
VB
XB
YB
XB
W
B
XB
ZB
ZB
ZB
ZB
ZB
ZB
YB
ZB
ZB
ZB
YB
XB
ZB
[#B
[#B
ZB
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
aHB
bNB
bNB
bNB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
ffB
gmB
hsB
gmB
ffB
gmB
gmB
hsB
hsB
jB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
jB
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
l�B
l�B
k�B
jB
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
m�B
m�B
n�B
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
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   By�B|Bz�B|�B|�B|�B~B}�B}�B~B~B}�B}�B}�B|Bz*By�Bv�Bh�BB�ByB{BHBf�B*�Bo�B��B��BrGBc�BMBBP.BY�B88B5B/ B,�B9XBC�B?.B;B6B,B*�B;B"�B�B0B��B�JB�
B��BбBĜB��B��B}<B�Bz�B[WB=�BM�BI�B<B,"B+�B# B�BdBB�B	�B
��B
�'B
ٚB
��B
ÖB
�B
�#B
�eB
��B
x�B
��B
��B
|B
iB
OvB
CB
)_B
*�B
1�B
0�B
"4B
'B	�B	�NB	�XB	�B	�,B	�$B	�RB	�B	�BB	�.B	��B	��B	�sB	�B	��B	��B	�'B	�B	�hB	�JB	��B	�MB	�OB	��B	��B	��B	x�B	n�B	n�B	mwB	gmB	i�B	aHB	N�B	N"B	J=B	AB	GzB	B�B	HB	F�B	AUB	?.B	<B	33B	eB	bB	�B	�B	�B	�B	�B	�B	�B	�B	B�B�TB��B�xB�zB�lB�lB�0B�fB�}B�B��B��B��B��B��B��BāB�zB�iB�6B�`B��B�>B��B��B��B��B�$B��B�MB��B�dB�B�"B�B�B�-B��B��B~�Bq'Bk6Be�Bf�BkQBezBR:BP�BZQBW?BTaBPHBY�B[�BZ�BS�BMBC�BH�BI�B4�B5�BGEBHKBKBHBB'B8lB-�B*�B�B�B-�B4�B/ B*�B(�B�B$�B �B1B�B,=B$�B'�B'�B$�B�B�B!�B'mB!|B5B B�B�B�BHBMB	�B�*B�B2B$BBbB�B
�B#B$&B�B�B#TB%`B�B!�B�B"�B�B�B�B�B�B�B �B!�BB B#�B"�B(�B(�B)�B#�B/�B1�B2�B3�B0�B/�B,�B#nB/ B4B4B6B6�B9	B9>BB�BA;BCBD3BIBDgBG+BM6BLBHfBHKBA�BH�BE�BQ�B\CB[qBW�BV�BV�BZ�B^�Bf�Bo�Bo Bj0Bg�Bo5Bs3Bq[Bv`Bz^B}VBcB|�B��B��B��B��B��B��B��B��B��B�B��B�B�!B�4B�&B�ZB�0B�QB�KB�WB�kB��B��B�tB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�B�	B�B�B��B� B�B�.B�BB�HB�.B�NB�vB͹BیBޞB��B�B��B�B��B��B�)B�'B�+B�>B�DB�B��B	�B	xB	�B	�B	�B	�B	�B	�B	�B	B	B	%,B	./B	/5B	/5B	/OB	/5B	.cB	/�B	0�B	0�B	:�B	:�B	<�B	?�B	D�B	F�B	G�B	G�B	H�B	I�B	J�B	J�B	K�B	K�B	MB	NB	N�B	OB	O(B	QB	Q4B	Q4B	S@B	UMB	W?B	YKB	[WB	[WB	[qB	_�B	h�B	m�B	n�B	n�B	q�B	r�B	s�B	t�B	r�B	r�B	sB	s3B	t9B	zB	}B	|B	}"B	~B	.B	.B	�B	�;B	�AB	�AB	�AB	�;B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�,B	�0B	�0B	�KB	�=B	��B	�3B	�[B	��B	�vB	�tB	�tB	��B	�rB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�B	�&B	�B	�&B	�B	�2B	�,B	�MB	�EB	�$B	�MB	�YB	�QB	چB	چB	�pB	�vB	�B	�B	�B	�B	�B	�B	�B	�fB	�zB	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�6B	�(B	�BB
'B
 B
 B
 4B	�]B
;B
;B
 iB
aB
gB
YB
EB
fB
fB
tB
zB
	lB
	lB
	�B
^B
xB
PB
~B
�B
dB
~B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$B
$B
%B
%�B
%�B
&B
%�B
$&B
%B
'B
'�B
($B
(
B
(
B
(
B
'B
'B
'B
'B
($B
)B
)B
(>B
'8B
)*B
,"B
+6B
,B
-B
,=B
+B
+6B
-)B
-B
,"B
,=B
,"B
-CB
-CB
./B
-CB
+QB
.IB
-]B
.IB
/OB
/OB
0UB
1[B
1AB
1[B
1[B
1vB
2GB
3hB
3MB
2|B
3hB
3�B
3hB
6FB
5tB
5ZB
7fB
7�B
7�B
6zB
6zB
7�B
7�B
7�B
9rB
9rB
9�B
8�B
;B
;�B
:xB
:xB
:xB
:xB
;�B
:�B
;B
=�B
<�B
=�B
>�B
>�B
=�B
>�B
?�B
?�B
?�B
?}B
?�B
>�B
=�B
>�B
@�B
?�B
A�B
A�B
A�B
B�B
A�B
A�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
C�B
C�B
F�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
H�B
K�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
MB
NB
NB
N�B
OB
N�B
M�B
MB
M�B
NB
N�B
PB
PB
PB
PB
QB
QB
R B
R B
RB
RB
R B
RB
SB
R B
SB
S�B
S&B
SB
SB
SB
SB
T,B
T,B
T,B
UB
U2B
U2B
VB
VB
VB
V9B
V9B
V9B
V9B
V9B
W?B
VSB
X+B
Y1B
XEB
WYB
XEB
Z7B
Z7B
Z7B
ZB
Z7B
ZQB
Y1B
Z7B
Z7B
ZQB
Y1B
XEB
ZQB
[=B
[WB
ZQB
\]B
]IB
]IB
^jB
^jB
^OB
^OB
^5B
^OB
^jB
^jB
^jB
]dB
]dB
^jB
^jB
_pB
_VB
_�B
abB
bhB
bhB
bhB
b�B
a�B
b�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
f�B
g�B
h�B
g�B
f�B
g�B
g�B
h�B
h�B
j�B
i�B
i�B
i�B
iyB
h�B
h�B
i�B
i�B
j�B
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
l�B
l�B
k�B
j�B
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
m�B
m�B
n�B
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
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808160034482018081600344820180816003448201808160200152018081602001520180816020015201808170023552018081700235520180817002355  JA  ARFMdecpA19c                                                                20180812093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180812003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180812003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180812003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180812003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180812003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180812003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180812003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180812003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180812003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20180812005631                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180812153345  CV  JULD            G�O�G�O�F�ǰ                JM  ARGQJMQC2.0                                                                 20180812153345  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20180812153345  CV  LONGITUDE       G�O�G�O��"�q                JM  ARCAJMQC2.0                                                                 20180815153448  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180815153448  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180815170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180816152355  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                