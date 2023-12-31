CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-22T18:41:45Z creation;2023-01-22T18:41:46Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230122184145  20230122185824  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�(��G1   @�)>F�@;lI�^5?�d?|�h1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   AffA>ffA`  A�  A�  A�  A���A�  A�  A�  A���B   B  B  B  B   B(ffB0  B8  B@  BHffBPffBX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C�C  C  C  C�C  C   C"  C$  C&  C(�C*�C,  C.  C/�fC1�fC3�fC6  C8  C:  C<  C=�fC@  CB  CD  CF�CG�fCJ  CL�CN�CO�fCQ�fCT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��C�  C�  C�  C��3C��3C�  C�  C�  C��C��C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��C��C��C��C�  C��3C�  C�  C��3C��3C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3D y�D  D� D  D� D  D� D  D�fD  D� D��D� D  D� D  D� D��D	y�D
  D
y�D
��D� D  D� D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� DbfDb�fDc  Dc� Dd  Dd�fDe  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{y�D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�C3D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƃ3D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�<�Dɀ D��3D�3D�@ D�|�Dʼ�D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D���D�<�D̀ D�� D�  D�@ D΀ Dμ�D�  D�<�Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ D�|�D�� D�  D�@ Dփ3D�� D�  D�@ D׀ D�� D���D�<�D؀ D��3D�  D�@ D�|�Dټ�D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�C3D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D��3D�3D�C3D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@vff@�33@�33A  A<  A]��A}��A���A���A���A���A���A���AA���BffBffBffBffB'��B/ffB7ffB?ffBG��BO��BWffB_ffBgffBo  BwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��fB��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	� CٚCٚCٚCٚC�4CٚCٚCٚC�4CٚCٚC!ٚC#ٚC%ٚC'�4C)�4C+ٚC-ٚC/� C1� C3� C5ٚC7ٚC9ٚC;ٚC=� C?ٚCAٚCCٚCE�4CG� CIٚCK�4CM�4CO� CQ� CSٚCUٚCWٚCY� C[ٚC]ٚC_ٚCaٚCcٚCe� CgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C�� C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C�� C�� D p D �fDvfD�fDvfD�fDvfD�fD|�D�fDvfD� DvfD�fDvfD�fDvfD� D	p D	�fD
p D
� DvfD�fDvfD�fDvfD�fDvfD�fDp D�fDvfD�fD|�D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD ��D!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2� D3vfD3�fD4vfD4�fD5vfD5��D6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?p D?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ��DKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOp DO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW� DXvfDX�fDY|�DY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_|�D_�fD`vfD`�fDavfDa��Db|�Db�fDcvfDc�fDd|�Dd�fDevfDe� DfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDk|�Dk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrp Dr�fDsvfDs�fDt|�Dt��DuvfDu�fDvvfDv�fDwvfDw�fDxvfDx� DyvfDy�fDzvfDz�fD{p D{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�>fD�~fD��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��fD�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D�� D�� D�;3D�{3D��3D�� D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D�� D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�x D��3D��3D�;3D�{3D��3D��fD�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�x D�� D��3D�>fD�{3D��3D��3D�;3D�{3D��3D�� D�;3D�{3D��fD��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D�� D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D�� D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�~fDƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȾfD��3D�8 D�{3DɾfD��fD�;3D�x Dʸ D��3D�;3D�{3D˻3D��3D�;3D�~fD̻3D�� D�8 D�{3Dͻ3D��3D�;3D�{3Dθ D��3D�8 D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�x Dջ3D��3D�;3D�~fDֻ3D��3D�;3D�{3D׻3D�� D�8 D�{3DؾfD��3D�;3D�x Dٸ D��3D�>fD�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�>fD�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��fD�;3D�x D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�fD��fD�;3D�{3D�3D��3D�;3D�{3D�fD��fD�>fD�~fD��fD��3D�;3D�{3D�3D��3D�;3D�{3D�3D�� D�8 D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�x D�3D��fD�>fD�{3D�3D��3D�;3D�{3D��3D��3D�;3D�x D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� iA��A��A��A�@A��A�%zA�.}A�1�A�3hA�6zA�4nA�6�A�5�A�49A�#nA�&�A�רA�u�A��
A��XA�f�A�.IA��.A�}VA�k�A�A�A��A���A�zDA�>�A�&A�#�A�CA��A��BA���A�7�A�PA�GEA���A�bA�2�A��]A���A��A�@OA��A�F?A�VA�GA��0A��JA��CA�2�A��A��EA��qA�z�A�j�A�M6A�� A�˒A���A�y	A��A�q�A�.IA��)A���A�C�A�&A�H�A��A�~�A��*A��A�-�A��[A��A��A��aA��FA�
	A��nA�ޞA�t�A��A�c�A���A�Q�A���A��rA� �A��!A��A�J�A(A}�oA}�	A|xA{:*Ax�]Av=qAt
�Ar6zAqN�Ao��Am-Ak�Aj_Ahs�Ag��Aga�Af��Ae� Ae�AdzxAd	lAcߤAc҉Ac��AcDgAb��AbIRAaݘA`˒A_��A_�[A_A^�A]U�A\�PA[�/AZ�AY�AX��AWMjAUGAS�+AR�cAR!-AN�	AMJ#AL�YAJl�AIb�AI7AH�AH�AH�AHT�AF�ZAF4�AE�}AD�.AD*0AC��AB)�A@�}A?�A?Z�A>�WA>��A>!-A=�A=p�A=+A<��A;�4A:��A8e,A6�mA5�`A5U�A4�$A3�A2�^A1�9A1OA0�hA/��A/g8A.��A.+A,n�A*e,A'7�A%�/A%��A%u%A$�fA#=�A"�hA"H�A!�sA!^�A �vA |�A�AݘA�+A�}AA�A(�AJ�A�HA2�A�
A�AA�A�AϫA��AxlA��A��A33A��Ah�A<6AںA�-A��A}VAYKA!A
�dA
�~A
O�A	l�A��A�A�IA��A��A�A��A�A��Aw�A��A ƨA [�@�� @�V@���@�A�@�u@�1@�_�@��@��U@�4@��@�@�_@�S@��	@�;�@��a@���@�A@��&@�C�@��/@�E�@��@�+�@�F�@���@ڃ�@�s�@��)@�c @�4@���@���@׷�@ש�@ׄM@�\�@�8@��@���@��@�GE@�&�@�l�@��?@�@��@�a@���@�?@��r@�{J@�q@��@�Q�@�0�@�/@�e,@�ԕ@��z@��@�7@�B[@��k@���@�bN@�N�@�	@�\�@�{J@�5?@��@��3@�o�@�9�@��P@���@�hs@�-w@�!@��T@��	@��@��e@�_@��K@��@�j�@�,�@�	l@��@��@��@���@�|�@��@���@��Y@� �@��@���@�J#@��Z@�7�@��9@���@�T�@�-w@�(�@�(@��@��K@�� @��.@��@���@�}V@�\�@�V@�E�@�6�@�1'@�-@�(�@�!�@�{@��@�o�@�ی@�B[@��{@��@��'@��@�a�@�0�@��2@��,@���@��r@��g@��	@�Z�@�A�@�(�@�ں@�e�@�@��@�S�@���@���@�@�@��@��@���@��
@���@���@�-w@��"@��@��@���@��@���@���@�}�@�RT@��@��K@��U@��L@���@���@�a|@�@���@��@�4@�o @� i@��@�D�@�U�@�֡@���@�\�@���@��@�l�@�P�@���@�͟@��@�u�@�e�@�^5@�W�@�+k@�b@�ݘ@��n@�F�@���@�
�@���@��K@���@�x@��@�oi@�1@��j@���@���@���@�k�@�?}@�	l@�i�@��;@��[@���@��	@�zx@�s�@�s�@�m]@�Mj@��j@�n�@�K^@�[�@�&�@�;@@~��@~0U@|$@{�:@{@z�@x��@x$@wخ@w�V@wC�@v
�@uJ�@t�@tu�@t�@sb�@sJ#@sK�@sX�@sX�@sF�@s>�@rߤ@r!�@q�@q�T@q7L@p�@p��@p�Y@pV�@oخ@o��@o��@o�f@n�x@mS&@l�@l��@lu�@k�f@j;�@i�h@h�I@g�@g�[@g��@f�y@e�d@d�f@d�)@d��@d�@d��@c�@c��@b�@b0U@a��@aJ�@`��@`[�@_�V@_�4@_o�@_e�@_b�@_\)@_U�@_P�@_C�@_$t@^��@^�m@^�\@^M�@^($@^ �@]�@]�@]��@]��@]�"@]\�@]Q�@]L�@]a�@]Dg@]+�@\�U@\l"@[��@Z��@Z�B@Z͟@Z�b@Z��@Z��@Z@Y�@Y��@Y?}@X�|@X�|@X�K@X�|@X�|@X�	@Y%@X�5@X4n@W�@V�R@V��@V�+@V_�@VC�@V($@V�@U�#@U�^@US&@U�@T�5@T�O@T��@T?�@T$@S�W@S˒@S$t@Q��@P��@O�A@O�q@O>�@N�c@Nu%@M�@M�H@M��@M�t@M�t@M��@M�C@M��@M��@Mu�@MO�@M=�@M5�@M�@MV@L��@L��@LS�@LS�@L7�@L�@K��@K��@KdZ@K6z@Ko@K i@J��@J��@J��@JC�@J_@I��@Ix�@I&�@H�@HN�@HD�@HN�@H�@G�{@GW?@F��@E�.@DXy@C�r@C��@C��@C�{@C@O@C$t@C�@B�"@B�@B��@B�X@B�'@B��@B�}@B��@B��@Bq�@BOv@B.�@B�@A��@A+@@/�@?�0@?��@?j�@?Z�@?,�@>��@>�@>�@> �@=�@=��@=�=@=rG@=J�@<�|@<�@;��@;�{@;@:�6@9�z@8��@8y>@8h�@8U2@8PH@8I�@84n@8 �@7��@7��@7s@7@6ں@6��@6��@61�@6�@5�@4��@4�I@4]d@3��@3x@3X�@3H�@3A�@36z@3�@2�H@2��@2�@1�C@1`B@0�@0]d@/�@/��@/�g@/�a@/�*@/�@/;d@.�@.�s@.�@.ں@.�]@.�H@.��@.�<@.�@.�L@.~�@.\�@.R�@.@-��@-o @-+@,�I@+˒@*�X@*c @)��@)5�@);@(�@(ѷ@(�)@(�)@(��@(��@(��@(�@(�@(��@(�@(g8@(N�@(7�@'�@']�@&�H@&3�@%�o@%?}@$�9@$~(@$K^@$M@$@#�Q@#x@#A�@#.I@#33@#�@#�@"�8@"ں@"҉@"��@"^5@"GE@!�@![W@ �@ z�@ 9X@�@�0@��@_p@�@�c@�6@:*@u@�.@�.@��@�@�M@Vm@��@bN@�@�a@l�@�@�'@��@Ov@u@��@m]@Dg@�@��@��@��@�	@��@�`@�`@�z@��@Z@I�@A�@�@�
@�f@�@��@i�@Ta@.�@�@	@�@�^@�C@��@��@��@��@��@c@c@rG@A @�@��@��@�@|�@C-@1'@(�@�@�
@��@��@�f@O@�@�B@҉@��@�s@�,@�L@{�@d�@?@{@��@f�@A @�E@��@h�@M@H@��@��@�K@��@�k@�:@�4@j�@j�@dZ@Z�@Mj@,�@��@��@0U@ �@��@�X@u�@Y�@B�@4@�@~(@A�@"h@�]@�A@��@��@�K@�@��@��@��@�@iD@_p@O@�@
ߤ@
��@
a|@
Ov@
E�@
($@
�@	ϫ@	��@	ϫ@	�H@	�n@	�X@	��@	��@	��@	�C@	��@	�@	��@	��@	Vm@	 \@		l@�f@��@ѷ@��@��@��@��@y>@tT@|�@��@��@u�@g8@9X@�@�@�@��@U�@!-@�"@�@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� iA��A��A��A�@A��A�%zA�.}A�1�A�3hA�6zA�4nA�6�A�5�A�49A�#nA�&�A�רA�u�A��
A��XA�f�A�.IA��.A�}VA�k�A�A�A��A���A�zDA�>�A�&A�#�A�CA��A��BA���A�7�A�PA�GEA���A�bA�2�A��]A���A��A�@OA��A�F?A�VA�GA��0A��JA��CA�2�A��A��EA��qA�z�A�j�A�M6A�� A�˒A���A�y	A��A�q�A�.IA��)A���A�C�A�&A�H�A��A�~�A��*A��A�-�A��[A��A��A��aA��FA�
	A��nA�ޞA�t�A��A�c�A���A�Q�A���A��rA� �A��!A��A�J�A(A}�oA}�	A|xA{:*Ax�]Av=qAt
�Ar6zAqN�Ao��Am-Ak�Aj_Ahs�Ag��Aga�Af��Ae� Ae�AdzxAd	lAcߤAc҉Ac��AcDgAb��AbIRAaݘA`˒A_��A_�[A_A^�A]U�A\�PA[�/AZ�AY�AX��AWMjAUGAS�+AR�cAR!-AN�	AMJ#AL�YAJl�AIb�AI7AH�AH�AH�AHT�AF�ZAF4�AE�}AD�.AD*0AC��AB)�A@�}A?�A?Z�A>�WA>��A>!-A=�A=p�A=+A<��A;�4A:��A8e,A6�mA5�`A5U�A4�$A3�A2�^A1�9A1OA0�hA/��A/g8A.��A.+A,n�A*e,A'7�A%�/A%��A%u%A$�fA#=�A"�hA"H�A!�sA!^�A �vA |�A�AݘA�+A�}AA�A(�AJ�A�HA2�A�
A�AA�A�AϫA��AxlA��A��A33A��Ah�A<6AںA�-A��A}VAYKA!A
�dA
�~A
O�A	l�A��A�A�IA��A��A�A��A�A��Aw�A��A ƨA [�@�� @�V@���@�A�@�u@�1@�_�@��@��U@�4@��@�@�_@�S@��	@�;�@��a@���@�A@��&@�C�@��/@�E�@��@�+�@�F�@���@ڃ�@�s�@��)@�c @�4@���@���@׷�@ש�@ׄM@�\�@�8@��@���@��@�GE@�&�@�l�@��?@�@��@�a@���@�?@��r@�{J@�q@��@�Q�@�0�@�/@�e,@�ԕ@��z@��@�7@�B[@��k@���@�bN@�N�@�	@�\�@�{J@�5?@��@��3@�o�@�9�@��P@���@�hs@�-w@�!@��T@��	@��@��e@�_@��K@��@�j�@�,�@�	l@��@��@��@���@�|�@��@���@��Y@� �@��@���@�J#@��Z@�7�@��9@���@�T�@�-w@�(�@�(@��@��K@�� @��.@��@���@�}V@�\�@�V@�E�@�6�@�1'@�-@�(�@�!�@�{@��@�o�@�ی@�B[@��{@��@��'@��@�a�@�0�@��2@��,@���@��r@��g@��	@�Z�@�A�@�(�@�ں@�e�@�@��@�S�@���@���@�@�@��@��@���@��
@���@���@�-w@��"@��@��@���@��@���@���@�}�@�RT@��@��K@��U@��L@���@���@�a|@�@���@��@�4@�o @� i@��@�D�@�U�@�֡@���@�\�@���@��@�l�@�P�@���@�͟@��@�u�@�e�@�^5@�W�@�+k@�b@�ݘ@��n@�F�@���@�
�@���@��K@���@�x@��@�oi@�1@��j@���@���@���@�k�@�?}@�	l@�i�@��;@��[@���@��	@�zx@�s�@�s�@�m]@�Mj@��j@�n�@�K^@�[�@�&�@�;@@~��@~0U@|$@{�:@{@z�@x��@x$@wخ@w�V@wC�@v
�@uJ�@t�@tu�@t�@sb�@sJ#@sK�@sX�@sX�@sF�@s>�@rߤ@r!�@q�@q�T@q7L@p�@p��@p�Y@pV�@oخ@o��@o��@o�f@n�x@mS&@l�@l��@lu�@k�f@j;�@i�h@h�I@g�@g�[@g��@f�y@e�d@d�f@d�)@d��@d�@d��@c�@c��@b�@b0U@a��@aJ�@`��@`[�@_�V@_�4@_o�@_e�@_b�@_\)@_U�@_P�@_C�@_$t@^��@^�m@^�\@^M�@^($@^ �@]�@]�@]��@]��@]�"@]\�@]Q�@]L�@]a�@]Dg@]+�@\�U@\l"@[��@Z��@Z�B@Z͟@Z�b@Z��@Z��@Z@Y�@Y��@Y?}@X�|@X�|@X�K@X�|@X�|@X�	@Y%@X�5@X4n@W�@V�R@V��@V�+@V_�@VC�@V($@V�@U�#@U�^@US&@U�@T�5@T�O@T��@T?�@T$@S�W@S˒@S$t@Q��@P��@O�A@O�q@O>�@N�c@Nu%@M�@M�H@M��@M�t@M�t@M��@M�C@M��@M��@Mu�@MO�@M=�@M5�@M�@MV@L��@L��@LS�@LS�@L7�@L�@K��@K��@KdZ@K6z@Ko@K i@J��@J��@J��@JC�@J_@I��@Ix�@I&�@H�@HN�@HD�@HN�@H�@G�{@GW?@F��@E�.@DXy@C�r@C��@C��@C�{@C@O@C$t@C�@B�"@B�@B��@B�X@B�'@B��@B�}@B��@B��@Bq�@BOv@B.�@B�@A��@A+@@/�@?�0@?��@?j�@?Z�@?,�@>��@>�@>�@> �@=�@=��@=�=@=rG@=J�@<�|@<�@;��@;�{@;@:�6@9�z@8��@8y>@8h�@8U2@8PH@8I�@84n@8 �@7��@7��@7s@7@6ں@6��@6��@61�@6�@5�@4��@4�I@4]d@3��@3x@3X�@3H�@3A�@36z@3�@2�H@2��@2�@1�C@1`B@0�@0]d@/�@/��@/�g@/�a@/�*@/�@/;d@.�@.�s@.�@.ں@.�]@.�H@.��@.�<@.�@.�L@.~�@.\�@.R�@.@-��@-o @-+@,�I@+˒@*�X@*c @)��@)5�@);@(�@(ѷ@(�)@(�)@(��@(��@(��@(�@(�@(��@(�@(g8@(N�@(7�@'�@']�@&�H@&3�@%�o@%?}@$�9@$~(@$K^@$M@$@#�Q@#x@#A�@#.I@#33@#�@#�@"�8@"ں@"҉@"��@"^5@"GE@!�@![W@ �@ z�@ 9X@�@�0@��@_p@�@�c@�6@:*@u@�.@�.@��@�@�M@Vm@��@bN@�@�a@l�@�@�'@��@Ov@u@��@m]@Dg@�@��@��@��@�	@��@�`@�`@�z@��@Z@I�@A�@�@�
@�f@�@��@i�@Ta@.�@�@	@�@�^@�C@��@��@��@��@��@c@c@rG@A @�@��@��@�@|�@C-@1'@(�@�@�
@��@��@�f@O@�@�B@҉@��@�s@�,@�L@{�@d�@?@{@��@f�@A @�E@��@h�@M@H@��@��@�K@��@�k@�:@�4@j�@j�@dZ@Z�@Mj@,�@��@��@0U@ �@��@�X@u�@Y�@B�@4@�@~(@A�@"h@�]@�A@��@��@�K@�@��@��@��@�@iD@_p@O@�@
ߤ@
��@
a|@
Ov@
E�@
($@
�@	ϫ@	��@	ϫ@	�H@	�n@	�X@	��@	��@	��@	�C@	��@	�@	��@	��@	Vm@	 \@		l@�f@��@ѷ@��@��@��@��@y>@tT@|�@��@��@u�@g8@9X@�@�@�@��@U�@!-@�"@�@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB�B%BMB�BBBgB�B�B�BBSBB�BuB�B��B��B�ZB�3B�|B�B�B��B�B�B
XB�B�AB�QB�DB�B�$B�B��BʦB�B�XB�9B�B�%B�uB~(By�Bv+BkB?�B�B��B�)B��B��B��B��B��B�B��B�lB�1B�zB�aB�B��B� Bz�Bv`Bs�Bq�Bm�Bi�B`'BQNBJ�B8�B)DBIB�B��B��B�;B�jB�!B��B�Bu�BncBjB]~BV�BD�B<6B72B1AB*0B vBB
��B
��B
�GB
��B
�4B
�B
��B
��B
��B
��B
��B
�TB
�RB
��B
y�B
tnB
q�B
mCB
i_B
d�B
b4B
_B
]IB
\�B
\�B
Z7B
WYB
T�B
RTB
MB
HfB
F?B
B'B
<�B
9rB
2aB
.�B
(�B
#:B
qB
mB
^B
�B	��B	��B	�B	�B	�jB	�B	��B	��B	�6B	��B	��B	��B	��B	��B	�qB	��B	��B	��B	�]B	��B	�|B	��B	�B	�=B	��B	��B	��B	��B	�:B	�PB	��B	��B	|�B	w�B	t�B	r�B	pB	lB	i�B	h�B	e�B	c�B	a�B	^5B	\)B	T�B	N"B	FB	;B	:DB	8�B	9	B	2�B	0;B	.�B	-)B	+�B	)�B	'8B	%�B	!�B	 BB	VB	)B	�B	B	:B	�B	�B	
�B	�B	�B	�B	�B	 �B��B�B�8B��B�B�[B��B�kB��B�B�B��B��B�DB�
B�B�B��B�B�NB�|B�;B�~B�~BۦB��BچB�B��B��B��B�YBևB�B�B�B�:B��BѝB�4B�}B�}B�bB�.B�(B�B�<B�4B��B��B��BңB�:B�:B�[BԕB�mB��B��B��B�BؓB�_B�_BؓBؓBخBخB�yBخB��B��B�QB�~B�OB��B�BB��B�B��B�FB�`B�B�LB�B�B��B�QB��B�UB�[B�vB��B��B��B��B	 B	 4B	 OB	;B	�B	�B	
�B	�B	"B	VB	VB	�B	[B	�B	�B	�B	IB	�B	B	!B	 �B	!HB	!�B	"�B	#�B	$&B	$�B	%B	&�B	,�B	/�B	/�B	0�B	1[B	1�B	2B	2aB	6zB	<�B	A�B	E�B	H�B	IB	IRB	I�B	I�B	KB	NB	M�B	NB	NVB	N�B	OvB	O�B	O�B	P.B	PB	PHB	PHB	PHB	P.B	P�B	R�B	TaB	V�B	Y�B	[=B	\]B	_�B	b�B	c�B	e�B	fLB	f�B	gmB	kkB	m)B	ncB	o�B	p�B	r�B	t�B	u�B	w�B	zDB	}B	�B	�GB	��B	�9B	�mB	�mB	��B	��B	�bB	�,B	��B	�$B	��B	�B	�B	��B	�jB	�\B	��B	�FB	�B	�*B	��B	�B	�B	��B	�OB	��B	��B	��B	� B	��B	B	ɆB	��B	�}B	�[B	�?B	��B	�#B	��B	޸B	�vB	��B	��B	�B	�&B	��B	�B	��B	�B	�B	�B	�B	��B	��B	�dB	�B	�B	�<B
uB
9B
�B
�B
�B
	B
	RB

=B
^B
�B
 B
4B
4B
 B
B
B
B
B
hB
�B
sB
eB
�B
�B
�B
 B
"NB
"�B
%�B
&�B
(>B
)�B
-wB
.�B
/�B
0;B
1AB
7�B
:�B
;�B
<6B
;B
<�B
=�B
>]B
?�B
@�B
B�B
C�B
D�B
FYB
F�B
F�B
H�B
JrB
J�B
K)B
K�B
NpB
O\B
PB
Q4B
U2B
YB
Z7B
[	B
\]B
_�B
dZB
g8B
jB
l"B
lWB
l�B
m�B
oB
p�B
p�B
p�B
p�B
q�B
s3B
s�B
v�B
y>B
z�B
|B
}�B
�B
�B
�'B
�'B
�'B
�AB
�[B
�uB
�[B
�[B
��B
�-B
��B
�gB
�B
��B
�B
�B
�YB
��B
��B
�_B
�B
�KB
�1B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�}B
��B
��B
�B
�B
�[B
��B
�MB
�gB
��B
��B
��B
��B
�MB
��B
�YB
��B
�qB
��B
�)B
��B
��B
�IB
�~B
�B
�OB
��B
�B
�\B
��B
�-B
��B
��B
�hB
�NB
��B
��B
��B
�kB
��B
�CB
�/B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�MB
�MB
��B
��B
�TB
�ZB
��B
��B
�+B
��B
��B
��B
�B
��B
��B
��B
�>B
�rB
��B
��B
�dB
�jB
�"B
�B
�}B
� B
� B
��B
��B
��B
��B
��B
��B
�B
�B
�_B
ǔB
�B
ȚB
��B
��B
��B
�B
�RB
�7B
�RB
�lB
ɆB
��B
��B
�#B
ʌB
��B
�)B
�^B
�jB
��B
�\B
�vB
ϑB
ϑB
ϑB
�}B
ѷB
� B
�TB
�oB
ҽB
ңB
��B
�B
�uB
�B
�gB
�gB
�SB
֡B
ؓB
�7B
��B
��B
��B
��B
��B
��B
�	B
یB
�B
��B
��B
�/B
ݘB
ݘB
�jB
�OB
ބB
��B
�B
�|B
�B
��B
�B
�B
�B
�B
� B
�nB
��B
�B
�,B
�B
�LB
�B
�B
�B
�B
�B
��B
�$B
�XB
��B
��B
��B
�B
�B
�B
�*B
�DB
�_B
�_B
�B
�B
�B
��B
�KB
�B
�B
�B
�B
�cB
��B
�UB
��B
�vB
��B
��B
�B
��B
�-B
�GB
�aB
�|B
�B
�B
��B
�B
�MB
�hB
�B
�?B
�+B
��B
�B
��B
�>B
��B
��B
��B
��B
��B
��B
�B
�6B
�B
�jB
�jB
��B
��B
��B
�"B
�qB
�VB
�BB
��B
�cB
��B 4B �B �BB;B�B�B�B[BuB�B�B-B-B{B{BB3B�B�B?B�BB+BzB�B1BKB�B	B	B�B	B	B	�B	�B	�B
rB
�BBB
�B)BxB�B�BPB�B�B�B<B"B�B�B�B�B�B�B�B�B�B�B�BBBvB�BBBbB�B B B4B�B�B�B�BTB�B�B�B�B�B�B@BuBuB�B�B{B�B�B�B�B9BmBSB
B?B$BsBsB�B�B�B�B�B�B�B�B+B�B1BB�B�B7BkB�B�B�BqB�B�BB)B]BxBxB�B�B�B�B�BB/B/B�BB5B�B�B�BB!B�B�B�B�B�B�B�B�B�B�B�B B�B�B BB �B �B �B �B �B �B �B �B �B �B �B �B!B �B!B �B!HB!bB!�B!�B"hB"�B#:B#nB#�B#�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   BB�B%BMB�BBBgB�B�B�BBSBB�BuB�B��B��B�ZB�3B�|B�B�B��B�B�B
XB�B�AB�QB�DB�B�$B�B��BʦB�B�XB�9B�B�%B�uB~(By�Bv+BkB?�B�B��B�)B��B��B��B��B��B�B��B�lB�1B�zB�aB�B��B� Bz�Bv`Bs�Bq�Bm�Bi�B`'BQNBJ�B8�B)DBIB�B��B��B�;B�jB�!B��B�Bu�BncBjB]~BV�BD�B<6B72B1AB*0B vBB
��B
��B
�GB
��B
�4B
�B
��B
��B
��B
��B
��B
�TB
�RB
��B
y�B
tnB
q�B
mCB
i_B
d�B
b4B
_B
]IB
\�B
\�B
Z7B
WYB
T�B
RTB
MB
HfB
F?B
B'B
<�B
9rB
2aB
.�B
(�B
#:B
qB
mB
^B
�B	��B	��B	�B	�B	�jB	�B	��B	��B	�6B	��B	��B	��B	��B	��B	�qB	��B	��B	��B	�]B	��B	�|B	��B	�B	�=B	��B	��B	��B	��B	�:B	�PB	��B	��B	|�B	w�B	t�B	r�B	pB	lB	i�B	h�B	e�B	c�B	a�B	^5B	\)B	T�B	N"B	FB	;B	:DB	8�B	9	B	2�B	0;B	.�B	-)B	+�B	)�B	'8B	%�B	!�B	 BB	VB	)B	�B	B	:B	�B	�B	
�B	�B	�B	�B	�B	 �B��B�B�8B��B�B�[B��B�kB��B�B�B��B��B�DB�
B�B�B��B�B�NB�|B�;B�~B�~BۦB��BچB�B��B��B��B�YBևB�B�B�B�:B��BѝB�4B�}B�}B�bB�.B�(B�B�<B�4B��B��B��BңB�:B�:B�[BԕB�mB��B��B��B�BؓB�_B�_BؓBؓBخBخB�yBخB��B��B�QB�~B�OB��B�BB��B�B��B�FB�`B�B�LB�B�B��B�QB��B�UB�[B�vB��B��B��B��B	 B	 4B	 OB	;B	�B	�B	
�B	�B	"B	VB	VB	�B	[B	�B	�B	�B	IB	�B	B	!B	 �B	!HB	!�B	"�B	#�B	$&B	$�B	%B	&�B	,�B	/�B	/�B	0�B	1[B	1�B	2B	2aB	6zB	<�B	A�B	E�B	H�B	IB	IRB	I�B	I�B	KB	NB	M�B	NB	NVB	N�B	OvB	O�B	O�B	P.B	PB	PHB	PHB	PHB	P.B	P�B	R�B	TaB	V�B	Y�B	[=B	\]B	_�B	b�B	c�B	e�B	fLB	f�B	gmB	kkB	m)B	ncB	o�B	p�B	r�B	t�B	u�B	w�B	zDB	}B	�B	�GB	��B	�9B	�mB	�mB	��B	��B	�bB	�,B	��B	�$B	��B	�B	�B	��B	�jB	�\B	��B	�FB	�B	�*B	��B	�B	�B	��B	�OB	��B	��B	��B	� B	��B	B	ɆB	��B	�}B	�[B	�?B	��B	�#B	��B	޸B	�vB	��B	��B	�B	�&B	��B	�B	��B	�B	�B	�B	�B	��B	��B	�dB	�B	�B	�<B
uB
9B
�B
�B
�B
	B
	RB

=B
^B
�B
 B
4B
4B
 B
B
B
B
B
hB
�B
sB
eB
�B
�B
�B
 B
"NB
"�B
%�B
&�B
(>B
)�B
-wB
.�B
/�B
0;B
1AB
7�B
:�B
;�B
<6B
;B
<�B
=�B
>]B
?�B
@�B
B�B
C�B
D�B
FYB
F�B
F�B
H�B
JrB
J�B
K)B
K�B
NpB
O\B
PB
Q4B
U2B
YB
Z7B
[	B
\]B
_�B
dZB
g8B
jB
l"B
lWB
l�B
m�B
oB
p�B
p�B
p�B
p�B
q�B
s3B
s�B
v�B
y>B
z�B
|B
}�B
�B
�B
�'B
�'B
�'B
�AB
�[B
�uB
�[B
�[B
��B
�-B
��B
�gB
�B
��B
�B
�B
�YB
��B
��B
�_B
�B
�KB
�1B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�}B
��B
��B
�B
�B
�[B
��B
�MB
�gB
��B
��B
��B
��B
�MB
��B
�YB
��B
�qB
��B
�)B
��B
��B
�IB
�~B
�B
�OB
��B
�B
�\B
��B
�-B
��B
��B
�hB
�NB
��B
��B
��B
�kB
��B
�CB
�/B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�MB
�MB
��B
��B
�TB
�ZB
��B
��B
�+B
��B
��B
��B
�B
��B
��B
��B
�>B
�rB
��B
��B
�dB
�jB
�"B
�B
�}B
� B
� B
��B
��B
��B
��B
��B
��B
�B
�B
�_B
ǔB
�B
ȚB
��B
��B
��B
�B
�RB
�7B
�RB
�lB
ɆB
��B
��B
�#B
ʌB
��B
�)B
�^B
�jB
��B
�\B
�vB
ϑB
ϑB
ϑB
�}B
ѷB
� B
�TB
�oB
ҽB
ңB
��B
�B
�uB
�B
�gB
�gB
�SB
֡B
ؓB
�7B
��B
��B
��B
��B
��B
��B
�	B
یB
�B
��B
��B
�/B
ݘB
ݘB
�jB
�OB
ބB
��B
�B
�|B
�B
��B
�B
�B
�B
�B
� B
�nB
��B
�B
�,B
�B
�LB
�B
�B
�B
�B
�B
��B
�$B
�XB
��B
��B
��B
�B
�B
�B
�*B
�DB
�_B
�_B
�B
�B
�B
��B
�KB
�B
�B
�B
�B
�cB
��B
�UB
��B
�vB
��B
��B
�B
��B
�-B
�GB
�aB
�|B
�B
�B
��B
�B
�MB
�hB
�B
�?B
�+B
��B
�B
��B
�>B
��B
��B
��B
��B
��B
��B
�B
�6B
�B
�jB
�jB
��B
��B
��B
�"B
�qB
�VB
�BB
��B
�cB
��B 4B �B �BB;B�B�B�B[BuB�B�B-B-B{B{BB3B�B�B?B�BB+BzB�B1BKB�B	B	B�B	B	B	�B	�B	�B
rB
�BBB
�B)BxB�B�BPB�B�B�B<B"B�B�B�B�B�B�B�B�B�B�B�BBBvB�BBBbB�B B B4B�B�B�B�BTB�B�B�B�B�B�B@BuBuB�B�B{B�B�B�B�B9BmBSB
B?B$BsBsB�B�B�B�B�B�B�B�B+B�B1BB�B�B7BkB�B�B�BqB�B�BB)B]BxBxB�B�B�B�B�BB/B/B�BB5B�B�B�BB!B�B�B�B�B�B�B�B�B�B�B�B B�B�B BB �B �B �B �B �B �B �B �B �B �B �B �B!B �B!B �B!HB!bB!�B!�B"hB"�B#:B#nB#�B#�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230122184144  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230122184145  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230122184146  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230122184146                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230122184147  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230122184147  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230122185824                      G�O�G�O�G�O�                