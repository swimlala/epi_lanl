CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:04Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170904  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               .A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؗ�ۗa�1   @ؗ�m�D2@6�1&�x��c���+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    .A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  BpffBx  B��B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�\D�!�D�[�D��D��RD�!�D�` D��)D��HD��D�J�D��D��D��D�`RDډ�D��{D��D�V�D��D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffB��BffBffB'ffB/ffB7��B?ffBGffBOffBWffB_ffBgffBo��BwffB  B��3B��3B��3B��3B��3B�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��gCٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCI�4CKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCp DC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDtəDy��D��D�V�D��GD���D��D�[3D��\D��{D��D�E�D��RD���D�D�[�DڅD�߮D��D�Q�D�~D�׮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�~�A�~�AŁAŁAŉ7AōPAŏ\Aŏ\Aŏ\AőhAœuAőhAœuAœuAœuAŕ�Aŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Ař�Ař�Aś�Aś�Aŗ�AœuA�n�A���A�(�A��\A�
=A� �A���A�ȴA�;dA���A��^A���A��7A�7LA�n�A�bA�(�A��
A���A�G�A��A���A�|�A�&�A���A���A�+A��DA�5?A��^A� �A��mA�M�A���A���A���A��A�Q�A�bA���A���A��HA���A�O�A�7LA���A�x�A�1A���A��A�C�A���A�C�A�\)A��-A��DA�"�A�t�A�r�A��A�hsA�K�A��A��A���A�C�A���A��wA�C�A���A��A��A��A��A��jA���A��;A�l�A��A��A�l�A�/A� �Az��Au�PAo��Am�-Ak��Ai��Af��Ad�Ad�uAdM�Ad=qAd�Ad  AchsAaC�A[�#AZ�RAW�AU�AT$�AS�FAR�+AQO�AM��AJ�/AH^5AG�;AHbAIhsAH-AFAB�!AB{AA�7A?t�A:��A9�;A8r�A7��A6�\A41'A3�
A3|�A2��A25?A1�A0��A/�#A-�wA+��A(��A&��A&�DA&M�A%%A#��A#�A#VA#A"�A"�9A"z�A"A�A"1'A"5?A"M�A!�A!O�A �A �jA\)AA�A�yA�AbA�A�An�A�
A��A+Az�Ax�A�A�DAA�7AK�AĜA�RA�A7LA�A�RAQ�A�
A�A9XAA/AĜAZA�;A��A�A7LA
bNA
JA	�#A	
=AZA1A/A�A��AK�An�A1AƨA�AA��A ��@��@��@���@��j@���@���@�ff@�@�|�@��9@��H@�Q�@�!@�V@���@�
=@�7L@��
@�~�@�&�@�w@�dZ@�33@��@�ȴ@���@�V@���@�9X@�
=@�ƨ@ڧ�@���@�9X@���@ԛ�@��
@�"�@��@�x�@�%@д9@Гu@ϥ�@θR@��#@�&�@��m@ʰ!@�5?@ɺ^@���@�S�@�n�@�?}@�|�@���@�-@�?}@�1@���@�\)@�x�@�I�@�  @���@�-@�j@�|�@�ff@��-@���@�\)@��+@��@���@�?}@���@��@�=q@��-@���@�r�@�1'@�dZ@��+@��@�X@�Q�@��
@��@�ff@�$�@���@��@�G�@��@�?}@���@��/@��u@���@���@���@�r�@�ƨ@�\)@���@�Q�@�@��#@���@��@�r�@���@�l�@��!@�ȴ@���@��R@���@�
=@��@�o@��H@��@��@�
=@��@�C�@�\)@�"�@��@��@��@�o@��@���@�5?@��#@���@���@���@���@��h@�X@�/@���@��/@��j@��D@�I�@��P@��@��y@��+@�@��^@��-@��h@�p�@��u@�ȴ@��T@���@�{@��D@�b@���@�~�@�$�@�$�@�-@�=q@�=q@�5?@�{@�@��T@�@�5?@�v�@�=q@��T@�7L@�Z@��@��F@���@�|�@��@���@�-@��T@���@��^@��@�$�@��@��@� �@���@��@�ƨ@�ƨ@���@�t�@���@���@���@��w@�+@���@�ff@�{@��T@��^@��@�p�@�X@�/@�%@��`@��9@���@��u@��@�I�@�(�@���@���@�K�@��@��@��R@���@�M�@�@��@��j@��@�z�@�z�@�z�@�r�@�bN@�Z@��@�|�@�dZ@�
=@���@���@�ff@�\�@uQ�@o�@f��@a`B@XFt@O��@H�?@@"h@:!�@7e�@0[�@+33@%�@��@!�@8�@;�@�n@	��@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�n�A�~�A�~�AŁAŁAŉ7AōPAŏ\Aŏ\Aŏ\AőhAœuAőhAœuAœuAœuAŕ�Aŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Ař�Ař�Aś�Aś�Aŗ�AœuA�n�A���A�(�A��\A�
=A� �A���A�ȴA�;dA���A��^A���A��7A�7LA�n�A�bA�(�A��
A���A�G�A��A���A�|�A�&�A���A���A�+A��DA�5?A��^A� �A��mA�M�A���A���A���A��A�Q�A�bA���A���A��HA���A�O�A�7LA���A�x�A�1A���A��A�C�A���A�C�A�\)A��-A��DA�"�A�t�A�r�A��A�hsA�K�A��A��A���A�C�A���A��wA�C�A���A��A��A��A��A��jA���A��;A�l�A��A��A�l�A�/A� �Az��Au�PAo��Am�-Ak��Ai��Af��Ad�Ad�uAdM�Ad=qAd�Ad  AchsAaC�A[�#AZ�RAW�AU�AT$�AS�FAR�+AQO�AM��AJ�/AH^5AG�;AHbAIhsAH-AFAB�!AB{AA�7A?t�A:��A9�;A8r�A7��A6�\A41'A3�
A3|�A2��A25?A1�A0��A/�#A-�wA+��A(��A&��A&�DA&M�A%%A#��A#�A#VA#A"�A"�9A"z�A"A�A"1'A"5?A"M�A!�A!O�A �A �jA\)AA�A�yA�AbA�A�An�A�
A��A+Az�Ax�A�A�DAA�7AK�AĜA�RA�A7LA�A�RAQ�A�
A�A9XAA/AĜAZA�;A��A�A7LA
bNA
JA	�#A	
=AZA1A/A�A��AK�An�A1AƨA�AA��A ��@��@��@���@��j@���@���@�ff@�@�|�@��9@��H@�Q�@�!@�V@���@�
=@�7L@��
@�~�@�&�@�w@�dZ@�33@��@�ȴ@���@�V@���@�9X@�
=@�ƨ@ڧ�@���@�9X@���@ԛ�@��
@�"�@��@�x�@�%@д9@Гu@ϥ�@θR@��#@�&�@��m@ʰ!@�5?@ɺ^@���@�S�@�n�@�?}@�|�@���@�-@�?}@�1@���@�\)@�x�@�I�@�  @���@�-@�j@�|�@�ff@��-@���@�\)@��+@��@���@�?}@���@��@�=q@��-@���@�r�@�1'@�dZ@��+@��@�X@�Q�@��
@��@�ff@�$�@���@��@�G�@��@�?}@���@��/@��u@���@���@���@�r�@�ƨ@�\)@���@�Q�@�@��#@���@��@�r�@���@�l�@��!@�ȴ@���@��R@���@�
=@��@�o@��H@��@��@�
=@��@�C�@�\)@�"�@��@��@��@�o@��@���@�5?@��#@���@���@���@���@��h@�X@�/@���@��/@��j@��D@�I�@��P@��@��y@��+@�@��^@��-@��h@�p�@��u@�ȴ@��T@���@�{@��D@�b@���@�~�@�$�@�$�@�-@�=q@�=q@�5?@�{@�@��T@�@�5?@�v�@�=q@��T@�7L@�Z@��@��F@���@�|�@��@���@�-@��T@���@��^@��@�$�@��@��@� �@���@��@�ƨ@�ƨ@���@�t�@���@���@���@��w@�+@���@�ff@�{@��T@��^@��@�p�@�X@�/@�%@��`@��9@���@��u@��@�I�@�(�@���@���@�K�@��@��@��R@���@�M�@�@��@��j@��@�z�@�z�@�z�@�r�@�bN@�Z@��@�|�@�dZ@�
=@���@���G�O�@�\�@uQ�@o�@f��@a`B@XFt@O��@H�?@@"h@:!�@7e�@0[�@+33@%�@��@!�@8�@;�@�n@	��@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BH�BH�BI�BN�B[#Bq�Bw�Bx�B}�B�1B�bB�\B�\B�\B�\B�\B�VB�PB�PB�PB�DB�=B�=B�7B�%B�+B�B�B�B� B}�B}�By�Bw�Bw�Bt�Bs�Bs�Bs�Br�Bq�Bq�Bo�Bk�BffBdZBbNBaHB`BBZBYBQ�BI�B8RB#�B�B\B+B��B�B�NB��BƨBÖB��B�jB�FB�3B��B�\B�%B|�Bt�Bk�BgmBW
B<jB.BPBB
��B
�;B
��B
}�B
A�B

=B	�ZB	�FB	��B	�hB	� B	q�B	aHB	aHB	`BB	_;B	^5B	\)B	XB	J�B	+B	�B	{B	B	B	  B��B�B�#B��BǮB��B�)B	B	1B��B�B�B�mB�B�RB��B�^B�FB�'B�B��B��B��B��B��B��B��B��B�1B}�Bl�BjBl�BjBdZBe`BffBffBffBe`Be`Be`BffBffBgmBp�Bq�Bo�Bm�Be`BcTBcTBe`B_;BZBYBT�BR�BR�BQ�BM�BK�BI�BG�BF�BC�BC�BD�BD�BE�BA�B@�B?}B>wB<jB<jB<jB=qB=qB:^B9XB8RB7LB7LB6FB6FB49B49B33B49B33B49B1'B0!B0!B0!B.B.B/B.B-B.B-B+B+B+B+B+B+B)�B+B)�B)�B)�B&�B&�B&�B%�B&�B&�B)�B+B+B,B-B/B49B6FB7LB7LB8RB:^B>wB?}B@�BB�BC�BD�BD�BE�BG�BG�BH�BH�BG�BJ�BK�BL�BN�BP�BS�BS�BS�BW
BZB[#B_;BbNBbNBdZBffBjBk�Bk�Br�Bs�Bt�Bs�By�B|�B}�B� B�B�1B�7B�JB�PB�bB�hB�uB��B��B��B��B��B��B��B�B�!B�9B�RB�XB�jB�wB��BƨB��B��B�/B�TB�B�B��B��B	B	+B		7B	1B	+B	
=B	uB	�B	oB	hB	hB	�B	�B	�B	�B	 �B	%�B	)�B	-B	49B	8RB	;dB	?}B	@�B	D�B	J�B	M�B	Q�B	W
B	YB	[#B	[#B	[#B	[#B	\)B	\)B	^5B	`BB	aHB	bNB	e`B	l�B	l�B	o�B	p�B	q�B	r�B	r�B	t�B	u�B	{�B	}�B	}�B	�B	�B	�%B	�+B	�1B	�+B	�+B	�B	� B	�B	�B	�B	� B	�B	�B	�%B	�%B	�+B	�7B	�DB	�JB	�PB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�FB	�RB	�dB	�qB	�}B	��B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�
B	�
B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�#B	�B	�B	�
B	�KB	�-B
^B
<B
�B
;B
(�B
1B
:DB
B�B
F�B
K�B
Q4B
V�B
^OB
c�B
j�B
m�B
q[B
wfB
zx111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B@[B?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB?UB@[B@[BAaBFBR�BiOBotBpzBu�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B}�B~�B|�Bz�Bz�Bw�Bu�Bu�Bq�BowBoxBleBk_Bk_Bk_BjYBiSBiSBgHBc/B^B\BY�BX�BW�BQ�BP�BI�BAgB0 B�BDBB��B�oB�EB�BˮB�_B�MB�:B�"B��B��B�B�B}�Bt�BlyBcCB_+BN�B4+B%�BB
��B
��B
�B
�sB
u�B
9]B
B	�5B	�$B	��B	�IB	w�B	i�B	Y-B	Y-B	X'B	W B	VB	TB	O�B	B�B	"�B	�B	fB�B��B��B��B�B�B��B��B��B�B��B	 B��B�B�uB�]B�B�FB�wB�RB�;B�B��B��B��B��B��B��B��B��B��B�*Bu�Bd�BbzBd�Bb{B\VB]\B^bB^bB^bB]\B]\B]\B^bB^bB_iBh�Bi�Bg�Be�B]]B[QB[QB]]BW9BRBQBL�BJ�BJ�BI�BE�BC�BA�B?�B>�B;�B;�B<�B<�B=�B9�B8�B7~B6xB4kB4kB4kB5rB5rB2`B1ZB0TB/NB/NB.HB.HB,;B,;B+5B,;B+6B,<B)*B($B($B($B&B&B'B&B%B&B%B#B#B#B#B#B#B"B#B"B"B"B�B�B�B�B�B�B"B#B#B$B%B'!B,?B.LB/RB/RB0WB2cB6|B7�B8�B:�B;�B<�B<�B=�B?�B?�B@�B@�B?�BB�BC�BD�BF�BH�BK�BK�BK�BOBR"BS(BW@BZSBZSB\_B^kBb�Bc�Bc�Bj�Bk�Bl�Bk�Bq�Bt�Bu�BxBy
B�5B�;B�NB�TB�eB�kB�xB��B��B��B��B��B��B��B�B�#B�;B�TB�ZB�kB�xB��B��B��B��B�/B�SB�~B�B��B��B�	B�(B	4B	 .B�(B	:B	qB	}B	
lB		eB		eB	�B	�B	�B	�B	�B	�B	!�B	%	B	,4B	0LB	3^B	7wB	8}B	<�B	B�B	E�B	I�B	OB	QB	SB	SB	SB	SB	T!B	T!B	V-B	X:B	Y@B	ZFB	]XB	d�B	d�B	g�B	h�B	i�B	j�B	j�B	l�B	m�B	s�B	u�B	u�B	x�B	{B	~B	!B	�'B	!B	!B	{B	w�B	|B	}B	x�B	w�B	{	B	|B	~B	~B	"B	�-B	�:B	�@B	�FB	�LB	�RB	�^B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�!B	�-B	�:B	�:B	�FB	�XB	�eB	�qB	�|B	��B	��B	��B	��B	��B	´B	úB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�
B	�
G�O�B	�=B	�B
NB
,B
�B
+B
 �B
(�B
23B
:�B
>�B
C�B
I"B
N�B
V=B
[�B
b�B
e�B
iHB
oSB
re111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170904    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170904  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170904  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                