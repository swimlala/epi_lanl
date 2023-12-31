CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:29Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170929  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @����i�1   @���{@6L�C���b�
=p��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  Dy�D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�&fD�[�D��
D��fD� �D�V�D��D��{D��D�Z�D��qD�޸D�!HD�W
Dڏ
D���D�
D�QHD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCC�4CEٚCGٚCI� CKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fD|�D�fDvfD�fDp D� DvfD�fDvfD�fDvfD�fDvfD�fD|�D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6� D7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;��D<|�D<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDp DD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY��DZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt� Dy�D�!�D�V�D��=D�əD��D�Q�D��RD�߮D��D�U�D���D���D�{D�R=Dڊ=D��)D�=D�L{D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��yA��A�(�A��A�JA���A��A��mA��HA��
A���A¶FA�\)A��mA�ĜA��\A�S�A���A�VA��+A�E�A��9A�+A�?}A���A�ZA�G�A��A�^5A�%A�+A�{A��#A�v�A�bNA�1'A��A�1'A��9A�;dA��\A�S�A�VA��A�O�A��A��A�A�A��A���A�ffA��A��PA��A��A�t�A�ƨA�ZA���A�(�A��#A�M�A���A�
=A��FA���A�A�A��!A���A�ƨA�&�A���A�5?A�E�A���A�ffA�hsA��/A�7LA���A�bA�&�A��PA�hsA��A�bNA�A�n�A�-A���A�JA�ȴA��A��`A�?}A��A�jA�I�A�/A��A��#A�-A�9XA��A�bNA�=qA��A�n�A�r�A��HA�A�A�t�A��DA��#A���A�7LAoA{�-AvȴAs�-Ar�\Ap�Ak\)Ah~�Aex�Ad(�Ab�A`��A_�hA^�/A^(�A[XAWS�AV �AUATffAS�hAR�\AN�+AM%AK�AIG�AF�\ABM�A>�A<jA;&�A9%A7��A7�A7XA7
=A5S�A3��A2�+A0E�A.z�A,�A,ffA+O�A)�mA'�
A&��A%�7A$�\A#ƨA"��A!�A�^A�yAM�AK�A�uA$�A��A�A��AO�AĜA��A��A{A�A��A�/A�
A�+A��A��A�A��Al�AjA�^A
�A	�A	�A��A�DA��A|�A&�Az�A�A��AI�A`BA+A �R@�l�@���@�1@��@�;d@�E�@���@�33@�Ĝ@��;@�R@�@�S�@�~�@�p�@�D@�(�@�  @�C�@��@�`B@�j@�&�@�u@���@���@�`B@��u@�C�@�J@�I�@ڟ�@�V@ו�@և+@�x�@�I�@�33@�ff@Ϯ@��@��`@��@�
=@���@�7L@���@�{@�X@ě�@���@Ý�@���@��F@�-@�`B@��@�ƨ@�l�@���@��-@�p�@�p�@�hs@�X@�/@�(�@���@�E�@��!@�J@���@��j@��w@��@�33@���@��@���@��@�r�@��@���@�o@�-@��@��@��9@��@�(�@��w@��P@�C�@�S�@�33@�@��y@��!@�^5@��^@�O�@�V@���@�(�@���@�ƨ@�t�@��@�M�@�-@��@�hs@���@��`@��j@��D@�bN@���@�\)@�@���@�v�@���@��@�`B@�G�@�%@�j@���@���@�S�@��@��!@�E�@�@�hs@�/@���@��u@��@���@��@���@�|�@�S�@�C�@�+@���@���@��+@�M�@�{@�@��@�&�@��`@��@���@�j@��@��@��w@��P@�K�@��H@���@�ȴ@���@�@��@���@��@��;@���@�|�@�;d@���@�K�@��H@�
=@���@��y@��y@���@�n�@�$�@��-@�7L@��`@��`@���@���@��j@�Z@�A�@��;@���@��w@��@�l�@�S�@�S�@�K�@��@�ff@�=q@�@�p�@��@��D@�A�@� �@� �@��
@�;d@��@���@���@�~�@�^5@�ff@�E�@�J@���@��#@���@��h@��@�G�@��@�Ĝ@���@��u@�bN@�1'@���@��F@�|�@�C�@�C�@�33@�"�@�o@���@���@�v�@�V@�=q@���@��7@�X@�7L@�%@��@� �@��@�1@��;@��P@�\)@��@��!@�~�@�=q@�$�@���@��#@���@���@���@��@��@���@��9@���@�Q�@�b@�@{�k@r�"@k8@d��@[�+@U=�@ML�@H`�@Bȴ@=�@6n�@0�@*�R@&M�@ q@C@��@͟@�:@e�@�q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��yA��A�(�A��A�JA���A��A��mA��HA��
A���A¶FA�\)A��mA�ĜA��\A�S�A���A�VA��+A�E�A��9A�+A�?}A���A�ZA�G�A��A�^5A�%A�+A�{A��#A�v�A�bNA�1'A��A�1'A��9A�;dA��\A�S�A�VA��A�O�A��A��A�A�A��A���A�ffA��A��PA��A��A�t�A�ƨA�ZA���A�(�A��#A�M�A���A�
=A��FA���A�A�A��!A���A�ƨA�&�A���A�5?A�E�A���A�ffA�hsA��/A�7LA���A�bA�&�A��PA�hsA��A�bNA�A�n�A�-A���A�JA�ȴA��A��`A�?}A��A�jA�I�A�/A��A��#A�-A�9XA��A�bNA�=qA��A�n�A�r�A��HA�A�A�t�A��DA��#A���A�7LAoA{�-AvȴAs�-Ar�\Ap�Ak\)Ah~�Aex�Ad(�Ab�A`��A_�hA^�/A^(�A[XAWS�AV �AUATffAS�hAR�\AN�+AM%AK�AIG�AF�\ABM�A>�A<jA;&�A9%A7��A7�A7XA7
=A5S�A3��A2�+A0E�A.z�A,�A,ffA+O�A)�mA'�
A&��A%�7A$�\A#ƨA"��A!�A�^A�yAM�AK�A�uA$�A��A�A��AO�AĜA��A��A{A�A��A�/A�
A�+A��A��A�A��Al�AjA�^A
�A	�A	�A��A�DA��A|�A&�Az�A�A��AI�A`BA+A �R@�l�@���@�1@��@�;d@�E�@���@�33@�Ĝ@��;@�R@�@�S�@�~�@�p�@�D@�(�@�  @�C�@��@�`B@�j@�&�@�u@���@���@�`B@��u@�C�@�J@�I�@ڟ�@�V@ו�@և+@�x�@�I�@�33@�ff@Ϯ@��@��`@��@�
=@���@�7L@���@�{@�X@ě�@���@Ý�@���@��F@�-@�`B@��@�ƨ@�l�@���@��-@�p�@�p�@�hs@�X@�/@�(�@���@�E�@��!@�J@���@��j@��w@��@�33@���@��@���@��@�r�@��@���@�o@�-@��@��@��9@��@�(�@��w@��P@�C�@�S�@�33@�@��y@��!@�^5@��^@�O�@�V@���@�(�@���@�ƨ@�t�@��@�M�@�-@��@�hs@���@��`@��j@��D@�bN@���@�\)@�@���@�v�@���@��@�`B@�G�@�%@�j@���@���@�S�@��@��!@�E�@�@�hs@�/@���@��u@��@���@��@���@�|�@�S�@�C�@�+@���@���@��+@�M�@�{@�@��@�&�@��`@��@���@�j@��@��@��w@��P@�K�@��H@���@�ȴ@���@�@��@���@��@��;@���@�|�@�;d@���@�K�@��H@�
=@���@��y@��y@���@�n�@�$�@��-@�7L@��`@��`@���@���@��j@�Z@�A�@��;@���@��w@��@�l�@�S�@�S�@�K�@��@�ff@�=q@�@�p�@��@��D@�A�@� �@� �@��
@�;d@��@���@���@�~�@�^5@�ff@�E�@�J@���@��#@���@��h@��@�G�@��@�Ĝ@���@��u@�bN@�1'@���@��F@�|�@�C�@�C�@�33@�"�@�o@���@���@�v�@�V@�=q@���@��7@�X@�7L@�%@��@� �@��@�1@��;@��P@�\)@��@��!@�~�@�=q@�$�@���@��#@���@���@���@��@��@���@��9@���@�Q�@�bG�O�@{�k@r�"@k8@d��@[�+@U=�@ML�@H`�@Bȴ@=�@6n�@0�@*�R@&M�@ q@C@��@͟@�:@e�@�q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	� B	� B	� B	� B	� B	� B	�B	�+B	��B	�RB	��B	�qB	�wB	��B	��B	��B	�B	�TB	�B
B
2-B
L�B
cTB
�1B
�B
�BB+B
=B�B�BhB�B<jBR�BhsBs�B�B��B��B��B�B��B��B��B��B�B�fB��BB	7B�B#�B+B7LB<jB@�B>wB.B&�B'�B1'BH�BH�BC�BG�B?}B9XB2-B0!B'�B"�B�BhBhB�B�BB��B�ZB�B�qB�-B�B��B��B��B�Bu�Bt�Bn�Bl�Bm�Bt�Bx�B�By�BiyBo�Bn�BdZBO�B9XB0!B"�B�B
=B
�B
�B
ɺB
�FB
{�B
I�B
#�B	��B	�HB	��B	ŢB	��B	�B	m�B	aHB	XB	K�B	@�B	:^B	33B	+B	hB	%B	B��B�B�B�HB��B��BĜB�9B��B{�By�Br�Bp�BiyBhsBhsBffBe`B^5B]/B\)BZBW
BT�BT�BP�BP�BK�BK�BI�BI�BJ�BG�BF�BD�BC�BC�BB�BB�BA�BB�BA�BA�B@�B?}B?}B@�BA�BC�BC�BB�BA�B?}B=qB<jB<jB9XB8RB6FB5?B33B33B/B.B-B,B+B,B)�B)�B)�B)�B(�B)�B)�B+B(�B'�B&�B&�B%�B(�B'�B(�B+B+B(�B)�B)�B(�B(�B(�B(�B(�B'�B'�B(�B$�B%�B'�B)�B+B+B,B,B.B0!B1'B1'B0!B1'B1'B0!B6FB8RB;dB>wB?}B@�B@�BB�BF�BF�BI�BL�BL�BR�BW
BXBYB[#B\)B]/B`BBbNBcTBcTBcTBcTBdZBiyBk�Bl�Bt�By�Bz�B}�B�B�B�B�B�1B�DB�DB�JB�bB�oB��B��B��B��B��B��B��B��B��B�B�9B�^B�wB�}B��BÖBɺB��B��B��B�B�
B�B�#B�;B�HB�TB�fB�B�B�B�B�B�B��B��B��B	  B	B	B	1B	1B		7B	JB	bB	�B	�B	�B	�B	�B	"�B	'�B	+B	+B	-B	1'B	49B	8RB	;dB	@�B	A�B	C�B	C�B	D�B	G�B	K�B	M�B	O�B	R�B	XB	[#B	_;B	bNB	e`B	e`B	hsB	l�B	o�B	p�B	q�B	q�B	q�B	r�B	r�B	u�B	x�B	z�B	}�B	� B	�B	�%B	�1B	�=B	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�9B	�?B	�9B	�9B	�?B	�?B	�FB	�FB	�FB	�LB	�RB	�^B	�dB	�jB	�}B	��B	ÖB	ŢB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�)B	�/B	�)B	�5B	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
	�B
,B
 �B
(sB
4TB
<�B
C-B
I7B
N�B
S�B
Y�B
_�B
dB
hsB
m]B
r�B
v`B
zDB
~wB
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	u	B	tB	vB	r�B	q�B	q�B	p�B	p�B	p�B	p�B	p�B	p�B	q�B	xB	��B	�=B	�mB	�\B	�bB	�tB	��B	��B	��B	�;B	�qB	��B
#B
=�B
T,B
yB
��B
�rB
��B
��B
�BEBQB-BjB-+BC�BY/BdqBr�B�>B�WB�QB��B��B�WB�JB��B��B�B�vB�B��B+BB�B'�B-B1)B/B�B�B�B!�B9ZB9[B4=B8UB0%B*B"�B �B�B~BNBBBlB1B�B�}B�B��B�-B��B��B��B��B�HBt�Bf�Be�B__B]RB^XBe�Bi�Br�Bj�BZAB`fB_`BU#B@�B*'B �B�BUB
�B
�jB
��B
��B
�&B
l�B
:�B
�B	��B	�EB	��B	��B	��B	u"B	^�B	RVB	IB	<�B	1�B	+rB	$HB	B	�B�@B�.B�B��BްB�iB�B�B��B�`B��BmBkBc�Ba�BZ�BY�BY�BW�BV�BOjBNdBM_BKSBHABF5BF6BBBBB=B=B:�B:�B;�B8�B7�B5�B4�B4�B3�B3�B2�B3�B2�B2�B1�B0�B0�B1�B2�B4�B4�B3�B2�B0�B.�B-�B-�B*�B)�B'�B&�B$vB$vB _BXBRBLBGBMBABABABBB<BBBBBHB<B7B0B0B*B=B7B=BIBIB>BDBDB>B>B>B>B>B9B9B?B'B,B9BEBKBKBQBRB^B!kB"qB"qB!kB"qB"qB!kB'�B)�B,�B/�B0�B1�B1�B3�B7�B7�B;B>B>BD:BHRBIXBJ_BLkBMqBNwBQ�BS�BT�BT�BT�BT�BU�BZ�B\�B]�BfBk Bl&Bo9BtWBtWBu^BvdByuB|�B|�B}�B��B��B��B��B��B��B��B�B�B�+B�=B�UB�yB��B��B��B��B��B��B�B�"B�5B�AB�GB�SB�_B�wB҄BԏBסB��B��B��B��B��B��B��B�B�-B�9B�DB�WB�iB�iB�oB��B	�B	�B	�B		�B	�B	�B	B	%B	6B	6B	BB	"[B	%mB	)�B	,�B	1�B	2�B	4�B	4�B	5�B	8�B	<�B	?B	AB	D"B	I@B	LRB	PjB	S}B	V�B	V�B	Y�B	]�B	`�B	a�B	b�B	b�B	b�B	c�B	c�B	f�B	jB	lB	o B	q,B	s8B	wPB	y\B	{hB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�`B	�`B	�fB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�(B	�4B	�:B	�@B	�FB	�FB	�LB	�LB	�RB	�RB	�LB	�RB	�LB	�XB	�eB	�eB	�kB	�qB	�qB	�wB	�}B	փB	׈B	ٕB	ٕB	ٕB	ڛB	ڛB	ۡB	ۡB	ۡB	ܧB	ܧB	ܧB	޳B	��B	��B	��B	��B	��G�O�B	�B	��B
KB
B
�B
%qB
-�B
4IB
:RB
?�B
D�B
J�B
P�B
U%B
Y�B
^wB
c�B
gyB
k]B
o�B
u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144262022020411442620220204114426  AO  ARCAADJP                                                                    20200619170929    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170929  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170929  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114426  IP                  G�O�G�O�G�O�                