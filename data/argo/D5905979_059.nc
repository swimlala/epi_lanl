CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:08Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170908  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               ;A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ا��|��1   @ا�s���@6�hr� ��c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ;A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�K�D���D�ӅD�3D�T�D��RD��D�#3D�Y�D���D��{D��D�MDڊ�D��{D��D�aHD�)D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33@�33A��A=��A]��A}��A���A���A���A���Aϙ�A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��fB��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#�4C%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}� CٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD��DvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD� DvfD�fDvfD�fDvfD�fD vfD �fD!vfD!� D"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2� D3vfD3�fD4vfD4�fD5vfD5� D6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt��Dy�D�	�D�G
D���D�θD�fD�P D���D��GD�fD�UD��D�߮D��D�HRDڅ�D�߮D��D�\{D�\D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�K�A�`BA�n�A�x�A�t�A�t�A�x�A�v�A�v�A�v�A�x�A�|�A�|�A�|�A�~�A�~�A��A��A��A��A��A��A��A��A�|�A�x�A�x�A�z�A�z�A�|�A��+A��+A��A��7A��DA��DA��PA��+A�~�A�|�A�v�A�r�A��A�VA�bNA��A��A��PA�+A�E�A�\)A��hA�A��^A��HA��-A�p�A�O�A�jA��HA��A���A��A��DA�O�A� �A��A�$�A��TA�\)A��;A�A��hA�"�A��mA��;A��
A��wA���A�x�A���A��FA�r�A�M�A��A��9A���A��A���A��A�ƨA�-A���A��A�E�A���A��A��!A��HA�{A�"�A�A��A���A��A�A��A���A�$�A��A��A~A�A|jAz$�Ax��Ax �Av=qAt{ArjAp�/Am�
Al^5Ak�FAk%Aj�RAj�+AjffAjAioAg��Adn�A`�+A^�+A]�-A]x�A[O�AYVAV�jATbNASO�AR5?AO�AM33AK?}AJ�AJ��AJI�AI�AIK�AF~�AE�ADv�AC\)ABbNAAx�A=A;A;&�A:~�A9��A6�+A5A3�A3dZA2A�A0�RA01'A0ZA/�-A.��A.  A-?}A+��A)t�A'7LA&�yA&Q�A%O�A#�^A"�A!��A!�^A ZA�`A1AƨA�FA��A�A�`A�jA��AA�AC�A/A&�A�A��A��AC�A1AC�Av�A$�A�;A|�A�AE�A7LA�A�`AAG�A
�A
M�A	��A	S�A	�AM�Al�A�Ap�A�DA"�A-A�A��A j@��w@���@���@�j@�E�@��@�-@���@�v�@�|�@�n�@�E�@��@���@�x�@��@�t�@�=q@�v�@�?}@���@�z�@� �@�K�@�x�@�C�@�ȴ@���@�O�@�bN@�l�@�E�@�I�@ӝ�@�{@Гu@�C�@���@�A�@ˮ@�t�@��H@��@�&�@ȼj@��@���@ǝ�@�l�@�"�@Ƈ+@���@�V@î@�l�@�l�@�K�@�n�@��7@���@�Z@�\)@�5?@�hs@�%@�Ĝ@�j@��@��@�J@�?}@���@�9X@�K�@���@��@��7@�hs@�O�@��@�Ĝ@��@��+@��7@��`@��9@��@�1'@�ƨ@�|�@�l�@�;d@���@�ff@���@�G�@��/@�Ĝ@��j@���@�Z@��@��@�^5@�&�@��D@��@�z�@�bN@� �@�K�@���@�n�@�`B@��`@��9@�r�@�A�@�b@���@��@���@�|�@�S�@�33@��@���@�V@�-@�J@��#@�@��^@�V@��@� �@�C�@���@���@��@��;@�1@�b@��@�(�@�1@���@��m@�o@���@��@�ȴ@���@��+@��@��7@�?}@���@�Q�@��m@���@�l�@�+@��@��@��R@���@��@�p�@�?}@�/@�V@���@�Ĝ@��@���@���@��u@�Z@��@��
@���@�l�@��@��@���@�{@�@�/@�z�@��;@���@���@��P@�dZ@�;d@�"�@�ȴ@�n�@�=q@��T@���@�O�@�Ĝ@�I�@�(�@� �@��@��;@�|�@�"�@���@�ȴ@�~�@��@���@���@��@���@��D@�z�@�r�@�bN@�I�@� �@�ƨ@�S�@�"�@�o@��@�o@�
=@�@��H@���@���@��\@�V@�5?@�$�@�@��@�@��7@�?}@��@���@��u@�j@�I�@� �@�33@��@��y@��y@��y@���@���@��+@�v�@�Ɇ@�\�@x��@k�@_�@Y�3@TI�@NR�@D�@=�"@6��@0r�@)�@%O�@�6@ی@��@ �@�<@
p;@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�K�A�`BA�n�A�x�A�t�A�t�A�x�A�v�A�v�A�v�A�x�A�|�A�|�A�|�A�~�A�~�A��A��A��A��A��A��A��A��A�|�A�x�A�x�A�z�A�z�A�|�A��+A��+A��A��7A��DA��DA��PA��+A�~�A�|�A�v�A�r�A��A�VA�bNA��A��A��PA�+A�E�A�\)A��hA�A��^A��HA��-A�p�A�O�A�jA��HA��A���A��A��DA�O�A� �A��A�$�A��TA�\)A��;A�A��hA�"�A��mA��;A��
A��wA���A�x�A���A��FA�r�A�M�A��A��9A���A��A���A��A�ƨA�-A���A��A�E�A���A��A��!A��HA�{A�"�A�A��A���A��A�A��A���A�$�A��A��A~A�A|jAz$�Ax��Ax �Av=qAt{ArjAp�/Am�
Al^5Ak�FAk%Aj�RAj�+AjffAjAioAg��Adn�A`�+A^�+A]�-A]x�A[O�AYVAV�jATbNASO�AR5?AO�AM33AK?}AJ�AJ��AJI�AI�AIK�AF~�AE�ADv�AC\)ABbNAAx�A=A;A;&�A:~�A9��A6�+A5A3�A3dZA2A�A0�RA01'A0ZA/�-A.��A.  A-?}A+��A)t�A'7LA&�yA&Q�A%O�A#�^A"�A!��A!�^A ZA�`A1AƨA�FA��A�A�`A�jA��AA�AC�A/A&�A�A��A��AC�A1AC�Av�A$�A�;A|�A�AE�A7LA�A�`AAG�A
�A
M�A	��A	S�A	�AM�Al�A�Ap�A�DA"�A-A�A��A j@��w@���@���@�j@�E�@��@�-@���@�v�@�|�@�n�@�E�@��@���@�x�@��@�t�@�=q@�v�@�?}@���@�z�@� �@�K�@�x�@�C�@�ȴ@���@�O�@�bN@�l�@�E�@�I�@ӝ�@�{@Гu@�C�@���@�A�@ˮ@�t�@��H@��@�&�@ȼj@��@���@ǝ�@�l�@�"�@Ƈ+@���@�V@î@�l�@�l�@�K�@�n�@��7@���@�Z@�\)@�5?@�hs@�%@�Ĝ@�j@��@��@�J@�?}@���@�9X@�K�@���@��@��7@�hs@�O�@��@�Ĝ@��@��+@��7@��`@��9@��@�1'@�ƨ@�|�@�l�@�;d@���@�ff@���@�G�@��/@�Ĝ@��j@���@�Z@��@��@�^5@�&�@��D@��@�z�@�bN@� �@�K�@���@�n�@�`B@��`@��9@�r�@�A�@�b@���@��@���@�|�@�S�@�33@��@���@�V@�-@�J@��#@�@��^@�V@��@� �@�C�@���@���@��@��;@�1@�b@��@�(�@�1@���@��m@�o@���@��@�ȴ@���@��+@��@��7@�?}@���@�Q�@��m@���@�l�@�+@��@��@��R@���@��@�p�@�?}@�/@�V@���@�Ĝ@��@���@���@��u@�Z@��@��
@���@�l�@��@��@���@�{@�@�/@�z�@��;@���@���@��P@�dZ@�;d@�"�@�ȴ@�n�@�=q@��T@���@�O�@�Ĝ@�I�@�(�@� �@��@��;@�|�@�"�@���@�ȴ@�~�@��@���@���@��@���@��D@�z�@�r�@�bN@�I�@� �@�ƨ@�S�@�"�@�o@��@�o@�
=@�@��H@���@���@��\@�V@�5?@�$�@�@��@�@��7@�?}@��@���@��u@�j@�I�@� �@�33@��@��y@��y@��y@���@���@��+G�O�@�Ɇ@�\�@x��@k�@_�@Y�3@TI�@NR�@D�@=�"@6��@0r�@)�@%O�@�6@ی@��@ �@�<@
p;@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB5?B49B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B33B49B33B49B5?B49B49B49B49B5?B5?B6FB9XB:^B;dB<jBK�B^5BjBm�Bp�Bp�Br�Bx�B~�B�B~�Bz�B|�B{�B{�Bu�Bk�BhsB`BBYBP�BL�BH�BG�B<jB49B0!B+B�BVB+BB��B��B��B��B�B�)B��BB�-B��B�7Bv�Bk�B[#BJ�BI�BH�BA�B<jB8RB33B&�B�BoB+B
��B
�B
�;B
��B
��B
�dB
�3B
�B
��B
��B
�{B
�7B
~�B
u�B
gmB
ZB
VB
J�B
>wB
/B
&�B
oB
B	��B	��B	��B	�B	�B	�B	�mB	�B	ƨB	��B	��B	�=B	�%B	x�B	e`B	XB	L�B	C�B	>wB	33B	#�B	�B	�B	�B	�B	�B	�B	hB	
=B		7B	+B	B��B�B�#B�B��B��B�qB�LB�'B�B��B��B��B��B��B��B��B��B�PB�Br�Bp�Bm�BiyBdZBaHB^5B\)BW
BQ�BN�BL�BK�BK�BJ�BH�BG�BE�BB�B?}B?}B?}B>wB=qB:^B:^B9XB9XB8RB8RB8RB8RB6FB7LB5?B49B33B33B2-B1'B1'B1'B0!B/B0!B-B-B,B+B,B(�B(�B'�B(�B&�B&�B%�B$�B%�B'�B&�B$�B'�B%�B%�B%�B%�B%�B%�B%�B%�B%�B'�B'�B'�B(�B'�B(�B,B-B,B.B.B/B/B1'B2-B33B6FB8RB;dB<jBA�BA�BA�BC�BE�BG�BH�BJ�BK�BK�BL�BM�BO�BQ�BVB[#B[#B[#B[#B`BBe`BhsBl�Bs�Bz�B|�B}�B~�B�B�B�7B��B��B��B��B��B��B�B�B�B�B�B�B�9B�XB�qB��B��B��BBĜBŢBŢBƨBǮB��B��B��B��B��B��B��B��B�B�#B�5B�TB�`B�fB�sB�B�B�B��B��B	B	B	B	%B	1B	
=B	
=B	
=B	JB	oB	�B	�B	�B	�B	#�B	%�B	(�B	,B	.B	.B	8RB	B�B	C�B	B�B	E�B	H�B	L�B	R�B	T�B	VB	VB	XB	YB	YB	YB	]/B	`BB	bNB	cTB	ffB	jB	n�B	n�B	r�B	y�B	{�B	}�B	�B	�B	�B	�%B	�+B	�7B	�bB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�9B	�?B	�?B	�FB	�FB	�RB	�dB	�dB	�qB	�wB	��B	B	ŢB	ŢB	ƨB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�ZB	�`B	�`B	�`B	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	��B
-B
NB
�B
#�B
.IB
4B
:�B
A;B
F�B
N�B
Q�B
V�B
^jB
`�B
f�B
l�B
n�B
t9B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B-1B,+B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B+%B,+B+%B,+B-0B,+B,+B,+B,+B-0B-0B.7B1IB2OB3UB4[BC�BV$BbnBeBh�Bh�Bj�Bp�Bv�B}Bv�Br�Bt�Bs�Bs�Bm�BcvB`dBX4BQ
BH�BD�B@�B?�B4_B,.B(B"�B�BNB�$B��B��B��B��B��B�B�%B��B��B�,B��B�9Bn�Bc�BS)BB�BA�B@�B9�B4rB0ZB+<B�B�B
zB
�7B
��B
�B
�JB
�B
��B
�vB
�EB
�B
��B
��B
��B
�LB
wB
m�B
_�B
R6B
NB
B�B
6�B
'7B
B

�B	�>B	�B	��B	��B	��B	��B	�B	ߏB	�:B	��B	��B	��B	�eB	~NB	p�B	]�B	P<B	D�B	;�B	6�B	+bB	B	�B	�B	�B	�B	�B	�B		�B	pB	jB�^B�EB�.B��B�ZB�GB�/B�B��B��B�aB�NB�7B��B��B�7B�1B�B��B��B��B|WBj�Bh�Be�Ba�B\�BY�BVvBTjBOLBJ.BGBEBD
BD
BCB@�B?�B=�B:�B7�B7�B7�B6�B5�B2�B2�B1�B1�B0�B0�B0�B0�B.�B/�B-�B,B+yB+yB*tB)nB)nB)nB(hB'bB(hB%UB%UB$PB#JB$PB!>B!>B 8B!?B2B2B,B&B,B 9B3B'B :B-B-B-B-B-B-B-B-B-B ;B ;B ;B!AB ;B!AB$SB%YB$SB&_B&_B'fB'fB)rB*xB+~B.�B0�B3�B4�B9�B9�B9�B;�B=�B?�B@�BCBDBDBEBFBH)BJ6BNNBSmBSmBSmBSmBX�B]�B`�Bd�Bk�Bs)Bu6Bv<BwBByNB|aB�~B��B��B��B�B�/B�BB�HB�TB�ZB�ZB�aB�aB�B��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�0B�6B�6B�6B�<B�TB�gB�yBۗBݣBީB�B��B��B��B�B�B�NB�TB�aB�gB	 sB	~B	~B	~B	�B	
�B	�B	�B	�B	�B	B	#B	!6B	$HB	&SB	&SB	0�B	:�B	;�B	:�B	=�B	@�B	EB	K/B	M;B	NAB	NAB	PMB	QTB	QTB	QTB	UlB	XB	Z�B	[�B	^�B	b�B	f�B	f�B	j�B	rB	t"B	v/B	yAB	{MB	}ZB	~`B	fB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�5B	�MB	�`B	�lB	�qB	�qB	�wB	�wB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�;B	�AB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�lB	�lB	�lB	�rB	�rB	�yB	�~B	ڄB	ڄB	ܐB	ݖB	ݖB	ݖB	ݖB	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�]B	�0B	�bB
	�B
�B
%B
&}B
,SB
2�B
9nB
>�B
F�B
JB
O#B
V�B
YB
^�B
d�B
gB
lkB
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170908    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170908  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170908  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                