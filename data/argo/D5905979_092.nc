CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:15Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170915  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               \A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��З��1   @��[�p@6h�9Xb�c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    \A   B   B   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBXffB`  Bg��Bp  BxffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�_�D���D�ФD��D�K3D���D���D��D�e�D���D��D��D�D{Dڨ D��)D�!HD�Z�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A��A<  A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBG��BO��BW��B_ffBg  BoffBw��BffB�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׀ Bۀ B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)� C+ٚC-ٚC/ٚC1ٚC3ٚC5�4C7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQ� CSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCm�4CoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDp D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD|�D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHp DH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOp DO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDtəDy��D��D�Z�D��)D���D��D�FfD���D��D��D�`�D���D��GD��D�?�Dڣ3D��\D�{D�U�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�C�A�E�A�E�A�E�A�G�A�I�A�K�A�M�A�K�A�I�A�K�A�M�A�O�A�O�A�I�A�I�A�33A��A�A���A���A���A���AֶFA֕�A։7A�n�A�ĜAξwA�A���Aǟ�AŴ9A�VA� �Aã�A�r�A�1A��A�=qA�z�A��uA�ȴA�XA��DA�7LA���A���A��A���A�A�v�A�|�A��A��
A���A��\A�$�A��A��A��A�G�A�
=A���A�I�A�7LA�$�A���A���A�33A��^A��jA�v�A���A�1A��-A��A�z�A���A���A�7LA���A�VA��jA�5?A�(�A�/A�dZA�(�A���A��A�p�A��A�oA�dZA�x�A���A�p�A�A�K�A���A�  A��A�G�A�O�A���A���A�~�A���A�jA��\A��A���A��^A�dZA��PA���A��;A���A��9A��A}�Az��Aw�AuK�Aq�AooAm+AkAehsAbr�A_\)A[�
AY�AW�FAU�7AS�;AS�#ARE�AQXAOXAM�
AN�ANbAM��AL��AI�#AH�/AG�AE�mAC�ABZAA
=A=&�A;x�A:ZA8�A7�;A7x�A6�DA5�hA3�
A2��A1�TA0��A0�A.��A,�A+�wA+hsA+oA+VA*��A*Q�A)�hA)7LA(�A'�7A'7LA&ĜA#�A"�`A"�uA!��A �uA JA;dA��An�A�AĜA�-A�AI�A�-A7LAr�AbA/A�jA��A��A9XA��A"�AĜA��A
=A��A=qA/A �A\)A
z�A	�AĜA��A�A~�AbNA5?A��A��A�
A`BAĜA�AVA �/A ��A ��A =q@���@��T@�;d@��h@�bN@���@��h@��`@�@���@�O�@�33@�=q@�?}@��@��#@�|�@�p�@���@�@߶F@�o@޸R@ݙ�@�A�@�S�@ڧ�@��`@ׅ@֏\@��@�l�@�ff@�=q@�`B@Ѓ@��@��@́@�G�@̃@��
@˝�@�+@�ff@�x�@�j@�l�@��H@�5?@ũ�@�x�@�b@��@�&�@�dZ@�-@��9@���@�Z@��w@�33@�\)@���@�l�@�(�@�bN@�Z@�j@�1@���@��@���@��h@��h@�  @�ff@�+@��\@��7@�E�@�n�@�X@���@��@�7L@�@�"�@��@�~�@�E�@�o@��!@�S�@�@��u@�A�@��j@�O�@�@���@�@��
@���@��@�ff@�S�@�5?@��@���@�O�@��@��D@�j@�j@��@��;@��@���@��@��@���@�x�@���@���@��u@��@�S�@���@��@��@�bN@��
@��@��;@��\@�o@�ȴ@��@��
@���@��/@��@��w@�@�M�@��^@��`@�r�@�bN@�A�@��@�l�@�"�@��y@��!@��\@�{@�`B@�&�@�V@��/@�I�@�1@���@�|�@�"�@��@��H@�~�@�=q@�@���@���@��@���@��-@��-@��^@��@�G�@��@��D@�O�@���@�Z@��@�S�@���@�o@�
=@��@���@��!@�n�@���@��@��@���@��@�Ĝ@��u@�j@�A�@�9X@�9X@�1'@�b@��m@���@��@��@���@�~�@�V@��T@���@��h@�p�@�?}@��@�Ĝ@���@��D@���@��;@��F@���@�C�@���@�ȴ@�n�@�M�@�^5@�5?@�@��@�hs@�&�@���@�r�@�j@�9X@�1@��w@��P@��P@���@���@��F@��
@���@�dZ@�+@�
=@��@�ی@�@x�@p��@ba|@\c�@TN�@O�@H��@@��@:W�@2�@-5�@'�@$�O@{J@~�@��@Y@RT@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�C�A�E�A�E�A�E�A�G�A�I�A�K�A�M�A�K�A�I�A�K�A�M�A�O�A�O�A�I�A�I�A�33A��A�A���A���A���A���AֶFA֕�A։7A�n�A�ĜAξwA�A���Aǟ�AŴ9A�VA� �Aã�A�r�A�1A��A�=qA�z�A��uA�ȴA�XA��DA�7LA���A���A��A���A�A�v�A�|�A��A��
A���A��\A�$�A��A��A��A�G�A�
=A���A�I�A�7LA�$�A���A���A�33A��^A��jA�v�A���A�1A��-A��A�z�A���A���A�7LA���A�VA��jA�5?A�(�A�/A�dZA�(�A���A��A�p�A��A�oA�dZA�x�A���A�p�A�A�K�A���A�  A��A�G�A�O�A���A���A�~�A���A�jA��\A��A���A��^A�dZA��PA���A��;A���A��9A��A}�Az��Aw�AuK�Aq�AooAm+AkAehsAbr�A_\)A[�
AY�AW�FAU�7AS�;AS�#ARE�AQXAOXAM�
AN�ANbAM��AL��AI�#AH�/AG�AE�mAC�ABZAA
=A=&�A;x�A:ZA8�A7�;A7x�A6�DA5�hA3�
A2��A1�TA0��A0�A.��A,�A+�wA+hsA+oA+VA*��A*Q�A)�hA)7LA(�A'�7A'7LA&ĜA#�A"�`A"�uA!��A �uA JA;dA��An�A�AĜA�-A�AI�A�-A7LAr�AbA/A�jA��A��A9XA��A"�AĜA��A
=A��A=qA/A �A\)A
z�A	�AĜA��A�A~�AbNA5?A��A��A�
A`BAĜA�AVA �/A ��A ��A =q@���@��T@�;d@��h@�bN@���@��h@��`@�@���@�O�@�33@�=q@�?}@��@��#@�|�@�p�@���@�@߶F@�o@޸R@ݙ�@�A�@�S�@ڧ�@��`@ׅ@֏\@��@�l�@�ff@�=q@�`B@Ѓ@��@��@́@�G�@̃@��
@˝�@�+@�ff@�x�@�j@�l�@��H@�5?@ũ�@�x�@�b@��@�&�@�dZ@�-@��9@���@�Z@��w@�33@�\)@���@�l�@�(�@�bN@�Z@�j@�1@���@��@���@��h@��h@�  @�ff@�+@��\@��7@�E�@�n�@�X@���@��@�7L@�@�"�@��@�~�@�E�@�o@��!@�S�@�@��u@�A�@��j@�O�@�@���@�@��
@���@��@�ff@�S�@�5?@��@���@�O�@��@��D@�j@�j@��@��;@��@���@��@��@���@�x�@���@���@��u@��@�S�@���@��@��@�bN@��
@��@��;@��\@�o@�ȴ@��@��
@���@��/@��@��w@�@�M�@��^@��`@�r�@�bN@�A�@��@�l�@�"�@��y@��!@��\@�{@�`B@�&�@�V@��/@�I�@�1@���@�|�@�"�@��@��H@�~�@�=q@�@���@���@��@���@��-@��-@��^@��@�G�@��@��D@�O�@���@�Z@��@�S�@���@�o@�
=@��@���@��!@�n�@���@��@��@���@��@�Ĝ@��u@�j@�A�@�9X@�9X@�1'@�b@��m@���@��@��@���@�~�@�V@��T@���@��h@�p�@�?}@��@�Ĝ@���@��D@���@��;@��F@���@�C�@���@�ȴ@�n�@�M�@�^5@�5?@�@��@�hs@�&�@���@�r�@�j@�9X@�1@��w@��P@��P@���@���@��F@��
@���@�dZ@�+@�
=G�O�@�ی@�@x�@p��@ba|@\c�@TN�@O�@H��@@��@:W�@2�@-5�@'�@$�O@{J@~�@��@Y@RT@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
�B
(�B
-B
]/B
�BB%B
�B
��B\B �BVBz�B��B��B�{B��B��B�3B�}BĜB��B�
B�)B�BB{B&�B+B>wBP�BS�BS�BT�BW
BYB[#B\)BaHBcTBgmBm�Bm�Bn�Bp�Bp�Bk�Br�B�7B��B�9B�B��B�oB� B�B~�Br�B\)B;dB�B
=BVBPB+BJBVBPB��B�B��B�?B��B�bB�7B}�B^5BT�BK�B?}B1'B �B�B�B!�B�B
=B
��B
�B
��B
��B
�B
s�B
bNB
A�B
0!B
�B
JB	��B	�ZB	��B	��B	��B	�oB	�%B	u�B	T�B	C�B	33B	�B	�B	JB	uB	bB	�B	�B	
=B	  B	B	�B	�B	�B	oB	+B��B��B�B�mB�BB�B��B��B�wB�FB�!B�B��B��B��B��B��B��B��B��B�bB�JB�PB�VB�VB�PB�JB�7B�1B�%B�B�B�B{�Bv�Bs�Br�Bo�Bl�Bk�BhsBcTB`BB`BB]/B[#BZBW
BVBS�BQ�BP�BO�BP�BO�BI�BF�BI�BG�BG�BD�BC�B@�B?}B<jB=qB<jB:^B9XB49B6FB5?B33B33B49B2-B0!B/B/B/B.B-B-B,B,B+B+B,B+B+B+B,B,B,B,B-B0!B/B0!B1'B49B1'B2-B33B1'B1'B1'B1'B33B2-B2-B33B5?B7LB9XB<jB?}B@�B?}BA�BB�BD�BF�BF�BF�BH�BI�BI�BI�BJ�BL�BM�BN�BN�BP�BP�BO�BS�BS�BVBXBZB^5BaHBdZBffBffBiyBl�Bo�Bs�Bw�Bw�B{�B~�B|�B{�B}�B{�B}�B}�Bw�B|�B�B�B�\B��B��B��B��B�{B��B��B��B��B��B�'B�9B�B�B�B�!B�?B�^B�wBĜBÖB��B��B�)B�)B�B��B��B�B��B��B��B�B�;B�`B�`B�mB�mB�B�B��B	+B	JB	DB		7B	+B	%B	B	B	B	B��B��B	  B	bB	�B	�B	uB	hB	{B	�B	$�B	!�B	 �B	"�B	"�B	#�B	$�B	%�B	'�B	,B	2-B	5?B	6FB	8RB	:^B	<jB	B�B	F�B	G�B	J�B	O�B	Q�B	T�B	ZB	_;B	aHB	bNB	gmB	iyB	k�B	l�B	l�B	l�B	m�B	s�B	v�B	y�B	{�B	� B	�B	�B	�7B	�DB	�DB	�=B	�DB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�qB	�wB	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�ZB	��B
�B
gB
�B
B
&�B
0oB
9>B
="B
E�B
L�B
P�B
V�B
X�B
]�B
b�B
h�B
l�B
pB
v`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
NB
 �B
$�B
T�B
��B
��B
��B
�@B
�TB�BSBM�BrjB�B�!B�B�.B�XB��B�B�!B�wBΎBӭB�B��B�BiB"�B5�BHcBKvBKvBL|BN�BP�BR�BS�BX�BZ�B^�BeBeBfBh!Bh!BcBj-B��B�]B��B��B�B��Bw}Bx�BvwBj/BS�B2�BB�B�B�B��B�B�B�B��B�B�}B��B�MB��B��Bu�BU�BL�BC\B7B(�B]BJBVBcBQB�B
�dB
�!B
ȃB
�gB
|�B
k[B
Y�B
92B
'�B
cB
�B	�~B	�
B	ŅB	�6B	��B	�&B	}�B	m|B	L�B	;SB	*�B	xB	NB	B	6B	$B	yB	IB	�B��B��B	CB	UB	IB	
2B��B��B�B�uB�3B�	B��BɴB�SB�AB�B��B��B��B��B��B�sB�`B�aB�yB�hB�1B�B�B�%B�%B�B�B�B�B}�B{�By�Bx�Bs�Bn�Bk�Bj�BgpBd]BcXB`FB['BXBXBUBR�BQ�BN�BM�BK�BI�BH�BG�BH�BG�BA�B>BA�B?�B?�B<sB;mB8ZB7UB4BB5IB4BB26B11B,B.B-B+B+B,B*B'�B&�B&�B&�B%�B$�B$�B#�B#�B"�B"�B#�B"�B"�B"�B#�B#�B#�B#�B$�B'�B&�B'�B)B,B)B*	B+B)B)B)B)B+B*
B*
B+B-B/)B15B4GB7ZB8`B7ZB9fB:lB<yB>�B>�B>�B@�BA�BA�BA�BB�BD�BE�BF�BF�BH�BH�BG�BK�BK�BM�BO�BQ�BVBY$B\6B^BB^BBaUBdfBgyBk�Bo�Bo�Bs�Bv�Bt�Bs�Bu�Bs�Bu�Bu�Bo�Bt�By�By�B�6B�sB�sB�sB�sB�UB��B��B��B��B��B��B�B��B��B��B��B�B�6B�OB�sB�mB�aBưB��B��B��B��B��B��B��B��B��B��B�B�6B�6B�CB�CB�TB�mB�B��B	B	B	
B��B��B��B��B��B��B��B��B��B	5B	lB	eB	HB		;B	NB	fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	)�B	-B	.B	0#B	2/B	4;B	:`B	>xB	?~B	B�B	G�B	I�B	L�B	Q�B	W
B	YB	ZB	_;B	aGB	cSB	dYB	dYB	dYB	e_B	k�B	n�B	q�B	s�B	w�B	{�B	{�B	�B	�B	�B	�	B	�B	�GB	�SB	�SB	�YB	�_B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�"B	�(B	�.B	�.B	�;B	�AB	�SB	�SB	�XB	�eB	�kB	�kB	�kB	�qB	�wB	��B	ÐB	ĖB	ƢB	ƢB	ȮB	ǨB	ȮB	ȮB	ɵB	ɵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�"B	�B	�tB
-B
[B
�B
�B
(4B
1B
4�B
=fB
D�B
H�B
NeB
P�B
U\B
ZzB
`PB
d�B
g�B
n#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200619170915    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170915  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170915  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                