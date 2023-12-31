CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:55Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200618141355  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               HA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ظ�	�1   @ظ}'�@6�vȴ9X�c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    HA   B   B   @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D��D�c�D���D��\D�3D�_�D�� D��D��D�IHD���D�׮D��D�W\Dڞ�D��D�"=D�`RD�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�ff@�33A��A=��A]��A}��A���A���A���A���A���A���A���A�  BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBg��BoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚC� CٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9�4C;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCk� Cm� CoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚC�4C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D p D �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf� DgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtp Dy�=D��D�^�D���D��D�fD�Z�D��3D���D��D�D{D���D���D� D�R�Dڙ�D���D�pD�[�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�oA���A���A�dZA�`BA�+A��jA�S�A�bNA��A�VA��A�|�A�/A���A��A���A��9A���A�x�A��;A�\)A���A��A���A���A��uA�x�A�dZA�9XA��HA�O�A�1A��A�Q�A�/A�XA�
=A���A�9XA�33A��A�I�A���A��+A��+A��A�v�A�^5A�E�A�A�A�=qA� �A��yA���A���A�l�A�`BA�Q�A���A��-A�"�A�A���A��jA�S�A��^A���A���A���A�1A�x�A���A��A��A�
=A�^5A��A���A�S�A�ffA��A�n�A�v�A�ȴA�/A�VA��PA�;dA��9A��HA�|�A���A���A��FA�$�A�VA��A��jA�9XA��yA��DA��;A�\)A��A�ffA��+A��7A��A��!A�bNA��A�
A}S�A|��Az�uAy7LAw�-AvA�AuC�AtQ�As�ArI�Apn�Ao�Am�wAlI�Ai��AhȴAfbNAdZAcG�Aa\)A^��A]p�A\��A[��AZ�`AY�AWt�AS`BAQAN��AM;dALI�AJ�AG�AE�AE�AD��ACS�AA�-A@ffA@VA?��A<bNA;"�A:Q�A9t�A6n�A4M�A3\)A2�jA2$�A01'A-�#A,~�A+��A+�A+/A+�A*ȴA*bNA)�A(A�A'oA&5?A%VA#��A#dZA"�A"��A"^5A!�wA �/A�AO�AA�^A�A��A�A�RA�+A�A��A��A�uAVA�yAA��A�A��A-A��A��AjAAt�A33A�/A�uA(�A��AE�A+AĜA �A
�jA
^5A	��A�!A�A��A��AK�A�A�HA��A-A��A�A j@�V@�  @���@��@�A�@��+@�7L@�w@�J@���@�~�@�hs@���@�1'@��@�^@�?}@�V@�Ĝ@�A�@�b@�@�@��H@��@�n�@�1@�\)@�X@�?}@�7L@�V@�b@�\)@��@�5?@�7L@��/@ؓu@��
@ְ!@�{@ԓu@�C�@�^5@ϝ�@�-@��#@̬@��H@�v�@���@��@Ȭ@�b@��T@���@þw@�v�@���@�&�@��u@�ƨ@��@�=q@�-@��T@��@�1@�ƨ@�o@�ȴ@�`B@�+@���@�n�@�E�@�$�@��@�Z@��F@�"�@��w@��F@���@�`B@�I�@��@��@�-@��`@��F@�t�@��@�V@��@���@���@��T@��D@���@��R@�^5@�E�@��T@���@��u@�r�@�Z@�I�@�9X@� �@��
@���@���@���@�@���@�=q@�hs@��@��j@���@��u@�z�@�Z@�9X@�1@��m@��
@��w@���@�t�@�S�@�C�@���@��@�x�@�X@�/@�V@���@�bN@�I�@��@���@�@�=q@���@�O�@���@��9@��u@�j@�(�@��@�  @��@��
@��@�C�@���@�5?@�@��^@��h@��@�Ĝ@�b@��;@��w@���@�l�@�33@�"�@�33@�33@�+@�
=@��@���@��R@��!@��y@�ȴ@��!@��+@�ff@���@��@��R@��+@��@��@��@���@��@��@��@��`@��`@��/@���@�r�@���@��@�K�@�o@���@��@�ȴ@���@���@��+@�ff@�-@�p�@��j@��9@��9@��@���@�1'@�K�@��y@���@�n�@�5?@��#@�G�@���@��9@��@�Z@� �@�  @��
@�ƨ@��P@�33@��@��@��+@�V@��@��@��T@��T@��#@���@��^@���@��@��@|?�@n3�@f+k@]�Z@W��@N�@F��@?��@9�n@3��@0�@-�7@)�9@%�C@ ѷ@&�@�@�D@�@m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A�oA���A���A�dZA�`BA�+A��jA�S�A�bNA��A�VA��A�|�A�/A���A��A���A��9A���A�x�A��;A�\)A���A��A���A���A��uA�x�A�dZA�9XA��HA�O�A�1A��A�Q�A�/A�XA�
=A���A�9XA�33A��A�I�A���A��+A��+A��A�v�A�^5A�E�A�A�A�=qA� �A��yA���A���A�l�A�`BA�Q�A���A��-A�"�A�A���A��jA�S�A��^A���A���A���A�1A�x�A���A��A��A�
=A�^5A��A���A�S�A�ffA��A�n�A�v�A�ȴA�/A�VA��PA�;dA��9A��HA�|�A���A���A��FA�$�A�VA��A��jA�9XA��yA��DA��;A�\)A��A�ffA��+A��7A��A��!A�bNA��A�
A}S�A|��Az�uAy7LAw�-AvA�AuC�AtQ�As�ArI�Apn�Ao�Am�wAlI�Ai��AhȴAfbNAdZAcG�Aa\)A^��A]p�A\��A[��AZ�`AY�AWt�AS`BAQAN��AM;dALI�AJ�AG�AE�AE�AD��ACS�AA�-A@ffA@VA?��A<bNA;"�A:Q�A9t�A6n�A4M�A3\)A2�jA2$�A01'A-�#A,~�A+��A+�A+/A+�A*ȴA*bNA)�A(A�A'oA&5?A%VA#��A#dZA"�A"��A"^5A!�wA �/A�AO�AA�^A�A��A�A�RA�+A�A��A��A�uAVA�yAA��A�A��A-A��A��AjAAt�A33A�/A�uA(�A��AE�A+AĜA �A
�jA
^5A	��A�!A�A��A��AK�A�A�HA��A-A��A�A j@�V@�  @���@��@�A�@��+@�7L@�w@�J@���@�~�@�hs@���@�1'@��@�^@�?}@�V@�Ĝ@�A�@�b@�@�@��H@��@�n�@�1@�\)@�X@�?}@�7L@�V@�b@�\)@��@�5?@�7L@��/@ؓu@��
@ְ!@�{@ԓu@�C�@�^5@ϝ�@�-@��#@̬@��H@�v�@���@��@Ȭ@�b@��T@���@þw@�v�@���@�&�@��u@�ƨ@��@�=q@�-@��T@��@�1@�ƨ@�o@�ȴ@�`B@�+@���@�n�@�E�@�$�@��@�Z@��F@�"�@��w@��F@���@�`B@�I�@��@��@�-@��`@��F@�t�@��@�V@��@���@���@��T@��D@���@��R@�^5@�E�@��T@���@��u@�r�@�Z@�I�@�9X@� �@��
@���@���@���@�@���@�=q@�hs@��@��j@���@��u@�z�@�Z@�9X@�1@��m@��
@��w@���@�t�@�S�@�C�@���@��@�x�@�X@�/@�V@���@�bN@�I�@��@���@�@�=q@���@�O�@���@��9@��u@�j@�(�@��@�  @��@��
@��@�C�@���@�5?@�@��^@��h@��@�Ĝ@�b@��;@��w@���@�l�@�33@�"�@�33@�33@�+@�
=@��@���@��R@��!@��y@�ȴ@��!@��+@�ff@���@��@��R@��+@��@��@��@���@��@��@��@��`@��`@��/@���@�r�@���@��@�K�@�o@���@��@�ȴ@���@���@��+@�ff@�-@�p�@��j@��9@��9@��@���@�1'@�K�@��y@���@�n�@�5?@��#@�G�@���@��9@��@�Z@� �@�  @��
@�ƨ@��P@�33@��@��@��+@�V@��@��@��T@��T@��#@���@��^@���G�O�@��@|?�@n3�@f+k@]�Z@W��@N�@F��@?��@9�n@3��@0�@-�7@)�9@%�C@ ѷ@&�@�@�D@�@m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B��B�B+BT�Be`BgmBjBr�Bt�Bu�Bx�B|�B�B�B�B�+B�=B�JB�VB��B��B��B�B�-B�LB�^B�jB�qB��BŢBɺB�B�
B�B�
B��B��B��B��B��B�5B�;B�TB�mB�yB�yB�yB�yB�sB�sB�sB�sB�mB�fB�`B�NB�HB�BB�)B�
B��BȴBĜBƨB��B�}B��B�PB~�Bn�B`BBD�B&�BPB	7BB  B��B��B�B�HBɺB�?B��B��B�bB�1Bw�Bp�BdZB`BBW
BH�B:^B-B#�B�B{BPB1BB
��B
�B
�/B
��B
ƨB
��B
�bB
�JB
�+B
�B
p�B
_;B
ZB
Q�B
D�B
;dB
-B
%�B
�B
{B

=B	��B	�B	�TB	�/B	��B	ƨB	�^B	��B	��B	��B	|�B	q�B	q�B	{�B	s�B	k�B	[#B	2-B	�B	JB��B�B�;B��B��B�dB�RB�B��B��B��B��B�VB�%B�B|�B|�Bv�Bu�Bs�Bq�Bn�BhsBdZBdZBgmBk�Bl�Bm�Bm�Bm�Bk�BffBgmBdZBcTBbNB`BB`BB_;B]/B\)B[#BYBVBW
BYBYBYBYBYBZB[#B\)B]/B_;BcTB`BB_;B_;B`BB`BB_;B]/B\)B[#B[#B[#BZBYBYBW
BT�BP�BP�BR�BVBW
BXBYB[#B[#B]/B]/B\)B\)B\)B]/BaHB_;BbNBbNBcTBdZBdZBdZBdZBdZBcTBcTBe`BffBffBgmBm�B}�B�=B�=B�DB�JB�VB�VB�VB�VB�VB�VB�PB�VB�1B�B�B�B�B�%B�+B�7B�DB�JB�JB�JB�VB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�FB�FB�RB�RB�^B�dB�jB�jB�jB��B��B��BBBƨB��B��B��B��B��B��B��B�B�)B�TB�B�B�B��B��B��B��B��B��B	  B	JB	{B	�B	�B	�B	{B	hB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	�B	#�B	%�B	'�B	/B	1'B	49B	5?B	5?B	6FB	7LB	8RB	9XB	:^B	;dB	;dB	<jB	>wB	?}B	@�B	B�B	G�B	J�B	K�B	L�B	M�B	N�B	Q�B	R�B	S�B	VB	ZB	`BB	dZB	hsB	k�B	q�B	s�B	t�B	t�B	u�B	u�B	v�B	x�B	x�B	{�B	� B	�B	�B	�%B	�+B	�DB	�VB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�RB	�RB	�dB	�qB	�qB	�qB	�qB	�qB	�qB	�wB	�}B	��B	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�B	��B
�B
B
 �B
(XB
2GB
:B
@4B
GB
L�B
P�B
SB
V�B
[#B
`�B
g�B
o�B
v+B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B�5B�B#:BM4B]�B_�Bb�Bj�Bl�Bm�Bq
Bu#Bz@B}SB}SB_B�qB�~B��B��B��B�B�:B�_B�~B��B��B��B��B��B��B�4B�:B�@B�;B��B��B��B��B�)B�fB�lBۅBߝB�B�B�B�B�B�B�B�BߝBޗBݑB�B�yB�sB�ZB�<B��B��B��B��B��B��B�B��Bw1Bf�BX{B<�B&B�BwB�_B�@B�"B�B��BيB��B��B�6B��B��B�zBpBh�B\�BX�BOVBAB2�B%]B'B�B�B�B �B
�dB
�(B
��B
ՄB
�/B
��B
�B
��B
��B
�B
ybB
iB
W�B
R{B
JKB
<�B
3�B
%pB
EB
B
�B
�B	�:B	��B	ۻB	ՖB	�GB	�B	��B	�aB	�*B	��B	u\B	jB	jB	tVB	l%B	c�B	S�B	*�B	B	�B�GB�B׵B�gB�B��B��B��B�nB�+B�%B�B��B~�B|�BuoBuoBoKBnEBl8Bj-BgB`�B\�B\�B_�Bd	BeBfBfBfBd	B^�B_�B\�B[�BZ�BX�BX�BW�BU�BT�BS�BQ�BN�BO�BQ�BQ�BQ�BQ�BQ�BR�BS�BT�BU�BW�B[�BX�BW�BW�BX�BX�BW�BU�BT�BS�BS�BS�BR�BQ�BQ�BO�BM�BInBInBK{BN�BO�BP�BQ�BS�BS�BU�BU�BT�BT�BT�BU�BY�BW�BZ�BZ�B[�B\�B\�B\�B\�B\�B[�B[�B]�B^�B^�B_�BfBv}B��B��B��B��B��B��B��B��B��B��B��B��B��By�B{�B{�B|�B~�B�B��B��B��B��B��B��B��B�B�)B�;B�GB�rB�fB�ZB�TB�NB�TB�`B�lB�rB�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�0B�IB�UB�UB�UB�UB�mB͆BҥBԱB��B�B�%B�%B�BB�aB�aB�gB�gB�aB��B	�B	 B	%B	B	B	 B		�B		�B	�B	 B	B	B	+B	1B	1B	7B	7B	7B	7B	CB	JB	JB	CB	\B	hB	 tB	'�B	)�B	,�B	-�B	-�B	.�B	/�B	0�B	1�B	2�B	3�B	3�B	4�B	6�B	8 B	9B	;B	@1B	CDB	DJB	EPB	FVB	G\B	JnB	KtB	LzB	N�B	R�B	X�B	\�B	`�B	dB	j+B	l7B	m=B	m=B	nDB	nDB	oJB	qVB	qVB	tgB	x�B	|�B	}�B	~�B	�B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�OB	�nB	�gB	�gB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�+B	�+B	�+B	�0B	�0B	�1B	�1B	�6B	�1B	�=B	�IB	�IB	�IB	�IB	�CB	�IB	�UB	�UB	�[B	�hB	�nB	�tB	�zB	΀B	φB	φB	φB	ЌB	ЌB	ЌB	ЌB	ҙB	ҙB	ӟB	ӟB	ԥB	ԥB	իB	իB	իB	իB	իB	իB	իB	ֱG�O�B	�B	�^B
aB
�B
rB
 �B
*�B
2�B
8�B
?�B
E_B
I]B
K�B
OB
S�B
YpB
`B
hB
n�B
r8B
ue11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200618141355    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141355  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141355  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                