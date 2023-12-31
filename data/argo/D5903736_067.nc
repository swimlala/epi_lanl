CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:41Z AOML 3.0 creation; 2016-05-31T19:14:35Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230541  20160531121435  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               CA   AO  4051_7090_067                   2C  D   APEX                            5368                            041511                          846 @����1   @���̀@4@ě��T�eƧ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    CA   A   A   @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D��3D�FfD�y�D���D��D�Y�D�FfD�� D� D�C3D���D�ɚD�	�D�C3Dڌ�D๚D�	�D�6fD�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@�33@�  A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	�4CٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD��D	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"|�D"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDyvfD��fD�A�D�t�D���D� D�T�D�A�D��3D�3D�>fD���D���D��D�>fDڈ D��D��D�1�D�nfD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�K�A�M�A�hsA�l�A�n�A�l�A�l�A�jA�n�A�n�A�p�A�n�A�p�A�n�A�n�A�n�A�jA�l�A�z�A�r�A�jA�bNA�`BA�XA�;dA�%A�A���A��A���A���A�~�A�?}A��A���A��HA���A��\A�~�A�v�A�n�A�^5A�M�A�?}A�"�A�VA�1A�A��A��A��#A���A��A���A���A�1'A�-A���A���A�JA���A��A�t�A���A�M�A�oA��wA�-A�"�A���A��A�ffA��A�C�A�/A�p�A�A��A���A�VA��A�|�A�M�A�5?A�t�A�|�A���A�ĜA�{A�G�A���A�z�A�;dA�O�A��`A���A�x�A��+A�{A���A�(�A�l�A�A���A�(�A�^5A��AXA~��A}dZA|��A|�DA|��A|1'Az��Az��AzffAz�Ay��Ay��Ay��AyC�AxE�Aw��Av�Aul�AsAq�Ap$�AoO�Anv�Am��AlA�Ah��Af�yAe"�Ab�A_��A\��A[�AX��AV~�ATjAShsARr�AQ��AP��AO�-AM��AL��ALQ�AK�AJ�`AI;dAF$�AC�^AB�A?�A=
=A<A�A;�A;�wA;?}A:�RA:VA9ƨA7��A3��A1�;A0�DA/�A.��A-�TA+�#A*jA)��A)�#A(�yA'��A'%A&ĜA&��A&ffA&E�A%l�A$=qA#?}A!�;A!K�A ffA&�A��AĜA$�AA?}A��A\)A�jA��AoA��A�HA��Av�AM�A�A�^AO�AA
��A
ffA
�A	��A	�PA	33Ar�AA�A�A��A(�A�hAx�At�At�Ap�AXAVAK�A��An�AE�A(�A�At�A ~�@�M�@� �@�;d@�=q@�A�@��y@�v�@��#@��@�"�@�t�@�^@�9X@�F@�dZ@���@噚@���@���@��@�+@�~�@��y@��
@ՙ�@�(�@��H@�=q@���@� �@�S�@ͺ^@�dZ@ɩ�@�p�@�7L@���@ȴ9@�bN@�(�@�b@��;@Ǯ@�ȴ@ź^@Ł@�x�@�p�@�X@��@ļj@�+@�$�@�V@���@��/@�j@��@��@�@�^5@��T@��h@�`B@�7L@��D@�"�@�=q@��@���@�|�@�t�@�l�@�S�@�C�@�"�@��H@�M�@�I�@��w@��@�C�@�v�@�{@��@���@�p�@�V@��D@��@�I�@��;@�@�&�@�&�@�/@���@���@��@��F@���@���@���@��@���@��P@���@��@�l�@�+@�o@�@��H@���@��\@�ff@�@��^@��h@�p�@�%@�r�@���@���@�dZ@���@�$�@��T@���@��-@��-@��^@��-@��^@���@��#@��@���@�-@�$�@�$�@�5?@�n�@��@���@�^5@��T@�@�/@�%@���@��@�1'@�dZ@�@���@�ȴ@���@�v�@�{@��T@��#@���@���@��-@�`B@�/@��@��@�I�@��@��F@��P@�t�@�K�@��@���@��R@���@�n�@�M�@�J@��#@�@��7@�O�@�7L@�/@��@��@��`@�bN@��@��m@��
@��
@�ƨ@��F@��w@��@�+@�
=@��y@�ȴ@���@���@��R@��!@��+@�5?@��@��T@��-@��7@�hs@�7L@�&�@���@��`@��`@��`@��/@���@���@�Ĝ@��@��w@��P@��H@�ff@�-@��@���@���@���@��@��@��@�t�@�+@��@�n�@��T@�@��h@��@��7@��h@��@�O�@��@��/@��T@���@z^5@t�@m@dI�@[C�@R�@LI�@C�F@;ƨ@4�@+t�@  �@�y@�7@�T@��@r�@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�K�A�M�A�hsA�l�A�n�A�l�A�l�A�jA�n�A�n�A�p�A�n�A�p�A�n�A�n�A�n�A�jA�l�A�z�A�r�A�jA�bNA�`BA�XA�;dA�%A�A���A��A���A���A�~�A�?}A��A���A��HA���A��\A�~�A�v�A�n�A�^5A�M�A�?}A�"�A�VA�1A�A��A��A��#A���A��A���A���A�1'A�-A���A���A�JA���A��A�t�A���A�M�A�oA��wA�-A�"�A���A��A�ffA��A�C�A�/A�p�A�A��A���A�VA��A�|�A�M�A�5?A�t�A�|�A���A�ĜA�{A�G�A���A�z�A�;dA�O�A��`A���A�x�A��+A�{A���A�(�A�l�A�A���A�(�A�^5A��AXA~��A}dZA|��A|�DA|��A|1'Az��Az��AzffAz�Ay��Ay��Ay��AyC�AxE�Aw��Av�Aul�AsAq�Ap$�AoO�Anv�Am��AlA�Ah��Af�yAe"�Ab�A_��A\��A[�AX��AV~�ATjAShsARr�AQ��AP��AO�-AM��AL��ALQ�AK�AJ�`AI;dAF$�AC�^AB�A?�A=
=A<A�A;�A;�wA;?}A:�RA:VA9ƨA7��A3��A1�;A0�DA/�A.��A-�TA+�#A*jA)��A)�#A(�yA'��A'%A&ĜA&��A&ffA&E�A%l�A$=qA#?}A!�;A!K�A ffA&�A��AĜA$�AA?}A��A\)A�jA��AoA��A�HA��Av�AM�A�A�^AO�AA
��A
ffA
�A	��A	�PA	33Ar�AA�A�A��A(�A�hAx�At�At�Ap�AXAVAK�A��An�AE�A(�A�At�A ~�@�M�@� �@�;d@�=q@�A�@��y@�v�@��#@��@�"�@�t�@�^@�9X@�F@�dZ@���@噚@���@���@��@�+@�~�@��y@��
@ՙ�@�(�@��H@�=q@���@� �@�S�@ͺ^@�dZ@ɩ�@�p�@�7L@���@ȴ9@�bN@�(�@�b@��;@Ǯ@�ȴ@ź^@Ł@�x�@�p�@�X@��@ļj@�+@�$�@�V@���@��/@�j@��@��@�@�^5@��T@��h@�`B@�7L@��D@�"�@�=q@��@���@�|�@�t�@�l�@�S�@�C�@�"�@��H@�M�@�I�@��w@��@�C�@�v�@�{@��@���@�p�@�V@��D@��@�I�@��;@�@�&�@�&�@�/@���@���@��@��F@���@���@���@��@���@��P@���@��@�l�@�+@�o@�@��H@���@��\@�ff@�@��^@��h@�p�@�%@�r�@���@���@�dZ@���@�$�@��T@���@��-@��-@��^@��-@��^@���@��#@��@���@�-@�$�@�$�@�5?@�n�@��@���@�^5@��T@�@�/@�%@���@��@�1'@�dZ@�@���@�ȴ@���@�v�@�{@��T@��#@���@���@��-@�`B@�/@��@��@�I�@��@��F@��P@�t�@�K�@��@���@��R@���@�n�@�M�@�J@��#@�@��7@�O�@�7L@�/@��@��@��`@�bN@��@��m@��
@��
@�ƨ@��F@��w@��@�+@�
=@��y@�ȴ@���@���@��R@��!@��+@�5?@��@��T@��-@��7@�hs@�7L@�&�@���@��`@��`@��`@��/@���@���@�Ĝ@��@��w@��P@��H@�ff@�-@��@���@���@���@��@��@��@�t�@�+@��@�n�@��T@�@��h@��@��7@��h@��@�O�@��@��/@��T@���@z^5@t�@m@dI�@[C�@R�@LI�@C�F@;ƨ@4�@+t�@  �@�y@�7@�T@��@r�@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB#�B#�B#�B"�B#�B#�B#�B"�B#�B"�B#�B"�B"�B"�B#�B"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B$�B$�B%�B(�B+B.B/B2-B33B49B5?B5?B33B6FB7LB7LB7LB7LB7LB7LB7LB6FB6FB5?B5?B49B)�BoB	7BB��BŢBz�B`BBO�BG�BA�B)�B\B�B�/B��B�RB�3B�B��B�+B|�Bp�B\)B?}B{B��B�ZB��BŢB�FB�B��B��B�+Bn�BZBD�B49B(�B�B
��B
�B
�HB
��B
�oB
{�B
u�B
p�B
iyB
`BB
ZB
VB
P�B
H�B
E�B
D�B
D�B
A�B
;dB
:^B
7LB
6FB
5?B
49B
2-B
/B
)�B
%�B
�B
�B
JB
B	��B	�B	�B	�fB	�)B	��B	��B	�FB	��B	��B	�%B	{�B	q�B	e`B	]/B	XB	R�B	M�B	I�B	B�B	<jB	8RB	5?B	1'B	,B	#�B	�B	PB	+B��B��B�B�B�B�B�B�B�mB�/B��B��BɺBǮBĜB��B�jB�XB�LB�FB�3B�!B�B�B�B�B��B��B��B��B��B��B��B��B�hB�JB�=B�%B�B~�Bz�Bw�Bs�Bp�Bo�Bo�Bn�Bn�Bm�Bm�Bl�Bk�Bk�BjBjBiyBiyBhsBgmBgmBffBe`BdZBcTBdZBcTBcTBcTBbNBbNBbNBcTBcTBdZBdZBdZBdZB`BBdZBgmBiyBjBjBm�Bo�Bo�Bn�Bk�BffBbNBbNBaHBcTBcTBdZBjBm�Bm�Bl�Bk�Bl�Bm�Bk�Bk�Bk�Bn�Bn�Bp�Bq�Br�Bu�B|�B�B�B�B�B�B�+B�1B�1B�7B�7B�=B�\B�hB�uB�{B��B��B��B��B��B�B�B�B�!B�3B�FB�XB�qB��BBBBŢB��B�
B�;B�B�B�B�B�B�B�B�B�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	\B	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	#�B	$�B	$�B	%�B	&�B	&�B	(�B	)�B	+B	,B	.B	1'B	33B	49B	6FB	<jB	?}B	B�B	D�B	E�B	E�B	F�B	E�B	F�B	F�B	F�B	F�B	G�B	G�B	G�B	G�B	G�B	M�B	S�B	ZB	_;B	cTB	cTB	ffB	gmB	iyB	l�B	o�B	x�B	~�B	�B	�B	�B	�B	�7B	�DB	�DB	�DB	�DB	�JB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�9B	�?B	�?B	�FB	�RB	�^B	�jB	�qB	�qB	�qB	�qB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�BB	�HB	�HB	�TB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
PB
�B
�B
!�B
)�B
2-B
9XB
>wB
D�B
K�B
P�B
YB
aHB
bNB
gmB
l�B
n�B
q�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B#�B#�B#�B"�B#�B#�B#�B"�B#�B"�B#�B"�B"�B"�B#�B"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B$�B$�B%�B)B+B.+B/4B2EB3KB4RB5YB5WB3KB6\B7cB7dB7cB7cB7eB7eB7gB6_B6\B5YB5XB4PB*B�B	MB*B��BŶBz�B`TBO�BG�BA�B*BkB�B�BB��B�bB�EB�B��B�;B|�Bp�B\5B?�B�B��B�hB��BŰB�UB�B��B��B�8Bn�BZ.BD�B4GB)	B�B
��B
�B
�ZB
��B
��B
{�B
u�B
p�B
i�B
`YB
Z1B
VB
P�B
H�B
E�B
D�B
D�B
A�B
;{B
:rB
7bB
6\B
5WB
4OB
2CB
/1B
*B
%�B
�B
�B
bB
B	��B	��B	�B	�B	�BB	��B	��B	�aB	�B	��B	�@B	|B	q�B	e}B	]LB	X/B	SB	M�B	I�B	B�B	<�B	8pB	5_B	1EB	,&B	#�B	�B	rB	LB�B��B��B��B��B�B�B�B�B�SB�B��B��B��B��B��B��B�{B�pB�jB�VB�FB�8B�3B�3B�-B�&B�B��B��B��B��B��B��B��B�pB�cB�MB�4B#B{	Bw�Bs�Bp�Bo�Bo�Bn�Bn�Bm�Bm�Bl�Bk�Bk�Bj�Bj�Bi�Bi�Bh�Bg�Bg�Bf�Be�Bd�Bc|Bd�Bc|Bc}Bc~BbuBbyBbwBc}Bc}Bd�Bd�Bd�Bd�B`kBd�Bg�Bi�Bj�Bj�Bm�Bo�Bo�Bn�Bk�Bf�BbxBbtBaqBc~Bc{Bd�Bj�Bm�Bm�Bl�Bk�Bl�Bm�Bk�Bk�Bk�Bn�Bn�Bp�Bq�Br�Bu�B}B�2B�;B�?B�AB�HB�TB�YB�XB�^B�_B�bB��B��B��B��B��B��B��B��B�
B�3B�3B�;B�IB�XB�kB�|B��B��BµB¶B³B��B�B�0B�^B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B	3B	|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	#�B	$�B	$�B	&B	'B	'B	)B	*B	+#B	,)B	.5B	1GB	3TB	4ZB	6fB	<�B	?�B	B�B	D�B	E�B	E�B	F�B	E�B	F�B	F�B	F�B	F�B	G�B	G�B	G�B	G�B	G�B	M�B	TB	Z;B	_YB	crB	cuB	f�B	g�B	i�B	l�B	o�B	x�B	B	�"B	�"B	�0B	�<B	�UB	�bB	�bB	�bB	�`B	�gB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�)B	�*B	�2B	�.B	�/B	�8B	�=B	�CB	�HB	�UB	�UB	�[B	�YB	�`B	�nB	�zB	��B	��B	��B	��B	��B	��B	��B	ñB	ĺB	žB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�8B	�7B	�IB	�_B	�aB	�dB	�oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B
fB
�B
�B
!�B
*B
2GB
9rB
>�B
D�B
K�B
P�B
Y0B
a_B
bfB
g�B
l�B
n�B
q�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214352016053112143520160531121435  AO  ARCAADJP                                                                    20140721230541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230541  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230541  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121435  IP                  G�O�G�O�G�O�                