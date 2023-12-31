CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-15T19:16:56Z AOML 3.0 creation; 2016-05-31T19:14:48Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160315191656  20190604094000  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_142                   2C  D   APEX                            5368                            041511                          846 @ם">ą�1   @ם#ps])@4-V�dMG�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�3D�6fD��3D���D�fD�I�D�� D��3D�fD�@ D�|�D�� D�	�D�L�D�|�D�ɚD��D�S3D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCG�4CI�4CKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDti�Dy�3D�fD�1�D�~fD���D��D�D�D��3D��fD��D�;3D�x D��3D��D�H D�x D���D��D�NfD� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�&�A�{A�oA�A���AʾwAʟ�Aʏ\Aʇ+AʅA�~�A�z�A�n�A�S�A��AɸRA�M�A��A�?}A�z�A�ffA�=qA���A�?}A�bA��A�7LA�9XA�Q�A�  A��yAŏ\A�XA�?}A�7LA�/A�&�A��A�{A��TA�M�A�7LA�oA��A�A�ffA�/AA�ȴA��TA�XA�  A��#A�x�A��A���A��`A��hA���A�|�A���A��A�1A��A�|�A��A�ZA��9A��/A�;dA�  A�^5A���A�A�A�A��#A��yA�I�A��A�O�A���A�%A�hsA���A�~�A�ffA�9XA���A�|�A�A�E�A��^A�hsA�ƨA�z�A�1A�M�A�/A��A��A��/A���A�(�A���A���A�|�A���A�1A�M�A?}A~=qA}��A|-Ay�mAx~�AxI�Aw��Av�`Av$�Au�FArbApr�AoVAkl�Ah�\Af��Ae�
Ae`BAc��A^�HAZ�/AW��ATv�AQ��APr�AO|�AN��AL��AK�PAJ�AI+AG%AE;dADM�ABz�AA�;AA&�A@E�A;�A:M�A9`BA8A6(�A4��A3x�A1`BA/�hA-��A,=qA+|�A*�DA* �A)�PA)�A(ZA'��A'"�A&�uA%�A$bNA#G�A"�A"5?A!��A!�;A!�7A!33A $�AG�AVA��AG�A�\A��A�A�AVA�wA�yA�A�^A  A��A1A
�`A
bNA��Av�AVA�9A�#A-A M�@�33@�G�@���@�dZ@��y@���@�^5@�5?@�x�@�33@�\)@�@�Z@�ȴ@�V@�X@���@��/@��;@��H@��H@��@�5?@陚@�j@�dZ@���@�v�@�z�@��H@�5?@��T@�x�@�7L@�/@��/@�Q�@�t�@�hs@���@���@�|�@���@۶F@ڇ+@��@�p�@�Ĝ@׶F@�l�@��H@֗�@�5?@�%@ӕ�@ҏ\@�v�@�M�@ёh@���@̬@�C�@�~�@��@�V@�z�@��@ǥ�@Ɨ�@���@��@�dZ@�@�hs@�hs@�@�M�@�-@���@��@��@��w@�E�@��@�1'@���@��+@�-@�`B@��D@�bN@�1'@���@��@��@�G�@���@��u@�b@��;@��F@�
=@�v�@�{@�p�@��9@�Ĝ@���@�j@���@�o@�5?@���@��@��T@���@��^@�X@�V@���@�%@�?}@��/@�z�@��F@���@��P@�+@�n�@�p�@�V@���@��@��@��@�r�@�S�@�t�@���@�~�@�{@��7@�V@�Q�@�(�@�(�@��
@���@��P@�l�@�\)@�K�@�"�@�|�@�l�@��w@�ƨ@�"�@���@�ff@�5?@�@���@��@�Ĝ@��@���@�z�@�9X@���@���@�K�@�33@��@��R@�=q@��T@�@�p�@��u@��u@�Z@�1@�ƨ@��@�S�@��@���@��@���@�X@�O�@���@�G�@��u@��@�@��H@��@�
=@���@��@��+@�{@��#@�@��#@�hs@�?}@�&�@���@���@�z�@�bN@�I�@�1@��F@��@���@�l�@�S�@�dZ@���@��@�+@��y@�+@�o@��H@���@��!@�ȴ@��R@�v�@�V@�5?@��#@��h@�7L@�%@��@��`@��/@���@��@�I�@�b@��w@���@��\@��\@�J@�7L@���@��D@��@��@��@���@�|�@�o@�
=@���@��@�+@�33@�@���@���@��!@��@��^@�@�V@�~�@�E�@��@���@�p�@�/@��`@�Ĝ@�Z@�ƨ@�K�@�x�@��R@{��@s@j=q@^E�@T(�@M?}@EV@>ȴ@9x�@4��@.�@)��@#��@�R@�!@��@�`@9X@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�-A�&�A�{A�oA�A���AʾwAʟ�Aʏ\Aʇ+AʅA�~�A�z�A�n�A�S�A��AɸRA�M�A��A�?}A�z�A�ffA�=qA���A�?}A�bA��A�7LA�9XA�Q�A�  A��yAŏ\A�XA�?}A�7LA�/A�&�A��A�{A��TA�M�A�7LA�oA��A�A�ffA�/AA�ȴA��TA�XA�  A��#A�x�A��A���A��`A��hA���A�|�A���A��A�1A��A�|�A��A�ZA��9A��/A�;dA�  A�^5A���A�A�A�A��#A��yA�I�A��A�O�A���A�%A�hsA���A�~�A�ffA�9XA���A�|�A�A�E�A��^A�hsA�ƨA�z�A�1A�M�A�/A��A��A��/A���A�(�A���A���A�|�A���A�1A�M�A?}A~=qA}��A|-Ay�mAx~�AxI�Aw��Av�`Av$�Au�FArbApr�AoVAkl�Ah�\Af��Ae�
Ae`BAc��A^�HAZ�/AW��ATv�AQ��APr�AO|�AN��AL��AK�PAJ�AI+AG%AE;dADM�ABz�AA�;AA&�A@E�A;�A:M�A9`BA8A6(�A4��A3x�A1`BA/�hA-��A,=qA+|�A*�DA* �A)�PA)�A(ZA'��A'"�A&�uA%�A$bNA#G�A"�A"5?A!��A!�;A!�7A!33A $�AG�AVA��AG�A�\A��A�A�AVA�wA�yA�A�^A  A��A1A
�`A
bNA��Av�AVA�9A�#A-A M�@�33@�G�@���@�dZ@��y@���@�^5@�5?@�x�@�33@�\)@�@�Z@�ȴ@�V@�X@���@��/@��;@��H@��H@��@�5?@陚@�j@�dZ@���@�v�@�z�@��H@�5?@��T@�x�@�7L@�/@��/@�Q�@�t�@�hs@���@���@�|�@���@۶F@ڇ+@��@�p�@�Ĝ@׶F@�l�@��H@֗�@�5?@�%@ӕ�@ҏ\@�v�@�M�@ёh@���@̬@�C�@�~�@��@�V@�z�@��@ǥ�@Ɨ�@���@��@�dZ@�@�hs@�hs@�@�M�@�-@���@��@��@��w@�E�@��@�1'@���@��+@�-@�`B@��D@�bN@�1'@���@��@��@�G�@���@��u@�b@��;@��F@�
=@�v�@�{@�p�@��9@�Ĝ@���@�j@���@�o@�5?@���@��@��T@���@��^@�X@�V@���@�%@�?}@��/@�z�@��F@���@��P@�+@�n�@�p�@�V@���@��@��@��@�r�@�S�@�t�@���@�~�@�{@��7@�V@�Q�@�(�@�(�@��
@���@��P@�l�@�\)@�K�@�"�@�|�@�l�@��w@�ƨ@�"�@���@�ff@�5?@�@���@��@�Ĝ@��@���@�z�@�9X@���@���@�K�@�33@��@��R@�=q@��T@�@�p�@��u@��u@�Z@�1@�ƨ@��@�S�@��@���@��@���@�X@�O�@���@�G�@��u@��@�@��H@��@�
=@���@��@��+@�{@��#@�@��#@�hs@�?}@�&�@���@���@�z�@�bN@�I�@�1@��F@��@���@�l�@�S�@�dZ@���@��@�+@��y@�+@�o@��H@���@��!@�ȴ@��R@�v�@�V@�5?@��#@��h@�7L@�%@��@��`@��/@���@��@�I�@�b@��w@���@��\@��\@�J@�7L@���@��D@��@��@��@���@�|�@�o@�
=@���@��@�+@�33@�@���@���@��!@��@��^@�@�V@�~�@�E�@��@���@�p�@�/@��`@�Ĝ@�Z@�ƨG�O�@�x�@��R@{��@s@j=q@^E�@T(�@M?}@EV@>ȴ@9x�@4��@.�@)��@#��@�R@�!@��@�`@9X@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
�{B
�{B
�uB
�uB
�uB
�hB
�hB
�oB
�oB
�oB
�uB
�{B
��B
��B
�B
B
�TBbBXB�hB��B�FB��B�/B��B��BB	7B{B<jB5?B;dBJ�BN�BO�BO�BO�BN�BP�BbNB�DB�oB��B��B��B�}B��B�BB\B�B�B�B�BoBJBJBbBoB{BhB1BB��B��B��B��B�B�B�BB�B��B��BĜB�wB�FB��B��B�JB{�Bt�Bl�BaHBZBR�BB�B.B&�B"�B�BDB�B��B��B�RB��B�Bm�B`BB]/BZBB�B�B%B
��B
�BB
��B
ĜB
�^B
�-B
�B
��B
��B
�%B
z�B
w�B
s�B
l�B
ffB
_;B
F�B
;dB
/B
�B
%B	��B	�B	�sB	�B	��B	�7B	v�B	`BB	L�B	B�B	<jB	5?B	+B	"�B	�B	�B	JB	B��B��B��B�B�yB�;B�B��B��BɺBŢB��B�qB�^B�XB�RB�FB�?B�3B�3B�-B�'B�!B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�Bu�Bv�Bx�Bx�Bp�BdZB`BB^5BbNBbNB`BB^5BaHBdZBhsBk�Bn�Bn�Br�Br�Bp�Bk�BhsBe`BffBe`Be`BhsBhsBhsBiyBo�By�By�Bx�By�Bz�Bz�Bz�Bz�B|�B|�B}�B}�B~�B~�B� B�B�B�B�B� B�+B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B��B��B��B��B��B��B��B��B��B�B�B�B�?B�wBBȴB��B��B��B��B��B��B��B�
B�B�#B�#B�#B�B�B�B�B�B�
B�B�)B�ZB�sB�B�B�B�B�B�B�B��B��B��B��B��B	B	+B		7B	DB	JB	PB	VB	\B	oB	�B	�B	�B	�B	�B	�B	"�B	#�B	$�B	%�B	&�B	)�B	,B	.B	.B	-B	.B	0!B	49B	6FB	8RB	:^B	=qB	<jB	>wB	E�B	F�B	E�B	J�B	L�B	O�B	Q�B	R�B	S�B	[#B	_;B	dZB	e`B	e`B	iyB	k�B	l�B	l�B	l�B	m�B	n�B	r�B	u�B	u�B	v�B	v�B	v�B	z�B	~�B	�B	�B	�+B	�7B	�7B	�=B	�PB	�JB	�JB	�PB	�VB	�\B	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�9B	�?B	�FB	�LB	�jB	�wB	�}B	��B	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	�
B	�
B	�B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�HB	�TB	�TB	�HB	�BB	�BB	�HB	�NB	�NB	�ZB	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
�B
"�B
)�B
1'B
:^B
?}B
C�B
I�B
O�B
VB
\)B
aHB
e`B
jB
n�B
r�B
w�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
��B
�}B
�|B
��B
��B
��B
��B
��B
��B
��B
�!B
àB
�eBpBY&B�{B��B�WB��B�DB�
B��BB
JB�B=}B6VB<xBK�BO�BP�BP�BP�BO�BQ�Bc`B�[B��B��B��B�B��B��B�BBsB�B�B�B�B�BcBcByB�B�B~B	HB%B B�B��B��B��B�B�ZB�B��B��BŶB��B�]B��B��B�^B|�Bu�Bm�BbZB[2BT	BC�B/,B(B#�B�BXB��B��BB�eB��B�0Bn�BaXB^AB[/BC�B�B<B
��B
�WB
��B
ŰB
�nB
�AB
�B
��B
��B
�:B
{�B
x�B
t�B
m�B
gwB
`MB
G�B
<uB
0,B
�B
5B	��B	�B	�B	�"B	� B	�GB	w�B	aQB	M�B	C�B	=yB	6MB	,B	#�B	�B	�B	[B	)B�	B��B��B�B�B�GB�!B�B��B��BưBB��B�nB�gB�_B�YB�PB�FB�EB�?B�4B�0B�2B�)B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�vB�eB�$Bv�Bw�By�By�Bq�BejBaSB_FBc^BcZBaRB_DBbXBeiBi�Bl�Bo�Bo�Bs�Bs�Bq�Bl�Bi�BfrBgyBfpBfqBi�Bi�Bi�Bj�Bp�Bz�Bz�By�Bz�B{�B{�B{�B{�B}�B}�BBB�B�B�B�$B�%B�#B�B�B�9B��B��B��B��B��B��B��B��B��B��B��B�B�B� B�1B�%B�*B�7B�	B��B��B��B��B��B��B��B�	B�B�B�,B�MB��BßB��B��B��B��B��B��B�B�B�B�*B�2B�5B�7B�-B�-B�+B�(B� B�B�%B�=B�lB�B�B�B�B�B�B�B�B��B��B��B��B�B	*B	;B	
HB	WB	[B	`B	jB	nB	B	�B	�B	�B	�B	�B	�B	#�B	$�B	%�B	&�B	'�B	+B	-B	/)B	/(B	."B	/(B	15B	5KB	7ZB	9bB	;oB	>�B	=~B	?�B	F�B	G�B	F�B	K�B	M�B	P�B	R�B	TB	UB	\6B	`LB	ekB	fqB	frB	j�B	l�B	m�B	m�B	m�B	n�B	o�B	s�B	v�B	v�B	w�B	w�B	w�B	{�B	�
B	�B	�/B	�<B	�HB	�LB	�MB	�dB	�`B	�^B	�cB	�hB	�rB	�}B	�yB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�>B	�?B	�NB	�SB	�XB	�_B	�B	��B	��B	B	ųB	ƶB	ƴB	ǾB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�0B	�@B	�DB	�AB	�BB	�HB	�QB	�QB	�SB	�\B	�YB	�ZB	�UB	�VB	�VB	�SB	�VB	�YB	�UB	�[B	�\B	�^B	�`B	�[B	�gB	�iB	�WB	�TB	�RB	�ZB	�`B	�ZB	�jB	�uB	�vB	�wB	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
oB
�B
#�B
+B
28B
;sB
@�B
D�B
J�B
P�B
WB
]=B
b]B
fuB
k�B
o�B
s�B
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.15 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0.001(+/-0.002) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940002019060409400020190604094000  AO  ARCAADJP                                                                    20160315191656    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160315191656  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160315191656  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094000  IP                  G�O�G�O�G�O�                