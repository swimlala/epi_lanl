CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:43Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               EA   AO  20111205113541  20190522121836  1901_5055_069                   2C  D   APEX                            2140                            040306                          846 @�ԣ�y�1   @�Ԥu�@ @,\�1&��c��1'1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B��B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bw��B�  B�  B���B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B���C   C  C�C  C�fC
  C  C  C  C  C�fC�fC�fC  C  C  C�fC"  C$�C&  C(  C)�fC,  C.�C/�fC2  C4  C6  C7�fC9�fC<  C>�C@�CB�CD�CF�CH�CJ  CL  CN  CP  CQ�fCT  CV  CX  CY�fC[�fC]�fC`  Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~  C�fC�  C��C�  C�  C��3C��3C��C�  C�  C��C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C��C��C��C��C��C��C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C��C��C�  C��C��3C��3C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C��C�  C��3C�  C��C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C��C�  C�  C��3D   D � D ��D� DfD� D  D� D��D� DfD� D��D� DfD�fDfD� D	  D	y�D
  D
� D  D� D  D� DfD� D  D�fD  Dy�D  D�fD  D� D  D� DfD� D��Dy�D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D� DfD� D  D�fD  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,y�D,��D-y�D-��D.y�D.��D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4fD4�fD5fD5�fD6fD6�fD7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DCy�DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL�fDM  DM� DN  DN� DN��DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDVfDV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[y�D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDbfDb� Db��Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� DpfDp� Dp��Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dvs3Dyy�D�  D�L�D�s3D���D��3D�#3D�VfD��3D��3D�9�D�` D�� D��3D�3D�9�Dਗ਼D���D�,�D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@s33@���@�  AffA6ffAVffAvffA�33A�33A�33A�33A�33A�33A�33A�ffB33B33B��B��B%��B-��B5��B=��BE��BM��BU��B]��Be��Bm33Bu33B}��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bҙ�B���B���B���B�  B���B���B���B���B���B���B���CffC� CffCL�C	ffCffCffCffCffCL�CL�CL�CffCffCffCL�C!ffC#� C%ffC'ffC)L�C+ffC-� C/L�C1ffC3ffC5ffC7L�C9L�C;ffC=� C?� CA� CC� CE� CG� CIffCKffCMffCOffCQL�CSffCUffCWffCYL�C[L�C]L�C_ffCa� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}ffCL�C��3C�� C��3C��3C��fC��fC�� C��3C��3C�� C��3C��3C��3C�� C��3C��fC��fC��fC��3C��3C�� C�� C�� C�� C�� C�� C��3C��3C��fC��3C��3C��fC��3C��3C��fC��fC��fC��fC��3C��3C��3C��3C��3C��3C��3C�� C��3C��3C��3C�� C�� C��3C��fC��3C�� C��3C��fC��3C�� C��3C��fC��3C��3C��3C��fC��3C³3CæfCĳ3Cų3CƦfC�� C�� Cɳ3C�� C˦fC̦fC�� Cγ3Cϳ3C�� Cѳ3CҦfCӳ3CԳ3Cճ3C�� C׳3Cس3Cٳ3CڦfC۳3Cܳ3CݦfCަfC߳3C�3C�� C�3C�fC�3C�� C�3C�3C�fC�3C�3C�fC�3C�� C�� C�3C�3C�3C�3C�fC��3C�� C��3C��fC��3C��3C��3C�� C��3C��3C��fC��3D Y�D �3DY�D� DY�DٚDY�D�3DY�D� DY�D�3DY�D� D` D� DY�DٚD	S3D	ٚD
Y�D
ٚDY�DٚDY�D� DY�DٚD` DٚDS3DٚD` DٚDY�DٚDY�D� DY�D�3DS3DٚDY�DٚDY�DٚDY�DٚDY�DٚD` D� D` DٚDY�DٚDY�D� DY�DٚD` DٚDY�DٚD Y�D ٚD!` D!ٚD"Y�D"ٚD#Y�D#ٚD$S3D$ٚD%Y�D%ٚD&Y�D&� D'Y�D'ٚD(Y�D(ٚD)Y�D)�3D*Y�D*ٚD+Y�D+ٚD,S3D,�3D-S3D-�3D.S3D.�3D/S3D/ٚD0Y�D0ٚD1Y�D1ٚD2Y�D2ٚD3Y�D3� D4` D4� D5` D5� D6` D6� D7Y�D7ٚD8Y�D8ٚD9Y�D9ٚD:Y�D:ٚD;Y�D;ٚD<Y�D<� D=Y�D=ٚD>Y�D>ٚD?Y�D?ٚD@Y�D@ٚDAY�DAٚDB` DBٚDCS3DCٚDDY�DDٚDEY�DEٚDF` DFٚDGY�DGٚDHY�DHٚDIY�DIٚDJY�DJٚDKY�DK� DL` DLٚDMY�DMٚDNY�DN�3DOY�DO� DPY�DPٚDQY�DQٚDRY�DRٚDSY�DSٚDTY�DTٚDU` DU� DVY�DVٚDWY�DWٚDXY�DXٚDY` DYٚDZY�DZٚD[S3D[�3D\S3D\ٚD]Y�D]ٚD^Y�D^ٚD_Y�D_ٚD`Y�D`ٚDa` Da� DbY�Db�3DcY�Dc� DdY�DdٚDeY�DeٚDfY�DfٚDgY�DgٚDhY�DhٚDiY�DiٚDjY�Dj�3DkY�DkٚDl` DlٚDmY�DmٚDnY�DnٚDoY�Do� DpY�Dp�3DqY�DqٚDrY�DrٚDsY�Ds�3DtY�DtٚDuY�DuٚDvL�DyS3D���D�9�D�` D���D�� D� D�C3D�� D�� D�&fD�L�DǬ�D�� D�  D�&fD��fD�ٚD��D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AȓuAȟ�Aȟ�Aȡ�Aȟ�Aȡ�Aȝ�Aȟ�Aȟ�Aȝ�Aț�Aȝ�Aȡ�Aȟ�Aȡ�Aȣ�AȲ-AȲ-AȲ-AȲ-Aȴ9Aȴ9AȶFAȶFAȴ9Aȧ�Aȝ�Aȗ�AȋDA�p�A�dZA�  Aư!A�A�A�{A���AŶFA�|�A�Q�A�-A�%A��`A���Aħ�Aę�Aĉ7A�\)A�+A�JAò-AÝ�AÛ�A� �A���A�O�A�p�A�9XA���A��A�oA�=qA�v�A�hsA�/A�~�A���A��A�r�A�x�A�A��wA�^5A�{A��-A�E�A�oA���A��jA���A�&�A�n�A���A���A��PA�O�A�K�A�1A���A���A�n�A�C�A�Q�A���A|~�Avv�AtAr�Ar��Ao�Ah��Aax�A]�wA\�AZz�AX��AT^5AO�^AMl�ALAH��AC7LA@VA@�A>�A<-A9�FA8ĜA8bNA7ƨA6��A49XA2��A2bA0{A-��A-%A+��A(�yA'�A'O�A'��A'��A(ZA'��A({A)%A(1'A& �A%`BA%\)A%�A$9XA#+A"�A"��A"��A"�HA$bA$�A#�A ZA 5?AȴA�yA^5A��A��Av�A�jA�TAO�AC�A;dA?}Ax�A��AdZA9XA��AZAhsAO�AXA&�A��A�;A�A��A��A�!A(�AffA��A��A�A�A�A9XA�A�A�`A��A�
AhsA�RAr�An�A;dA
5?A
jA
ZA	�A��A��AffAffAbNA�-AK�A�A�\A^5AQ�A1A�
A�7A&�Ar�A5?AbA|�A;dA33A�A��A&�A��A1'A��A�wA��A��A7LA �uA 5?@�;d@��!@��@���@��m@���@��-@���@�  @��@�J@�/@�  @�R@�5?@�7@���@��D@�j@�o@�!@��@�G�@��@�t�@�C�@���@�V@��#@�h@�/@��@�Z@�
=@��@�@��@��@��@�@�z�@��;@���@�+@�$�@ᙚ@�`B@��@���@�j@� �@ߕ�@޸R@�~�@�-@�7L@ܓu@�ƨ@�\)@�
=@ڗ�@�@ش9@�1@���@֧�@�J@�`B@Ԭ@�1'@�ƨ@�@ҏ\@���@д9@���@ϥ�@�+@��y@Ώ\@��#@�O�@��`@̃@�(�@�"�@ʗ�@��@�`B@�V@ț�@�I�@��
@�|�@���@Ƈ+@�@�/@��`@�j@�  @��;@���@î@å�@�|�@��@���@���@��@��@�Q�@�b@��;@��@�\)@��@�~�@�^5@�=q@���@�p�@��@�z�@��;@�t�@��@���@��\@��@���@�p�@��@��9@� �@�ƨ@��P@�K�@���@���@���@�hs@�V@���@�A�@��w@�@��+@�=q@�@�?}@�%@���@�A�@��F@�o@�ȴ@���@�$�@��#@��@�hs@��@���@� �@�ƨ@���@�;d@��y@���@�ff@�5?@��@���@�?}@��/@�r�@��
@�l�@��@��R@��@��-@��h@�p�@�&�@���@��/@��@�Q�@��;@���@�o@��R@�{@�7L@��@��9@���@�I�@���@�o@��+@��R@���@���@�~�@�5?@��@���@���@��@��@���@���@���@�r�@��@��;@�|�@��@�n�@�~�@�M�@��@��@�&�@�%@���@�I�@�  @��w@�t�@��@�o@��y@���@�ff@�5?@��@���@�hs@�&�@�%@��`@��j@��@��m@���@�|�@�l�@�\)@���@�ff@�$�@���@��-@��9@���@�b@~��@t�D@k"�@d(�@[33@S��@L�@D1@<�@4j@-p�@(A�@"n�@V@�w@t�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AȓuAȟ�Aȟ�Aȡ�Aȟ�Aȡ�Aȝ�Aȟ�Aȟ�Aȝ�Aț�Aȝ�Aȡ�Aȟ�Aȡ�Aȣ�AȲ-AȲ-AȲ-AȲ-Aȴ9Aȴ9AȶFAȶFAȴ9Aȧ�Aȝ�Aȗ�AȋDA�p�A�dZA�  Aư!A�A�A�{A���AŶFA�|�A�Q�A�-A�%A��`A���Aħ�Aę�Aĉ7A�\)A�+A�JAò-AÝ�AÛ�A� �A���A�O�A�p�A�9XA���A��A�oA�=qA�v�A�hsA�/A�~�A���A��A�r�A�x�A�A��wA�^5A�{A��-A�E�A�oA���A��jA���A�&�A�n�A���A���A��PA�O�A�K�A�1A���A���A�n�A�C�A�Q�A���A|~�Avv�AtAr�Ar��Ao�Ah��Aax�A]�wA\�AZz�AX��AT^5AO�^AMl�ALAH��AC7LA@VA@�A>�A<-A9�FA8ĜA8bNA7ƨA6��A49XA2��A2bA0{A-��A-%A+��A(�yA'�A'O�A'��A'��A(ZA'��A({A)%A(1'A& �A%`BA%\)A%�A$9XA#+A"�A"��A"��A"�HA$bA$�A#�A ZA 5?AȴA�yA^5A��A��Av�A�jA�TAO�AC�A;dA?}Ax�A��AdZA9XA��AZAhsAO�AXA&�A��A�;A�A��A��A�!A(�AffA��A��A�A�A�A9XA�A�A�`A��A�
AhsA�RAr�An�A;dA
5?A
jA
ZA	�A��A��AffAffAbNA�-AK�A�A�\A^5AQ�A1A�
A�7A&�Ar�A5?AbA|�A;dA33A�A��A&�A��A1'A��A�wA��A��A7LA �uA 5?@�;d@��!@��@���@��m@���@��-@���@�  @��@�J@�/@�  @�R@�5?@�7@���@��D@�j@�o@�!@��@�G�@��@�t�@�C�@���@�V@��#@�h@�/@��@�Z@�
=@��@�@��@��@��@�@�z�@��;@���@�+@�$�@ᙚ@�`B@��@���@�j@� �@ߕ�@޸R@�~�@�-@�7L@ܓu@�ƨ@�\)@�
=@ڗ�@�@ش9@�1@���@֧�@�J@�`B@Ԭ@�1'@�ƨ@�@ҏ\@���@д9@���@ϥ�@�+@��y@Ώ\@��#@�O�@��`@̃@�(�@�"�@ʗ�@��@�`B@�V@ț�@�I�@��
@�|�@���@Ƈ+@�@�/@��`@�j@�  @��;@���@î@å�@�|�@��@���@���@��@��@�Q�@�b@��;@��@�\)@��@�~�@�^5@�=q@���@�p�@��@�z�@��;@�t�@��@���@��\@��@���@�p�@��@��9@� �@�ƨ@��P@�K�@���@���@���@�hs@�V@���@�A�@��w@�@��+@�=q@�@�?}@�%@���@�A�@��F@�o@�ȴ@���@�$�@��#@��@�hs@��@���@� �@�ƨ@���@�;d@��y@���@�ff@�5?@��@���@�?}@��/@�r�@��
@�l�@��@��R@��@��-@��h@�p�@�&�@���@��/@��@�Q�@��;@���@�o@��R@�{@�7L@��@��9@���@�I�@���@�o@��+@��R@���@���@�~�@�5?@��@���@���@��@��@���@���@���@�r�@��@��;@�|�@��@�n�@�~�@�M�@��@��@�&�@�%@���@�I�@�  @��w@�t�@��@�o@��y@���@�ff@�5?@��@���@�hs@�&�@�%@��`@��j@��@��m@���@�|�@�l�@�\)@���@�ff@�$�@���@��-@��9@���@�b@~��@t�D@k"�@d(�@[33@S��@L�@D1@<�@4j@-p�@(A�@"n�@V@�w@t�@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
w�B
w�B
w�B
x�B
w�B
u�B
m�B
`BB
`BB
]/B
\)B
T�B
O�B
M�B
H�B
D�B
A�B
?}B
<jB
;dB
9XB
6FB
33B
49B
/B
-B
+B
,B
�B
B
B
�B
=qB
�hB
��B
�5B
�B
��B
��B
��B
��B
��B
�B
�mB
�mB
�B
�B
�5B
ǮB
��B
�wB
�wB
��B
�}B
��B
ȴB
��B
��B
��B
�ZB
��B
�9B
�B
9XB

=B	��B	�HB	ŢB	�B	��B	��B	�uB	�=B	s�B	L�B	1'B	�B	uB		7B��B�B�B��BĜB��B��B��BȴB��B�jB�dB�dB�wB�qB�XB�RB�RB�dB�qB�^B�LB�?B�XB�dBÖB��B�fB	B	B	%B	49B	<jB	0!B	,B	6FB	;dB	<jB	@�B	C�B	XB	iyB	w�B	��B	�B	�^B	��B	��B	��B	�{B	��B	�'B	�dB	B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	ƨB	ȴB	��B	��B	��B	��B	ȴB	ȴB	��B	��B	��B	�NB	�TB	�mB	�mB	�fB	�`B	�TB	�B	��B	��B	�mB	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	��B
B
B
%B
+B
JB
JB
JB

=B
DB
JB
DB
	7B
	7B

=B

=B
	7B
DB
DB
bB
{B
uB
bB
hB
uB
�B
�B
uB
bB
bB
VB
PB
PB
JB
DB
DB
DB
DB

=B
1B
1B
+B
B
B
B
B
+B
1B
+B
B
B
B	��B	��B	��B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB

=B
	7B
	7B
DB
VB
VB
VB
\B
\B
VB
VB
VB
\B
\B
bB
bB
\B
bB
oB
oB
oB
oB
oB
hB
bB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
.B
49B
<jB
A�B
G�B
K�B
Q�B
W
B
\)B
_;B
e`B
iyB
o�B
s�B
w�B
{�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
u�B
v�B
x�B
w�B
x�B
y�B
x�B
y�B
w�B
cTB
bNB
^5B
^5B
W
B
P�B
N�B
I�B
E�B
B�B
@�B
=qB
<jB
;dB
8RB
49B
7LB
0!B
.B
0!B
7LB
1'B
	7B
JB
%�B
O�B
��B
�5B
�BPBB%BoBuBB
��B
��B  B
��B
��B
�B
��B
ȴB
��B
�)B
�NB
�B
��B
�B
�B
�'B1B
�B
�ZB
�B
�B
[#B
�B
B	��B	�NB	��B	�!B	��B	��B	��B	��B	x�B	F�B	'�B	#�B	�B	�B	1B�sB�/B�;B�B�5B��B�B��B��BB��BŢBɺBɺBBB��B��BĜBŢBǮB��B�jB��B��B�`B	B	  B	B	=qB	I�B	5?B	-B	9XB	D�B	C�B	C�B	B�B	W
B	gmB	r�B	��B	�9B	��B	�B	�3B	�B	��B	��B	�!B	�wB	ÖB	�B	�
B	��B	��B	��B	��B	��B	�)B	�TB	�#B	��B	��B	ǮB	ɺB	��B	�B	�B	�B	��B	��B	�B	��B	��B	�fB	�`B	�yB	�B	�B	�B	�B	�NB	��B	��B	�sB	�B	��B	�B	�B	��B	�B	�sB	�B	��B	��B	�B	�B	�B
  B
	7B
%B
	7B
	7B
VB
PB
VB
JB
VB
bB
bB
DB
DB
VB
JB

=B
JB
JB
hB
�B
�B
oB
uB
{B
�B
�B
�B
{B
{B
hB
hB
hB
hB
bB
\B
\B
\B
VB
JB
JB
JB
	7B
+B
1B
1B
	7B

=B
JB
B
B
B
B	��B	��B
B
%B
B
B
B
%B
B
%B
  B	��B	��B
  B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
  B
  B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
%B
+B
%B
+B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
	7B

=B
JB
PB
PB
PB
DB
DB
	7B
DB
JB
\B
\B
\B
bB
bB
\B
\B
bB
\B
\B
oB
oB
oB
hB
uB
uB
oB
{B
�B
uB
bB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
.B
49B
=qB
A�B
G�B
K�B
Q�B
W
B
\)B
`BB
ffB
jB
o�B
s�B
w�B
{�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<���<#�
<49X<u<�t�<T��<D��<�1<�/<D��<49X<�j<�h<u<T��<���<ě�<�o<u<T��<#�
<#�
<�o<�h=o<ě�<��=+<D��<#�
<u<e`B<�1=o=0 �=o<u<u<�j<�`B<��<e`B<#�
<#�
<�`B='�=,1<��
<#�
<�o<�o<�<�`B<�o<�o<���=o<u<#�
<D��<���<u<#�
<#�
<#�
<49X<�o<#�
<#�
<u<e`B<#�
<e`B<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.6 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250312012011312503120120113125031  AO  ARGQ                                                                        20111205113541  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113541  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125031  IP                  G�O�G�O�G�O�                