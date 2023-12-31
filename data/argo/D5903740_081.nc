CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:47Z AOML 3.0 creation; 2016-06-01T00:08:18Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230847  20160531170819  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               QA   AO  4055_7112_081                   2C  D   APEX                            5374                            041511                          846 @��W1�_�1   @��W�?�@:�=p��
�dn��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    QA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyffD���D�I�D�vfD�ٚD���D�6fD�� D���D�	�D�@ D��3D��fD�fD�L�Dډ�D�ٚD���D�FfD�l�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33A��A%��AE��Ae��A���A���A���A���A���A�  A���A�  BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBIffBQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(@ C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDt��Dy|�D�� D�T�D���D���D��D�A�D��3D�� D��D�K3D��fD�љD��D�X Dڔ�D���D� D�Q�D�x D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�ȴA�ĜA�ĜA�ƨA�ƨA�ĜAüjAå�AA��A�(�A�E�A�  A���A�JA���A�bNA���A��+A�A�A�x�A��A�$�A��;A��wA�E�A�v�A��TA�^5A��;A��mA��-A� �A��A�$�A�bA�VA���A��A�ƨA���A�VA��PA�{A��RA��9A��hA��A��hA�S�A���A�$�A��HA���A���A�K�A��
A�ĜA�+A�XA��A���A��-A���A�XA���A���A��FA���A�ZA�"�A�C�A���A��A��A��TA�ȴA�dZA��`A���A��A�7LA���A��A�VA��^A�O�A�n�A�  A�JA� �A��uA��7A��A�E�A~�A|��Ay�PAwK�Av�uAu|�At�At  As/Aq�Ao�
Am�
AkXAh�9Ag%Ae�Ad1'Ac��AbAaC�A`��A`^5A`^5A_S�A^VA\ĜA[�#AZ�jAYdZAX�AW��AU�#ATVAS�PAR�+AP�`AN��AN  AM�hAM+AL�!ALn�AK+AJ�RAI��AH^5AGS�AE�#AE�AEK�ADȴAA`BA@bA>ĜA>9XA=+A< �A;`BA;VA:-A9�A9A8�DA8 �A6=qA5
=A3�A3�7A3�A2ĜA2�uA29XA1��A17LA09XA/G�A.5?A-&�A,bA+%A*�+A*E�A*$�A)�
A(A�A&ȴA%hsA$z�A$VA$A�A$1'A${A#�TA#��A"$�A �+A�TA�9AA�/A{A�uA�Ax�A�A�A�A�
A��A5?AS�A��A�-A�A�A��A��A�jA�DAz�AffA{A;dA
�A	��A	XA��A �AG�A�jA~�AQ�A-A  An�AAhsA��An�AjAbNAbNAZAVAI�A(�A�A��At�A�A v�@��@���@���@��D@��P@���@��u@�"�@���@��@��@�1@�dZ@�(�@�ff@�-@��@�v�@��;@�\)@�ȴ@�@��@�1'@ڏ\@�/@���@�/@ύP@�=q@�X@�ƨ@��@�O�@���@�33@ƸR@�5?@ċD@�Z@�1@�K�@���@���@�Q�@��@�t�@�"�@�@��y@���@�E�@��#@�  @�C�@��+@��j@���@�|�@�@�ȴ@�M�@�J@���@�%@���@��m@���@��@�p�@�O�@��/@�z�@�9X@���@���@��P@�33@�{@��@��@�j@�A�@�  @���@�t�@�
=@���@�$�@���@���@��h@���@��
@�K�@�33@��@��y@��H@���@��@��@�o@���@���@�^5@�J@��@�&�@���@�Q�@�K�@�o@��H@�=q@���@�7L@���@��9@�Z@�A�@�(�@�1@��P@��\@�$�@�@�/@��9@��@��u@�j@��m@�A�@��F@�
=@���@�~�@�=q@��@���@�hs@���@�  @�K�@��@�o@�
=@�
=@�@���@��H@�ȴ@�n�@��#@��7@��@��j@��m@���@��P@��P@�|�@�l�@�\)@�S�@�C�@�+@��@��!@�O�@��j@�z�@�I�@��@�;d@���@��!@���@�~�@�-@��@��-@�p�@�V@��9@�(�@��@�|�@�C�@�33@��@�ȴ@�M�@��T@���@�hs@�O�@�?}@��@��`@��j@���@�Z@��@�@��@��!@��+@�v�@�n�@�M�@�$�@��@��#@�@��h@�V@��`@��D@~��@|�@|I�@{��@{��@{33@z��@z^5@y��@y�#@y�^@y��@y&�@x �@w+@vȴ@vV@u��@uV@tZ@s��@qx�@l9X@cS�@X��@U�h@N5?@I��@B~�@<Z@4�D@0��@,�@'+@#��@��@7L@�T@x�@��@
J@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A�ȴA�ĜA�ĜA�ƨA�ƨA�ĜAüjAå�AA��A�(�A�E�A�  A���A�JA���A�bNA���A��+A�A�A�x�A��A�$�A��;A��wA�E�A�v�A��TA�^5A��;A��mA��-A� �A��A�$�A�bA�VA���A��A�ƨA���A�VA��PA�{A��RA��9A��hA��A��hA�S�A���A�$�A��HA���A���A�K�A��
A�ĜA�+A�XA��A���A��-A���A�XA���A���A��FA���A�ZA�"�A�C�A���A��A��A��TA�ȴA�dZA��`A���A��A�7LA���A��A�VA��^A�O�A�n�A�  A�JA� �A��uA��7A��A�E�A~�A|��Ay�PAwK�Av�uAu|�At�At  As/Aq�Ao�
Am�
AkXAh�9Ag%Ae�Ad1'Ac��AbAaC�A`��A`^5A`^5A_S�A^VA\ĜA[�#AZ�jAYdZAX�AW��AU�#ATVAS�PAR�+AP�`AN��AN  AM�hAM+AL�!ALn�AK+AJ�RAI��AH^5AGS�AE�#AE�AEK�ADȴAA`BA@bA>ĜA>9XA=+A< �A;`BA;VA:-A9�A9A8�DA8 �A6=qA5
=A3�A3�7A3�A2ĜA2�uA29XA1��A17LA09XA/G�A.5?A-&�A,bA+%A*�+A*E�A*$�A)�
A(A�A&ȴA%hsA$z�A$VA$A�A$1'A${A#�TA#��A"$�A �+A�TA�9AA�/A{A�uA�Ax�A�A�A�A�
A��A5?AS�A��A�-A�A�A��A��A�jA�DAz�AffA{A;dA
�A	��A	XA��A �AG�A�jA~�AQ�A-A  An�AAhsA��An�AjAbNAbNAZAVAI�A(�A�A��At�A�A v�@��@���@���@��D@��P@���@��u@�"�@���@��@��@�1@�dZ@�(�@�ff@�-@��@�v�@��;@�\)@�ȴ@�@��@�1'@ڏ\@�/@���@�/@ύP@�=q@�X@�ƨ@��@�O�@���@�33@ƸR@�5?@ċD@�Z@�1@�K�@���@���@�Q�@��@�t�@�"�@�@��y@���@�E�@��#@�  @�C�@��+@��j@���@�|�@�@�ȴ@�M�@�J@���@�%@���@��m@���@��@�p�@�O�@��/@�z�@�9X@���@���@��P@�33@�{@��@��@�j@�A�@�  @���@�t�@�
=@���@�$�@���@���@��h@���@��
@�K�@�33@��@��y@��H@���@��@��@�o@���@���@�^5@�J@��@�&�@���@�Q�@�K�@�o@��H@�=q@���@�7L@���@��9@�Z@�A�@�(�@�1@��P@��\@�$�@�@�/@��9@��@��u@�j@��m@�A�@��F@�
=@���@�~�@�=q@��@���@�hs@���@�  @�K�@��@�o@�
=@�
=@�@���@��H@�ȴ@�n�@��#@��7@��@��j@��m@���@��P@��P@�|�@�l�@�\)@�S�@�C�@�+@��@��!@�O�@��j@�z�@�I�@��@�;d@���@��!@���@�~�@�-@��@��-@�p�@�V@��9@�(�@��@�|�@�C�@�33@��@�ȴ@�M�@��T@���@�hs@�O�@�?}@��@��`@��j@���@�Z@��@�@��@��!@��+@�v�@�n�@�M�@�$�@��@��#@�@��h@�V@��`@��D@~��@|�@|I�@{��@{��@{33@z��@z^5@y��@y�#@y�^@y��@y&�@x �@w+@vȴ@vV@u��@uV@tZ@s��@qx�@l9X@cS�@X��@U�h@N5?@I��@B~�@<Z@4�D@0��@,�@'+@#��@��@7L@�T@x�@��@
J@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�=B�DB�JB�PB�PB�VB�VB�VB�bB�hB��BŢB��B��B�jB��B��B�{B�\B�7B� Bq�Bo�Br�Bk�BhsBZBA�B5?B-B"�B�BPB
=BB�B�ZB�NB�TB�)B��BŢBB�dB��B��B��B��B��B��B��B��B�oB{�Br�Bn�BjBhsBgmBffBYBJ�BG�BE�BA�BD�B?}B9XB2-B �BbB+BB�B��BÖB��B�wB�dB�'B�oB|�BdZB?}B�BbBB
��B
�B
�)B
��B
B
�B
��B
�bB
�B
hsB
W
B
<jB
�B
B	��B	�B	�B	�ZB	�/B	��B	B	�FB	��B	�{B	�DB	�B	z�B	s�B	gmB	dZB	cTB	dZB	dZB	[#B	VB	S�B	L�B	F�B	?}B	9XB	/B	$�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	hB	\B	PB	
=B	+B	B	B	  B��B��B�B�B�sB�`B�NB�;B�/B�#B�B�B��B��B��BɺBŢBÖBB��B��B�}B�wB�wB�qB�jB�dB�RB�9B�B�B��B��B��B��B��B�{B�\B�JB�DB�DB�DB�=B�1B�%B� Bz�Bw�Bs�Bn�Bk�BhsBe`BdZBbNBaHB^5BZBT�BO�BL�BI�BG�BF�BE�BD�BB�B@�B?}B>wB>wB<jB;dB8RB7LB5?B49B2-B1'B/B.B.B-B,B)�B'�B&�B$�B$�B$�B$�B$�B#�B#�B#�B#�B#�B"�B"�B%�B'�B+B+B)�B(�B(�B&�B'�B&�B$�B#�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoB{B�B�B�B�B�B�B�B�B�B�B$�B$�B$�B&�B(�B+B-B.B.B.B.B-B-B-B,B-B.B.B33B5?B6FB7LB8RB:^B:^B<jB>wB?}B?}BE�BG�BJ�BI�BJ�BK�BL�BN�BQ�BQ�BR�BYBaHBaHBaHBbNBcTBcTBdZBffBffBiyBjBk�Bk�Bl�Bo�Bq�Br�Br�Br�Br�Bq�B{�B� B�B�%B�+B�1B�7B�DB�JB�JB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�!B�'B�9B�?B�FB�FB�RB�jBĜBǮB��B��B��B��B��B��B��B�/B�NB�TB�ZB�ZB�`B�`B�fB�mB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	DB	\B	hB	hB	uB	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	)�B	-B	33B	5?B	7LB	9XB	9XB	:^B	<jB	?}B	B�B	D�B	E�B	F�B	G�B	H�B	I�B	J�B	K�B	M�B	R�B	XB	ZB	\)B	]/B	^5B	^5B	_;B	`BB	aHB	bNB	bNB	bNB	cTB	cTB	bNB	ffB	k�B	l�B	m�B	n�B	q�B	s�B	u�B	w�B	x�B	x�B	y�B	}�B	�B	�B	�+B	�1B	�=B	�DB	�PB	�hB	��B	�B	��B	�B	��B
+B
hB
�B
(�B
49B
:^B
@�B
G�B
L�B
P�B
[#B
_;B
dZB
iyB
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�$B�,B�0B�9B�7B�=B�=B�?B�HB�LB��BŌB��BʧB�SB��B�qB�^B�AB�B�Bq�Bo�Br�BklBhWBY�BAjB5B,�B"�B�B3B
B �B�B�<B�0B�6B�
B̭BłB�mB�BB��B��B�pB��B��B��B��B��B�OB{�Br�BnxBj_BhSBgJBfEBX�BJ�BG�BE�BAgBD|B?\B96B2B �B@B	B �B�nB��B�sB�`B�TB�AB�B�MB|�Bd8B?\B�BAB�B
��B
�oB
�	B
��B
�lB
��B
��B
�BB
��B
hUB
V�B
<JB
}B
�B	��B	�B	�oB	�=B	�B	��B	�tB	�,B	��B	�cB	�+B	�B	z�B	s�B	gWB	dAB	c9B	dBB	dBB	[B	U�B	S�B	L�B	F�B	?eB	9AB	/B	$�B	�B	xB	lB	�B	�B	}B	vB	jB	^B	TB	IB	<B	
(B	B	B	 �B��B��B��B�B�vB�]B�LB�8B�(B�B�B��B��B��B��B��BɥBŏBÃB�zB�sB�nB�hB�cB�dB�ZB�XB�PB�>B�&B�
B��B��B��B��B��B��B�iB�JB�9B�2B�3B�4B�*B�B�B�Bz�Bw�Bs�Bn�BksBh`BeOBdIBb<Ba7B^%BZ
BT�BO�BL�BI�BG�BF�BE�BD�BB�B@sB?QB>fB>hB<XB;TB8BB7=B5/B4&B2B0�B/B.B.B,�B+�B)�B'�B&�B$�B$�B$�B$�B$�B#�B#�B#�B#�B#�B"�B"�B%�B'�B*�B*�B)�B(�B(�B&�B'�B&�B$�B#�B#�B �B�BB~B�B�BzB{B�B|B�B�B�B}B[BNB^BiBnBtBpB[BnB�B�B�B�B�B$�B$�B$�B&�B(�B*�B,�B-�B.B-�B. B,�B,�B,�B+�B,�B-�B-�B3 B5*B6/B78B8>B:IB:HB<TB>bB?hB?hBE�BG�BJ�BI�BJ�BK�BL�BN�BQ�BQ�BR�BYBa4Ba0Ba1Bb7Bc?Bc<BdABfOBfLBibBjhBklBknBlsBo�Bq�Br�Br�Br�Br�Bq�B{�B�B�B�B�B�B�B�*B�/B�0B�FB�fB�pB�tB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�$B�+B�+B�5B�LBĀBǐBʤB˨B̰BνB��B��B��B�B�3B�8B�<B�=B�@B�@B�FB�OB�VB�fB�mB�rB��B�B��B��B��B��B��B��B��B��B��B��B��B��B	#B	=B	HB	IB	TB	rB	�B	�B	�B	�B	 �B	"�B	$�B	&�B	)�B	,�B	3B	5B	7*B	95B	96B	:<B	<GB	?ZB	BnB	DyB	E�B	F�B	G�B	H�B	I�B	J�B	K�B	M�B	R�B	W�B	Y�B	\B	]B	^B	^B	_B	` B	a"B	b+B	b-B	b)B	c.B	c2B	b)B	fCB	kcB	leB	mnB	nsB	q�B	s�B	u�B	w�B	x�B	x�B	y�B	}�B	��B	��B	�B	�B	�B	�B	�+B	�AB	�bB	��B	ͪB	�eB	��B
B
=B
�B
(�B
4B
:4B
@ZB
G�B
L�B
P�B
Z�B
_B
d1B
iPB
nnB
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708192016053117081920160531170819  AO  ARCAADJP                                                                    20140721230847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230847  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230847  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170819  IP                  G�O�G�O�G�O�                