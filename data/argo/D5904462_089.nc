CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-26T03:16:26Z AOML 3.0 creation; 2016-08-07T21:51:23Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151126031626  20160807145124  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               YA   AO  5287_9017_089                   2C  D   APEX                            6529                            072314                          846 @ׁs\Y��1   @ׁt卂@0���+�d�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    YA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33C 33C�C�fC�fC�fC	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyy�D�	�D�S3D�p D�� D�  D�S3D�y�D���D�fD�FfD���D��3D�fD�\�Dډ�D��3D���D�@ D�vfD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33A��A%��AE��Ae��A���A���A���A���A���A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBIffBQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��3B��3B��fB��fB��3B��3B��fB��fB�� B�� B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3B܀ B�3B�3B�3B�3B�3B��3B��3B��fC ��Cs4C@ C@ C@ C
@ C@ CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBs4CDs4CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'�D'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt|�Dy� D��D�^fD�{3D��3D�3D�^fD���D���D��D�Q�D���D��fD��D�h Dڔ�D��fD��D�K3D�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�33A�5?A�5?A�7LA�9XA�9XA�9XA�9XA�A�A�I�A�O�A�^5A�VA�VA�ffA�hsA�l�A�n�A�ffA�ffA�jA�jA�l�A�hsA�ffA�l�A�p�A�n�A�n�A�p�A�jA�l�A�bNA�`BA�z�Aއ+Aމ7AރA�(�A���A�S�A�%A��/A�&�A��A�hsẠ�A�JA���A�7LA�t�AƉ7A��AļjA��A�1'A�^5A�A�A�%A��PA��mA�oA�
=A��A�oA���A��A�$�A���A�33A��jA���A���A��A��A��`A�C�A���A�jA�9XA��PA��mA�"�A�
=A��A�{A���A�hsA���A�ĜA�(�A�&�A���A�bNA�9XA~VAz~�Au�Ar(�Ao��AlI�Ae"�Ad(�A`ȴA^�A\�AZ�AY�TAY�hAWO�ASx�AR�!AP��AL��AI��AG|�AE�AD�HAB�A@I�A=dZA<z�A;\)A:�A9hsA8^5A6�A5��A4�/A4n�A4bA3��A3oA2=qA1ƨA1\)A0��A.(�A,A�A*�A);dA(^5A'��A&�!A%�mA#`BA"n�A!�PAdZA�^A&�A33A�\AA�TA�^A �AJA{AA�AjA �A1AC�A �At�AI�AG�Ap�A�7A�A�+A�DAbA�A��A\)A�yA5?A\)A	�A
A	�
A	p�A��A|�AVA�AG�A��A�AjAv�A/A33A�A��AI�A(�A�PA j@���A 5?A v�A �jAS�AK�A ��A �A ^5A �A 1'@��P@��A ^5A E�@���@��w@�l�@�
=@��!@�^5@��@�V@��w@���@��y@���@�@���@��@�t�@�$�@��H@���@��m@���@�Ĝ@�?}@�p�@�O�@��m@�-@�!@�w@�@�^@���@��@��T@�`B@�J@�V@ᙚ@��@��/@ߕ�@�~�@���@�O�@��@�ƨ@�;d@٩�@�v�@ٲ-@ٙ�@ف@�`B@ف@�O�@�b@�
=@֗�@�n�@�5?@Ձ@��/@ԣ�@ԃ@�z�@�z�@Ԭ@�bN@��m@�(�@�z�@�Z@���@�$�@��@Ѓ@Ͼw@�E�@́@̴9@��@�+@��#@�?}@��@�ȴ@�J@��T@ũ�@�G�@�z�@�z�@ě�@ļj@��@���@���@�X@�O�@�%@�X@�?}@�Ĝ@�Z@�A�@�1@�K�@�=q@���@�hs@�%@��D@�1'@� �@���@��@�@���@�^5@�@��^@��7@�p�@�O�@���@�Ĝ@��m@���@�K�@�33@��@���@�^5@��^@��7@�p�@�/@��9@�I�@�r�@�(�@�|�@�dZ@�;d@�v�@�J@�@�hs@�/@��@�X@�G�@��@�j@�ƨ@��@��!@�/@��;@�\)@�o@���@��-@�x�@���@��@�b@���@�l�@���@��R@���@�~�@�5?@��@�p�@���@��@��@�+@���@���@�-@��#@���@��@�O�@�p�@��7@�7L@��j@�A�@��
@��@���@�\)@�"�@��!@�^5@�=q@��@���@��7@�x�@�X@��@��9@��D@�j@�I�@���@��P@�\)@�;d@�+@��@��@���@�v�@���@���@�`B@�&�@��@���@��@��@��@��@��;@��
@��F@��P@�S�@�C�@�o@��y@�ȴ@���@�n�@�^5@�V@�M�@�^5@�-@��@��^@��7@���@�j@�I�@�  @�dZ@��@��!@�n�@�{@���@�p�@�`B@�`B@�`B@�O�@��/@��u@��u@��D@�z�@���@�l�@�t�@�hs@�~�@�j@{�@sƨ@j�H@a�7@Y�#@QX@G+@>�R@7;d@0��@+�
@&v�@#o@�@�@�@��@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�33A�33A�5?A�5?A�7LA�9XA�9XA�9XA�9XA�A�A�I�A�O�A�^5A�VA�VA�ffA�hsA�l�A�n�A�ffA�ffA�jA�jA�l�A�hsA�ffA�l�A�p�A�n�A�n�A�p�A�jA�l�A�bNA�`BA�z�Aއ+Aމ7AރA�(�A���A�S�A�%A��/A�&�A��A�hsẠ�A�JA���A�7LA�t�AƉ7A��AļjA��A�1'A�^5A�A�A�%A��PA��mA�oA�
=A��A�oA���A��A�$�A���A�33A��jA���A���A��A��A��`A�C�A���A�jA�9XA��PA��mA�"�A�
=A��A�{A���A�hsA���A�ĜA�(�A�&�A���A�bNA�9XA~VAz~�Au�Ar(�Ao��AlI�Ae"�Ad(�A`ȴA^�A\�AZ�AY�TAY�hAWO�ASx�AR�!AP��AL��AI��AG|�AE�AD�HAB�A@I�A=dZA<z�A;\)A:�A9hsA8^5A6�A5��A4�/A4n�A4bA3��A3oA2=qA1ƨA1\)A0��A.(�A,A�A*�A);dA(^5A'��A&�!A%�mA#`BA"n�A!�PAdZA�^A&�A33A�\AA�TA�^A �AJA{AA�AjA �A1AC�A �At�AI�AG�Ap�A�7A�A�+A�DAbA�A��A\)A�yA5?A\)A	�A
A	�
A	p�A��A|�AVA�AG�A��A�AjAv�A/A33A�A��AI�A(�A�PA j@���A 5?A v�A �jAS�AK�A ��A �A ^5A �A 1'@��P@��A ^5A E�@���@��w@�l�@�
=@��!@�^5@��@�V@��w@���@��y@���@�@���@��@�t�@�$�@��H@���@��m@���@�Ĝ@�?}@�p�@�O�@��m@�-@�!@�w@�@�^@���@��@��T@�`B@�J@�V@ᙚ@��@��/@ߕ�@�~�@���@�O�@��@�ƨ@�;d@٩�@�v�@ٲ-@ٙ�@ف@�`B@ف@�O�@�b@�
=@֗�@�n�@�5?@Ձ@��/@ԣ�@ԃ@�z�@�z�@Ԭ@�bN@��m@�(�@�z�@�Z@���@�$�@��@Ѓ@Ͼw@�E�@́@̴9@��@�+@��#@�?}@��@�ȴ@�J@��T@ũ�@�G�@�z�@�z�@ě�@ļj@��@���@���@�X@�O�@�%@�X@�?}@�Ĝ@�Z@�A�@�1@�K�@�=q@���@�hs@�%@��D@�1'@� �@���@��@�@���@�^5@�@��^@��7@�p�@�O�@���@�Ĝ@��m@���@�K�@�33@��@���@�^5@��^@��7@�p�@�/@��9@�I�@�r�@�(�@�|�@�dZ@�;d@�v�@�J@�@�hs@�/@��@�X@�G�@��@�j@�ƨ@��@��!@�/@��;@�\)@�o@���@��-@�x�@���@��@�b@���@�l�@���@��R@���@�~�@�5?@��@�p�@���@��@��@�+@���@���@�-@��#@���@��@�O�@�p�@��7@�7L@��j@�A�@��
@��@���@�\)@�"�@��!@�^5@�=q@��@���@��7@�x�@�X@��@��9@��D@�j@�I�@���@��P@�\)@�;d@�+@��@��@���@�v�@���@���@�`B@�&�@��@���@��@��@��@��@��;@��
@��F@��P@�S�@�C�@�o@��y@�ȴ@���@�n�@�^5@�V@�M�@�^5@�-@��@��^@��7@���@�j@�I�@�  @�dZ@��@��!@�n�@�{@���@�p�@�`B@�`B@�`B@�O�@��/@��u@��u@��D@�z�@���@�l�G�O�@�hs@�~�@�j@{�@sƨ@j�H@a�7@Y�#@QX@G+@>�R@7;d@0��@+�
@&v�@#o@�@�@�@��@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ZB
YB
ZB
YB
[#B
[#B
[#B
ZB
ZB
[#B
]/B
^5B
bNB
`BB
_;B
dZB
e`B
ffB
gmB
dZB
cTB
dZB
cTB
dZB
bNB
aHB
cTB
dZB
dZB
dZB
e`B
bNB
bNB
_;B
^5B
gmB
l�B
p�B
z�B
��B
��B
�B_;Bv�B��B�B�B�B��B�B'�B<jBI�BL�BYBffBp�By�B~�B{�B~�Bx�BjBR�B1'BbB��B�ZB��B�^BŢBȴBÖB�^B�FB�B��B�BjB@�BbBB
��B
�yB
�B
ȴB
�3B
�bB
w�B
O�B
8RB
�B
B	�sB	�B	��B	�qB	��B	�=B	x�B	jB	S�B	6FB	-B	�B	�B	bB	JB	
=B	+B��B�B�B�mB�`B�mB�B�B�B�B��B	1B		7B		7B		7B		7B	hB	oB	bB	{B	�B	�B	�B	!�B	(�B	+B	(�B	(�B	2-B	1'B	'�B	 �B	�B	�B	hB	VB	DB	PB	1B	  B��B	VB	�B	bB	bB	�B	�B	:^B	M�B	_;B	iyB	x�B	�DB	�hB	��B	�hB	�DB	�B	p�B	v�B	w�B	ffB	S�B	VB	Q�B	K�B	L�B	K�B	H�B	C�B	?}B	:^B	B�B	K�B	N�B	I�B	F�B	G�B	G�B	H�B	I�B	I�B	M�B	O�B	L�B	O�B	O�B	R�B	R�B	S�B	P�B	L�B	L�B	W
B	_;B	ffB	q�B	{�B	z�B	y�B	�B	�B	�JB	�bB	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�!B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	��B	��B	��B	�B	�-B	�9B	�9B	�-B	�{B	�JB	��B	�\B	�%B	� B	{�B	�B	�B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�uB	��B	�LB	�?B	�FB	�FB	�jB	��B	��B	�}B	�jB	�^B	�^B	�XB	�LB	�?B	�?B	�?B	�FB	�XB	��B	ĜB	ĜB	��B	��B	��B	��B	��B	ȴB	ƨB	ĜB	��B	�wB	�jB	�^B	�RB	�FB	�9B	�-B	�-B	�-B	�'B	�'B	�!B	�-B	�3B	�?B	�LB	�dB	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�)B	�#B	�)B	�)B	�/B	�)B	�)B	�HB	�BB	�ZB	�ZB	�NB	�NB	�NB	�ZB	�`B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�`B	�`B	�`B	�sB	�sB	�mB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
	7B
	7B
	7B
1B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
VB
\B
bB
hB
hB
oB
oB
oB
oB
oB
hB
\B
\B
\B
{B
�B
�B
"�B
'�B
0!B
6FB
;dB
B�B
I�B
O�B
YB
_;B
dZB
gmB
k�B
p�B
t�B
x�B
z�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
ZB
YB
Z	B
YB
[B
[B
[B
ZB
ZB
[B
]B
^B
b8B
`-B
_$B
dBB
eJB
fRB
gXB
dDB
c>B
dBB
cAB
dFB
b;B
a3B
c?B
dCB
dEB
dCB
eLB
b8B
b<B
_&B
^!B
gVB
lvB
p�B
z�B
��B
��B
�yB_!Bv�B��B��B��B�B˫B�}B'�B<PBI�BL�BX�BfIBp�By�B~�B{�B~�Bx�BjbBR�B1BBB��B�8B��B�ABŅBȕB�vB�?B�%B��B�tB��BjaB@cBCB�B
��B
�[B
��B
ȕB
�B
�EB
w�B
O�B
86B
�B
B	�YB	� B	̱B	�WB	��B	�&B	x�B	jhB	S�B	6.B	,�B	�B	oB	LB	5B	
'B	B��B��B�wB�[B�JB�XB�lB�uB�}B�B��B	B		!B		!B		"B		B	RB	VB	LB	gB	nB	vB	�B	!�B	(�B	*�B	(�B	(�B	2B	1B	'�B	 �B	�B	rB	OB	>B	)B	7B	B��B��B	>B	lB	JB	IB	tB	�B	:EB	M�B	_ B	iXB	x�B	�'B	�JB	�bB	�IB	�&B	��B	p�B	v�B	w�B	fGB	S�B	U�B	Q�B	K�B	L�B	K�B	H�B	CxB	?cB	:CB	BpB	K�B	N�B	I�B	F�B	G�B	G�B	H�B	I�B	I�B	M�B	O�B	L�B	O�B	O�B	R�B	R�B	S�B	P�B	L�B	L�B	V�B	_B	fDB	q�B	{�B	z�B	y�B	��B	��B	�)B	�CB	�B	��B	��B	��B	��B	��B	��B	�B	�B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�MB	�^B	��B	��B	��B	�B	�B	�B	�B	�XB	�(B	�kB	�<B	�B	�B	{�B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	�xB	��B	�dB	�KB	�RB	��B	�*B	�B	�"B	�!B	�HB	�cB	�dB	�ZB	�EB	�:B	�;B	�3B	�)B	�B	�B	�B	�"B	�5B	�_B	�yB	�wB	ʞB	δB	ζB	ˢB	ʝB	ȐB	ƃB	�xB	�aB	�SB	�EB	�9B	�.B	�!B	�B	�B	�
B	�	B	�B	�B	��B	�	B	�B	�B	�)B	�?B	�jB	ǉB	ʜB	ˣB	ˢB	��B	��B	��B	��B	��B	ϺB	εB	ˢB	ʞB	ɖB	ȏB	ƃB	ǉB	ɗB	ˢB	ͮB	ϺB	ζB	ͯB	ͭB	ͰB	ͮB	δB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�B	�B	�"B	�B	�4B	�5B	�&B	�'B	�*B	�5B	�:B	�TB	�VB	�TB	�YB	�mB	�rB	�tB	�qB	�wB	�qB	�hB	�NB	�<B	�:B	�;B	�NB	�MB	�GB	�@B	�>B	�@B	�IB	�GB	�NB	�TB	�XB	�[B	�YB	�_B	�hB	�_B	�eB	�jB	�lB	�qB	�yB	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
B
B
B
B
	B
	B
	B

B

B

B

B

B

B
B
"B
$B
)B
(B
/B
6B
;B
AB
?B
GB
IB
HB
HB
EB
@B
2B
6G�O�B
PB
_B
~B
"�B
'�B
/�B
6B
;:B
BeB
I�B
O�B
X�B
_B
d2B
gDB
kZB
p|B
t�B
x�B
z�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.35 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451242016080714512420160807145124  AO  ARCAADJP                                                                    20151126031626    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151126031626  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151126031626  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145124  IP                  G�O�G�O�G�O�                