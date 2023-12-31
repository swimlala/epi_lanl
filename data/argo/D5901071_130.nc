CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:27Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142128  20190522121827  1727_5046_130                   2C  D   APEX                            2143                            040306                          846 @Խ��Y@1   @Խ���	@7Z�G�{�d�\(��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffB��B ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDX  DX� DYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb��DcfDc�fDdfDd�fDe  De�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDs  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�  A�A�A�A�A�A�A�1A�JA�VA�VA�bA�oA�oA�oA�VA�1A��A��A�A��A���A��PA�x�A�33A���A�~�A�Q�A�7LA�VA���A���A��DA�VA�-A��A��A���A��A��A�ƨA���A���A��jA��A��FA���A��mA��`A��`A�~�A�jA�|�A��A��A��^A���A���A��A���A���A���A�A�A���A��A��A��`A��;A���A��uA���A���A�x�A�XA��yA�O�A�z�A��A�v�A�&�A�VA��A���A��FA���A�&�A��#A��A��RA�JA���A���A��FA�
=A�dZA�|�A��\A���A�%A�p�A���A��#A��jA���A��uA�oA��A��
A��+A�z�A�Q�A�  A��wA�G�A���A���A�hsA�dZA��A��A�{A���A��/A�Q�A�G�A��FA�ƨA�|�A�dZA��TA�7LA���A��AA}��Ax��Ax{Au�;As��Aq\)Ap5?Ao��An�Ak
=Af��Ac�
AadZA_C�A^�jA^E�A]%AZ��AY"�AX  AV�9AU%AR�AQƨAP��AN�ANn�AM�PAL�AJ5?AG�#AFbNAE?}AC7LAB1'AA��AA��A@�A=`BA<^5A<1'A;dZA7�mA69XA5dZA4�A3�^A3?}A2��A1��A0�+A/��A-�A,�A*jA(��A&ȴA&=qA%��A%��A%�A$�\A"�yA!�A ��AƨAS�A"�A�A��A�AA&�A��A�A�AjA��AVA�wA�DA��AhsA��A�TAt�A�jAM�A=qA�
A��A �A��AVAA�A��A|�A33A
��A
A	�wA	��A	�A	O�A��A~�A��A��A$�A\)A�A�yA1'A�;A|�A ��A ��A 5?@��F@�;d@�?}@��w@�-@�X@��m@��P@���@��@�@���@��T@�%@��@�@�@�1'@�
=@�@�7L@�9@���@��y@�h@��@ߍP@�E�@۾w@�O�@�o@�=q@��@�@թ�@ա�@ա�@Ցh@�&�@ԣ�@�Z@��m@�o@җ�@�V@�O�@��@˅@��@ț�@ř�@�b@�J@��@�  @�+@���@�^5@�@�7L@���@�A�@���@�dZ@�@�J@�A�@�l�@��y@�ȴ@���@���@�A�@�33@��R@�~�@�E�@���@�7L@�bN@��m@�dZ@���@��+@�M�@�@��/@���@�9X@�l�@���@��@��h@�/@��@�/@��`@�V@��@��@�E�@�V@�^5@�-@��^@�/@�%@�r�@��;@�K�@�C�@��@��\@��+@�ff@�@���@��@�Ĝ@�Q�@�ƨ@��@�+@�+@�33@��@��+@��^@�?}@���@��/@�Z@���@�S�@���@�E�@��@���@�G�@��@��j@�r�@��@��P@��@��@��R@���@�ff@��-@�/@���@��j@��@���@��D@�z�@�Z@�(�@�b@�  @���@�|�@���@�|�@�dZ@�
=@��H@���@��\@�V@��#@���@��;@�o@��R@�~�@�^5@�{@���@��#@��-@�hs@�7L@�%@���@�I�@�(�@�1'@�1'@�9X@�I�@�Q�@��@��w@�|�@�K�@�33@�
=@��R@�ff@�E�@�{@��#@��-@���@�p�@���@��u@��@�Z@��@��@�K�@�
=@���@���@��+@�ff@�M�@�@��T@�@��@���@�r�@�(�@�1'@�Q�@�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A�  A�A�A�A�A�A�A�1A�JA�VA�VA�bA�oA�oA�oA�VA�1A��A��A�A��A���A��PA�x�A�33A���A�~�A�Q�A�7LA�VA���A���A��DA�VA�-A��A��A���A��A��A�ƨA���A���A��jA��A��FA���A��mA��`A��`A�~�A�jA�|�A��A��A��^A���A���A��A���A���A���A�A�A���A��A��A��`A��;A���A��uA���A���A�x�A�XA��yA�O�A�z�A��A�v�A�&�A�VA��A���A��FA���A�&�A��#A��A��RA�JA���A���A��FA�
=A�dZA�|�A��\A���A�%A�p�A���A��#A��jA���A��uA�oA��A��
A��+A�z�A�Q�A�  A��wA�G�A���A���A�hsA�dZA��A��A�{A���A��/A�Q�A�G�A��FA�ƨA�|�A�dZA��TA�7LA���A��AA}��Ax��Ax{Au�;As��Aq\)Ap5?Ao��An�Ak
=Af��Ac�
AadZA_C�A^�jA^E�A]%AZ��AY"�AX  AV�9AU%AR�AQƨAP��AN�ANn�AM�PAL�AJ5?AG�#AFbNAE?}AC7LAB1'AA��AA��A@�A=`BA<^5A<1'A;dZA7�mA69XA5dZA4�A3�^A3?}A2��A1��A0�+A/��A-�A,�A*jA(��A&ȴA&=qA%��A%��A%�A$�\A"�yA!�A ��AƨAS�A"�A�A��A�AA&�A��A�A�AjA��AVA�wA�DA��AhsA��A�TAt�A�jAM�A=qA�
A��A �A��AVAA�A��A|�A33A
��A
A	�wA	��A	�A	O�A��A~�A��A��A$�A\)A�A�yA1'A�;A|�A ��A ��A 5?@��F@�;d@�?}@��w@�-@�X@��m@��P@���@��@�@���@��T@�%@��@�@�@�1'@�
=@�@�7L@�9@���@��y@�h@��@ߍP@�E�@۾w@�O�@�o@�=q@��@�@թ�@ա�@ա�@Ցh@�&�@ԣ�@�Z@��m@�o@җ�@�V@�O�@��@˅@��@ț�@ř�@�b@�J@��@�  @�+@���@�^5@�@�7L@���@�A�@���@�dZ@�@�J@�A�@�l�@��y@�ȴ@���@���@�A�@�33@��R@�~�@�E�@���@�7L@�bN@��m@�dZ@���@��+@�M�@�@��/@���@�9X@�l�@���@��@��h@�/@��@�/@��`@�V@��@��@�E�@�V@�^5@�-@��^@�/@�%@�r�@��;@�K�@�C�@��@��\@��+@�ff@�@���@��@�Ĝ@�Q�@�ƨ@��@�+@�+@�33@��@��+@��^@�?}@���@��/@�Z@���@�S�@���@�E�@��@���@�G�@��@��j@�r�@��@��P@��@��@��R@���@�ff@��-@�/@���@��j@��@���@��D@�z�@�Z@�(�@�b@�  @���@�|�@���@�|�@�dZ@�
=@��H@���@��\@�V@��#@���@��;@�o@��R@�~�@�^5@�{@���@��#@��-@�hs@�7L@�%@���@�I�@�(�@�1'@�1'@�9X@�I�@�Q�@��@��w@�|�@�K�@�33@�
=@��R@�ff@�E�@�{@��#@��-@���@�p�@���@��u@��@�Z@��@��@�K�@�
=@���@���@��+@�ff@�M�@�@��T@�@��@���@�r�@�(�@�1'@�Q�@�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�VB�VB�VB�VB�VB�PB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�?B�LB�LB�jB��B�B�NB�TB�TB�#B�B�BB�B��B
=B�B�B(�B0!B5?B9XB=qB@�BF�BL�BN�BO�BP�BS�BO�B:^B1'B:^BR�BN�BH�BL�BB�B�B  B�B�;B�BȴB�jB�dB�wB��B��B��B�B�/B�#B��B��B�-B��B��B�7B~�Bn�BG�B+BbB��B��B��B�B�B�B�B�TB��B�}B��B��B�=B{�Bs�BhsB\)BK�B8RB�B
�B
�
B
ŢB
�'B
��B
�=B
{�B
ZB
A�B
)�B
�B	��B	�B	�TB	�B	ĜB	�qB	�^B	�'B	��B	~�B	l�B	_;B	VB	Q�B	L�B	J�B	8RB	0!B	+B	$�B	�B	�B	{B	PB	PB	PB	DB	1B	B��B�B�yB�mB�ZB�TB�HB�BƨB��B�qB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�bB�hB�=B�7B�1B�+B�+B�%B�B�B�B� B}�B{�By�Bw�Bs�Bq�Bo�Bm�Bk�BjBjBiyBhsBhsBiyBhsBffBdZBcTBbNB`BB_;B^5B]/BYBVBT�BT�BT�BS�BR�BQ�BO�BN�BM�BL�BK�BL�BM�BO�BO�BO�BP�BP�BO�BN�BL�BK�BI�BJ�BO�BT�BW
B\)B_;B_;BaHB`BB^5B^5B_;B^5B_;B^5B^5B^5B^5B]/B]/B\)B\)B[#BYBYB\)B]/B^5B_;B_;B_;B_;B^5B^5B^5B_;B_;B_;B`BB_;B_;BbNBdZBgmBn�Bo�Br�Bw�Bz�B}�B�B�B�1B�=B�PB�PB�\B�oB�uB�oB�oB�{B��B��B��B��B�B�!B�FB�XB�dB�qB�qB�wBBƨB��B��B��B��B��B��B�
B�
B�
B�B�#B�5B�;B�NB�`B�B�B��B��B��B��B	B	B	B	%B	
=B	uB	uB	�B	�B	�B	�B	�B	�B	#�B	'�B	+B	.B	/B	2-B	33B	5?B	7LB	;dB	;dB	;dB	<jB	@�B	C�B	D�B	D�B	F�B	M�B	N�B	P�B	T�B	YB	[#B	^5B	_;B	aHB	cTB	ffB	jB	m�B	o�B	p�B	q�B	r�B	v�B	x�B	y�B	{�B	{�B	|�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�DB	�JB	�JB	�PB	�PB	�VB	�DB	�7B	�JB	�DB	�=B	�7B	�=B	�DB	�JB	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�LB	�LB	�RB	�RB	�RB	�jB	�}B	�}B	��B	��B	ÖB	ĜB	ĜB	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�VB�VB�VB�VB�VB�VB�PB�VB�VB�VB�VB�\B�\B�\B�\B�\B�\B�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B�B�'B�?B�LB�LB�jB��B�B�NB�TB�`B�#B�B�;B�B��B
=B�B�B(�B0!B5?B9XB=qB@�BF�BL�BN�BO�BP�BT�BR�B<jB2-B;dBT�BQ�BL�BO�BK�B%�BB�B�HB�/B��B�}B�qB�wB��B�B��B�B�;B�;B��BǮB�LB��B��B�DB�Bx�BP�B33B�BB��B��B��B�B�B�B�B�#BɺB�B��B�bB� Bz�Bn�BcTBR�BC�B1'B
��B
�/B
��B
�qB
��B
�PB
�+B
aHB
H�B
-B
$�B	��B	��B	�yB	�#B	ǮB	�}B	�qB	�dB	��B	�1B	s�B	ffB	XB	S�B	P�B	R�B	=qB	49B	/B	+B	%�B	�B	�B	oB	VB	\B	VB	VB		7B��B�B�B�B�`B�ZB�`B�HBɺB��B�}B��B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�JB�7B�1B�+B�%B�%B�%B�B�B� B}�B{�By�Bu�Bu�Br�Bo�Bl�Bl�Bl�BjBjBiyBiyBiyBiyBgmBe`BdZBbNB`BB_;B^5B`BBYBVBVBT�BT�BS�BS�BR�BP�BO�BO�BP�BO�BO�BP�BP�BQ�BQ�BR�BP�BO�BN�BM�BK�BK�BQ�BVBZB]/B_;B`BBbNBaHBbNBaHBaHBaHBaHB`BB_;B_;B_;B^5B_;B]/B^5B]/B]/B]/B_;B^5B_;B_;B_;B_;B_;B^5B_;B_;B`BB`BB`BBaHBbNBe`BdZBe`BjBp�Bt�Bu�Bz�B|�B� B�B�%B�7B�DB�VB�VB�bB�uB�{B�uB�{B��B��B��B��B��B�B�-B�LB�^B�dB�qB�wB�}BÖBǮB��B��B��B��B��B��B�
B�B�B�B�)B�;B�BB�NB�`B�B�B��B��B��B��B	B	B	%B	+B	
=B	{B	{B	�B	�B	�B	�B	�B	�B	$�B	(�B	,B	/B	0!B	33B	33B	6FB	7LB	;dB	;dB	<jB	=qB	A�B	D�B	D�B	E�B	G�B	N�B	O�B	Q�B	VB	YB	\)B	^5B	_;B	aHB	dZB	gmB	k�B	m�B	o�B	p�B	q�B	s�B	w�B	x�B	y�B	{�B	{�B	|�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�JB	�PB	�VB	�bB	�JB	�=B	�PB	�DB	�=B	�=B	�=B	�DB	�JB	�VB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�LB	�LB	�RB	�RB	�XB	�qB	�}B	�}B	��B	B	ĜB	ĜB	ĜB	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<D��<#�
<#�
<49X<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447192012010314471920120103144719  AO  ARGQ                                                                        20111130142128  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142128  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144719  IP                  G�O�G�O�G�O�                