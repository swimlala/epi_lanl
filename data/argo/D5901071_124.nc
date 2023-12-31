CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:25Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               |A   AO  20111130142006  20190522121827  1727_5046_124                   2C  D   APEX                            2143                            040306                          846 @Ե�lw� 1   @Ե�/h@@7� ě���d
ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi�fDj  Dj� Dj��Dk� DlfDl�fDm  Dm� Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C33C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD  D� DfD�fDfD�fDfD�fDfD�fD �D �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDi�Di��DjfDj�fDk  Dk�fDl�Dl��DmfDm�fDnfDn�fDofDo� DpfDp�fDqfDq�fDrfDr�fDsf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�
=A�VA�VA�oA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A�"�A�"�A� �A��A��A�JA��/A��7A�I�A�A�A�1'A�bA��yA��`A��/A���A��wA��hA�G�A�ĜA�A�A���A��
A�A��A��A�ƨA���A���A�33A�ȴA��A�  A�x�A��A�?}A��mA��A�p�A��mA��A�A�|�A�ƨA�{A�  A��uA�+A��A�I�A���A���A��PA��A�I�A�A�Q�A�p�A��A�A�`BA��!A�7LA���A�t�A��A���A��A�|�A��DA���A�\)A��A�x�A�1'A�33A�A�A�/A��uA���A�bNA�(�A�hsA�l�A���A��7A�ĜA��A�hsA�A��mA��;A���A��A~~�A|5?AydZAtv�ArQ�Ap�9Ao|�An��Am�#Am�-AmS�Al�Aj �Ah�\Ac��A`1'A]�A[��AZ�yAZVAY/AW�AT�9AQ��APĜAO33ANAL��AK|�AJz�AJffAI33AGl�AE\)AChsAB�jAB(�AAK�A?`BA=+A;33A8�!A7A7|�A6��A5S�A3A2-A1dZA0�A01'A/�A,�A,�`A. �A-p�A,��A*�DA);dA(�9A({A'�hA&jA%�hA%C�A$��A$ �A"Q�A ��A I�A��A9XAjA��A{A��AȴA��AI�A�#Ar�A��AAS�A��An�A�A�mA�7AG�AoA�9AbNA��A�AƨA
�HA
JA�9At�A��A&�A?}A;dA&�A�\A/A �A�A�\A=qA��Ap�@��@��w@�+@�{@��/@�|�@�^5@���@��@��
@@�33@�n�@�`B@�z�@�ȴ@�hs@��/@�K�@�J@�`B@���@�1'@�|�@��@�=q@�G�@��@�I�@���@�  @��
@�S�@٩�@��@ՙ�@ԓu@��
@�"�@�7L@�"�@�n�@���@�G�@��m@ʗ�@ɲ-@Ɂ@ȋD@��m@�$�@���@�Q�@�j@�bN@�ƨ@��@°!@�M�@��h@�Z@��w@��w@��@��@��^@��h@�x�@�G�@��u@��;@���@���@�X@�r�@�Q�@�(�@��m@�33@�E�@�&�@�j@�K�@���@��!@���@�ff@�-@��#@�x�@�V@��@���@�(�@��F@�t�@�C�@�
=@��@���@�J@��@���@�r�@���@�"�@��R@��T@�p�@��@�1@��F@�dZ@�{@��h@�O�@���@���@��9@�Q�@� �@�b@��m@�;d@��!@�ff@�=q@�-@�{@��@���@���@��P@�~�@�M�@���@���@�G�@�V@���@��@��`@���@��@��@��@�K�@��@��@��!@�=q@��T@��-@�hs@���@�Q�@�(�@��;@��@�K�@��!@��+@�E�@���@��7@�`B@�O�@�&�@��@�Q�@�1'@�b@�  @��;@���@�C�@���@�n�@�^5@��@�p�@��@��@��@�`B@��@�Ĝ@���@�Q�@��@� �@��@��@�S�@�S�@�;d@�+@��@���@��\@�v�@�ff@��@���@��#@���@�p�@�7L@��@��9@��D@�(�@��F@�o@��@���@�J@���@�X@�7L@��@��`@�Ĝ@��D@�z�@�bN@�b@��;@���@�dZ@�S�@�S�@�+@���@�ff@�E�@�J@��7@�?}@�&�@�%@��u@�(�@���@��P@��@�\)@�@���@�ȴ@�~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�
=A�VA�VA�oA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A�"�A�"�A� �A��A��A�JA��/A��7A�I�A�A�A�1'A�bA��yA��`A��/A���A��wA��hA�G�A�ĜA�A�A���A��
A�A��A��A�ƨA���A���A�33A�ȴA��A�  A�x�A��A�?}A��mA��A�p�A��mA��A�A�|�A�ƨA�{A�  A��uA�+A��A�I�A���A���A��PA��A�I�A�A�Q�A�p�A��A�A�`BA��!A�7LA���A�t�A��A���A��A�|�A��DA���A�\)A��A�x�A�1'A�33A�A�A�/A��uA���A�bNA�(�A�hsA�l�A���A��7A�ĜA��A�hsA�A��mA��;A���A��A~~�A|5?AydZAtv�ArQ�Ap�9Ao|�An��Am�#Am�-AmS�Al�Aj �Ah�\Ac��A`1'A]�A[��AZ�yAZVAY/AW�AT�9AQ��APĜAO33ANAL��AK|�AJz�AJffAI33AGl�AE\)AChsAB�jAB(�AAK�A?`BA=+A;33A8�!A7A7|�A6��A5S�A3A2-A1dZA0�A01'A/�A,�A,�`A. �A-p�A,��A*�DA);dA(�9A({A'�hA&jA%�hA%C�A$��A$ �A"Q�A ��A I�A��A9XAjA��A{A��AȴA��AI�A�#Ar�A��AAS�A��An�A�A�mA�7AG�AoA�9AbNA��A�AƨA
�HA
JA�9At�A��A&�A?}A;dA&�A�\A/A �A�A�\A=qA��Ap�@��@��w@�+@�{@��/@�|�@�^5@���@��@��
@@�33@�n�@�`B@�z�@�ȴ@�hs@��/@�K�@�J@�`B@���@�1'@�|�@��@�=q@�G�@��@�I�@���@�  @��
@�S�@٩�@��@ՙ�@ԓu@��
@�"�@�7L@�"�@�n�@���@�G�@��m@ʗ�@ɲ-@Ɂ@ȋD@��m@�$�@���@�Q�@�j@�bN@�ƨ@��@°!@�M�@��h@�Z@��w@��w@��@��@��^@��h@�x�@�G�@��u@��;@���@���@�X@�r�@�Q�@�(�@��m@�33@�E�@�&�@�j@�K�@���@��!@���@�ff@�-@��#@�x�@�V@��@���@�(�@��F@�t�@�C�@�
=@��@���@�J@��@���@�r�@���@�"�@��R@��T@�p�@��@�1@��F@�dZ@�{@��h@�O�@���@���@��9@�Q�@� �@�b@��m@�;d@��!@�ff@�=q@�-@�{@��@���@���@��P@�~�@�M�@���@���@�G�@�V@���@��@��`@���@��@��@��@�K�@��@��@��!@�=q@��T@��-@�hs@���@�Q�@�(�@��;@��@�K�@��!@��+@�E�@���@��7@�`B@�O�@�&�@��@�Q�@�1'@�b@�  @��;@���@�C�@���@�n�@�^5@��@�p�@��@��@��@�`B@��@�Ĝ@���@�Q�@��@� �@��@��@�S�@�S�@�;d@�+@��@���@��\@�v�@�ff@��@���@��#@���@�p�@�7L@��@��9@��D@�(�@��F@�o@��@���@�J@���@�X@�7L@��@��`@�Ĝ@��D@�z�@�bN@�b@��;@���@�dZ@�S�@�S�@�+@���@�ff@�E�@�J@��7@�?}@�&�@�%@��u@�(�@���@��P@��@�\)@�@���@�ȴ@�~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�hB�hB�bB�hB�bB�bB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�oB�oB��B��B��B��B��B��B��B��B��B��B�BǮBȴBƨBȴB��B��B��B�ZB�B�#B�RB��B�)B�B��B��B�XB�?B�B��B��B�{B�\B�7B{�Bt�Bp�Bl�BdZBP�B@�B5?B33B-B,B,B)�B'�B!�B�B�B	7BBBB�B�NB�#B�
B��BɺBĜB�}B�jB�'B��B��B��B�BS�B'�BhB
��B
�B
�fB
�HB
�B
ǮB
�wB
�^B
�B
��B
��B
�%B
s�B
aHB
S�B
=qB
�B
DB	�B	��B	ƨB	�jB	�9B	�B	��B	��B	��B	��B	�JB	w�B	XB	B�B	49B	-B	+B	%�B	�B	{B	+B��B�B�yB�BB�B��B��B�B��B�jB�FB�XB��BǮBǮBB�'B�B��B��B��B��B��B�oB�{B��B�bB�uB��B�oB��B�3B�LB�FB�3B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�VB�DB�7B�7B�7B�1B�+B�%B�%B�B�B�B�B�B�B�B�B� B~�B� B}�Bx�By�Bp�BjBgmBffBdZBiyBl�Bm�Bl�Bl�BgmB`BB_;B\)BYBVBP�BJ�B?}B;dB:^B8RB6FB49B33B2-B2-B2-B1'B1'B1'B0!B/B.B.B-B,B-B-B-B-B,B+B)�B'�B,B/B0!B1'B49B49B49B7LB8RB9XB:^B:^B=qBD�BF�BG�BH�BO�BXB[#B[#B]/B_;B^5B^5BaHBhsBk�Br�Bu�Bv�Bv�Bz�B}�B�B�B�+B�7B�JB�PB�PB�VB�oB��B��B��B��B��B��B��B�B�!B�?B�^B�^B�^B�qB�qB�wB��B��BĜBƨB��B��B��B��B��B��B��B��B�B�
B�B�BB�fB�mB�B�B�B�B�B�B��B��B	  B	B	B	+B		7B	DB	DB	PB	VB	\B	\B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	.B	.B	/B	2-B	7LB	>wB	@�B	@�B	@�B	@�B	A�B	G�B	J�B	K�B	O�B	P�B	R�B	W
B	ZB	[#B	]/B	`BB	e`B	gmB	iyB	jB	l�B	p�B	q�B	s�B	v�B	x�B	y�B	y�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�VB	�\B	�\B	�oB	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�9B	�FB	�LB	�^B	�^B	�dB	�qB	�}B	��B	��B	��B	B	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�5B	�5B	�5B	�BB	�HB	�NB	�NB	�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�hB�hB�bB�hB�bB�bB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B�!BȴB��BƨBȴB��B��B��B�fB�B�BB�XB��B�5B�BB��BŢB�jB�XB�B�B��B��B��B�VB�B{�Bw�Bo�Bl�BW
BF�B5?B6FB33B,B-B-B+B$�B�B�BDB+BB	7B�B�ZB�/B�B��B��BƨBĜB��B�3B�B��B��B�\B^5B.B�B
��B
�B
�sB
�`B
�#B
ɺB
�}B
�qB
�-B
��B
��B
�DB
x�B
gmB
[#B
K�B
 �B
oB	��B	�#B	��B	�}B	�FB	�B	��B	��B	��B	��B	�hB	�B	bNB	J�B	8RB	/B	-B	(�B	!�B	�B	bB��B��B�B�ZB�/B�B��B�#B��B��B�^B�dB��BɺB��BǮB�FB�'B��B��B��B��B��B��B��B��B�oB��B��B�oB�{B�?B�^B�jB�LB�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�JB�=B�=B�7B�1B�=B�=B�%B�B�%B�%B�B�B�B�B�B� B�B�Bz�B�Bs�Bm�Bk�BjBe`BiyBl�Bm�Bl�Bn�Bk�BcTBcTB]/BZBW
BR�BS�BG�B<jB<jB:^B8RB6FB6FB33B2-B33B2-B2-B33B2-B2-B0!B/B0!B.B.B.B.B.B-B-B,B.B/B0!B0!B1'B5?B6FB8RB9XB:^B:^B;dB=qB@�BE�BG�BH�BJ�BQ�BYB[#B]/B^5BbNB`BB_;BaHBhsBl�Bs�Bv�Bw�Bw�B|�B~�B�B�B�7B�=B�JB�PB�VB�\B�uB��B��B��B��B��B��B��B�B�-B�LB�dB�jB�dB�qB�qB�wB��B��BŢBǮB��B��B��B��B��B��B��B��B�B�B�#B�HB�mB�sB�B�B�B�B��B��B��B��B	B	B	B	1B		7B	DB	JB	PB	VB	\B	bB	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	+B	.B	/B	0!B	33B	7LB	>wB	@�B	@�B	@�B	A�B	B�B	H�B	K�B	K�B	O�B	Q�B	S�B	XB	ZB	\)B	^5B	aHB	e`B	gmB	iyB	k�B	m�B	p�B	q�B	t�B	v�B	x�B	y�B	y�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�VB	�\B	�bB	�oB	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�9B	�FB	�LB	�^B	�^B	�dB	�qB	�}B	��B	��B	��B	B	ĜB	ŢB	ǮB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�5B	�5B	�5B	�HB	�NB	�NB	�TB	�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447172012010314471720120103144717  AO  ARGQ                                                                        20111130142006  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142006  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144717  IP                  G�O�G�O�G�O�                