CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:29Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142248  20190522121827  1727_5046_136                   2C  D   APEX                            2143                            040306                          846 @��t�?�1   @��t�b�@7z�G�{�d�`A�71   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D	y�D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DsffDy331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�33A��A!��AA��Aa��A���A�  A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBH��BPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH  CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^33C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD�D�fD	  D	� D
fD
�fDfD�fDfD�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fD�D��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDD�DD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDV�DV��DWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDd  Dd�fDefDe�fDffDf� DgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDm  Dm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDsl�Dy9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��mA��yA��mA��;A��
A���A¼jA\A�A�A�A���A�|�A�\)A�VA�Q�A�O�A�E�A�  A�VA��jA�;dA���A��DA�G�A��A���A�Q�A�A�A�E�A�?}A�7LA�7LA�oA��A�C�A�\)A��jA��A��A�ZA�hsA�r�A�|�A�VA�ȴA���A�l�A�E�A�$�A���A�7LA��9A���A��A���A�x�A���A��TA�
=A��uA�C�A�  A���A���A��PA���A�bNA�O�A���A���A���A�~�A�%A��A���A�JA��A�XA��9A��7A�-A��A�/A��#A��A��wA��wA�/A��yA�1'A��A���A���A���A��7A���A�hsA��TA�A���A�ĜA��PA��
A�ȴA��;A�;dA�I�A��`A�G�A�K�A���A��hA�JA���A��7A�=qA�jA� �A���A�bA���A�r�A�G�A��A~�9A}�Az�Ay7LAxAw+Av(�Au�hAt�jAt{Ar�`Ap�uAn$�Am�Ai�Ah�`Ah��Ag��AdJAb  A`�!A_x�A^�+A]p�A\�yA\9XA[/AZ�DAY?}AW�TAW��AVv�AS�AR�AP�9AM&�AJZAI�AI��AHbAD~�AB9XAB-ABz�AA��A@��A?p�A>ZA<��A;�A:=qA8��A8�A8 �A7�TA6z�A3A2�/A2��A2M�A1x�A0�!A/�FA//A.�9A.��A-��A,9XA)��A(z�A({A&ȴA%�A%��A%?}A$��A$(�A$JA#S�A"�A!\)A �/At�Ar�A�;A��AAG�A��A�AVA��AA��AA�AXA��AJAl�A��AVA�A��A��A��A��A\)A&�A�jA�A
��A
M�A	A	VA��A~�A�A�hA"�AjAS�A��AbAG�A bNA  �@���@��R@��-@�z�@��@��m@�M�@��j@�-@��@��@�Z@��m@@�h@땁@�n�@�p�@�9@�r�@��@�  @���@�K�@�M�@�I�@���@��@߶F@���@�(�@�33@���@ڟ�@�-@��T@�p�@�Ĝ@׶F@���@ա�@�V@�1'@ҧ�@�G�@�K�@�M�@�/@ˍP@�V@�5?@��@Ƨ�@���@�%@�A�@Ý�@���@�J@��#@�p�@�/@��u@�+@�v�@�G�@��j@��@�&�@���@���@�hs@�p�@��@�G�@���@��@��!@�O�@�"�@�M�@�x�@���@�  @���@�"�@�@�S�@�C�@�ȴ@��+@�E�@�{@��-@�hs@�%@�b@�
=@���@�E�@�X@�b@�ƨ@��w@���@�\)@��y@��H@��H@��R@�v�@�x�@�bN@�Q�@��@�Z@��@��@��F@�l�@�;d@��!@�ff@�^5@�E�@���@�7L@���@�1'@��@��F@�K�@��y@�n�@���@��h@�/@�z�@�(�@��m@���@��@���@��m@�(�@��m@�K�@��@��!@���@��H@��y@�5?@���@��9@� �@��@�|�@��@���@�v�@��#@���@��@��7@��@�n�@�M�@���@��@�I�@�  @��@��P@�K�@��@��@��H@���@�V@�J@���@���@��h@��@�`B@�7L@�&�@��@�%@��/@��D@�j@�I�@�A�@�9X@�(�@��
@��F@�|�@�K�@��@��+@�M�@�$�@�J@���@���@�  @�ƨ@��P@�C�@��R@�{@��7@�J@�V@�{@�@��T@��^@�p�@��D@�9X@� �@��;@�C�@��+@�ff@�J@�hs@���@�@�=q@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��mA��yA��mA��;A��
A���A¼jA\A�A�A�A���A�|�A�\)A�VA�Q�A�O�A�E�A�  A�VA��jA�;dA���A��DA�G�A��A���A�Q�A�A�A�E�A�?}A�7LA�7LA�oA��A�C�A�\)A��jA��A��A�ZA�hsA�r�A�|�A�VA�ȴA���A�l�A�E�A�$�A���A�7LA��9A���A��A���A�x�A���A��TA�
=A��uA�C�A�  A���A���A��PA���A�bNA�O�A���A���A���A�~�A�%A��A���A�JA��A�XA��9A��7A�-A��A�/A��#A��A��wA��wA�/A��yA�1'A��A���A���A���A��7A���A�hsA��TA�A���A�ĜA��PA��
A�ȴA��;A�;dA�I�A��`A�G�A�K�A���A��hA�JA���A��7A�=qA�jA� �A���A�bA���A�r�A�G�A��A~�9A}�Az�Ay7LAxAw+Av(�Au�hAt�jAt{Ar�`Ap�uAn$�Am�Ai�Ah�`Ah��Ag��AdJAb  A`�!A_x�A^�+A]p�A\�yA\9XA[/AZ�DAY?}AW�TAW��AVv�AS�AR�AP�9AM&�AJZAI�AI��AHbAD~�AB9XAB-ABz�AA��A@��A?p�A>ZA<��A;�A:=qA8��A8�A8 �A7�TA6z�A3A2�/A2��A2M�A1x�A0�!A/�FA//A.�9A.��A-��A,9XA)��A(z�A({A&ȴA%�A%��A%?}A$��A$(�A$JA#S�A"�A!\)A �/At�Ar�A�;A��AAG�A��A�AVA��AA��AA�AXA��AJAl�A��AVA�A��A��A��A��A\)A&�A�jA�A
��A
M�A	A	VA��A~�A�A�hA"�AjAS�A��AbAG�A bNA  �@���@��R@��-@�z�@��@��m@�M�@��j@�-@��@��@�Z@��m@@�h@땁@�n�@�p�@�9@�r�@��@�  @���@�K�@�M�@�I�@���@��@߶F@���@�(�@�33@���@ڟ�@�-@��T@�p�@�Ĝ@׶F@���@ա�@�V@�1'@ҧ�@�G�@�K�@�M�@�/@ˍP@�V@�5?@��@Ƨ�@���@�%@�A�@Ý�@���@�J@��#@�p�@�/@��u@�+@�v�@�G�@��j@��@�&�@���@���@�hs@�p�@��@�G�@���@��@��!@�O�@�"�@�M�@�x�@���@�  @���@�"�@�@�S�@�C�@�ȴ@��+@�E�@�{@��-@�hs@�%@�b@�
=@���@�E�@�X@�b@�ƨ@��w@���@�\)@��y@��H@��H@��R@�v�@�x�@�bN@�Q�@��@�Z@��@��@��F@�l�@�;d@��!@�ff@�^5@�E�@���@�7L@���@�1'@��@��F@�K�@��y@�n�@���@��h@�/@�z�@�(�@��m@���@��@���@��m@�(�@��m@�K�@��@��!@���@��H@��y@�5?@���@��9@� �@��@�|�@��@���@�v�@��#@���@��@��7@��@�n�@�M�@���@��@�I�@�  @��@��P@�K�@��@��@��H@���@�V@�J@���@���@��h@��@�`B@�7L@�&�@��@�%@��/@��D@�j@�I�@�A�@�9X@�(�@��
@��F@�|�@�K�@��@��+@�M�@�$�@�J@���@���@�  @�ƨ@��P@�C�@��R@�{@��7@�J@�V@�{@�@��T@��^@�p�@��D@�9X@� �@��;@�C�@��+@�ff@�J@�hs@���@�@�=q@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB&�B(�B)�B(�B)�B,B1'B6FBB�BdZB�B�1B�=B�PB�PB�PB�PB�PB�PB�hB��B�-BĜB��B�/B�ZB�sB�B�B�B��B��B��B��B��B�B�B�B�B�TB�#B�#B�BB�B\B%�B2-B1'B0!B.B.B/B-B/B0!B/B0!B1'B>wBF�BM�BH�BF�BE�BF�BF�BI�B>wB2-B7LB)�B�B&�B#�B�BVB��B�B�sB�HB��B��B��B��B�B��B��B�Bs�BZB?}BhB�sB�5B��B��B�B��B�Bq�BcTBS�BD�B5?B)�B�B	7B
��B
�B
�
B
��B
��B
�^B
�XB
�B
��B
��B
�oB
�JB
�B
p�B
jB
gmB
aHB
^5B
YB
P�B
F�B
7LB
.B
(�B
&�B
"�B
�B
�B
�B
\B
B	��B	�B	�5B	��B	��B	ŢB	�^B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�JB	�B	�B	� B	z�B	r�B	cTB	<jB	!�B	�B	�B	VB�B�TB�ZB�yB�sB�HB�)B�#B��BȴB��BɺB��B��B��BȴB��B��BɺBȴBŢB��B�dB�RB�LBB��B�dB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�hB�PB�=B�7B�+B�B�B�B�B~�B|�Bz�Bw�Bu�Bs�Br�Bq�Bo�Bm�BjBhsBgmBffBdZBcTBaHB`BB_;B\)BYBXBW
BVBT�BR�BQ�BQ�BP�BO�BO�BN�BN�BO�BO�BL�BM�BL�BJ�BI�BJ�BK�BK�BJ�BJ�BJ�BI�BJ�BJ�BJ�BJ�BJ�BJ�BI�BI�BI�BI�BI�BL�BM�BL�BN�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BVBW
BW
BXBYBZBZBYBZBYBZBYBXB]/BffBhsBm�Bq�Bq�Bs�Bt�Bu�Bu�Bv�By�B{�Bw�Bu�Bw�Bv�B{�B�B�B�B�B�B�+B�JB�DB�1B�B�B�B�B�+B�7B�PB�hB��B��B��B��B��B��B�B�'B�-B�LB�dB�qB�qB�wB�}B�}B��B��B��BBÖBƨBȴBǮBǮB��B��B��B��B��B��B��B�B�B�5B�BB�NB�TB�ZB�mB�B�B��B��B��B��B��B	B	B	B	JB	oB	{B	�B	�B	�B	"�B	+B	.B	1'B	1'B	33B	7LB	>wB	B�B	F�B	J�B	E�B	D�B	E�B	F�B	I�B	K�B	H�B	H�B	I�B	J�B	N�B	T�B	YB	ZB	YB	YB	[#B	_;B	e`B	hsB	iyB	iyB	iyB	iyB	iyB	k�B	n�B	q�B	s�B	t�B	w�B	y�B	{�B	|�B	}�B	� B	�B	�%B	�+B	�+B	�1B	�1B	�1B	�7B	�7B	�=B	�PB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�-B	�-B	�'B	�!B	�3B	�?B	�FB	�9B	�9B	�9B	�3B	�3B	�LB	�dB	�jB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B&�B(�B)�B(�B)�B,B1'B6FBC�BffB�B�7B�DB�VB�PB�PB�PB�PB�VB�{B��B�9BŢB��B�5B�`B�B�B�B�B��B��B��B��B��B�B�B�B�B�fB�)B�#B�BB�BbB'�B33B2-B1'B/B0!B1'B/B/B0!B/B1'B49BB�BJ�BO�BJ�BG�BF�BF�BG�BL�BA�B33B;dB.B�B(�B&�B �B�B��B�B�B�ZB��B��B��BŢB�B�B��B�7Bz�B`BBK�B �B�B�HB�BȴB�9B��B�DBv�BiyBZBK�B9XB0!B�BPBB
�B
�B
��B
ÖB
�dB
�dB
�'B
��B
��B
��B
�uB
�=B
s�B
l�B
iyB
bNB
`BB
\)B
S�B
M�B
<jB
1'B
+B
)�B
$�B
�B
�B
�B
�B

=B	��B	��B	�HB	��B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�B	�B	�+B	}�B	v�B	k�B	C�B	"�B	�B	!�B	�B��B�TB�TB�B�B�ZB�;B�BB��B��B��B��B��B��B��B��B��B��B��B��BȴBÖB�qB�^B�RBƨBŢBÖB�LB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�\B�DB�JB�=B�+B�B�B�B�B~�B|�By�Bw�Bt�Bs�Br�Br�Bp�Bn�Bl�BhsBgmBgmBe`BcTBbNBaHB`BB]/BZBXBW
BW
BVBS�BS�BR�BR�BP�BO�BP�BQ�BQ�BQ�BP�BL�BM�BM�BL�BL�BL�BK�BM�BL�BM�BL�BL�BK�BJ�BK�BJ�BI�BJ�BK�BM�BN�BO�BO�BO�BN�BS�BS�BS�BS�BS�BS�BT�BT�BVBXBW
BW
B[#B\)B^5B\)B[#B]/B[#BZB[#B[#B^5BgmBiyBn�Br�Br�Bs�Bu�Bu�Bv�Bx�Bz�B}�Bx�Bw�B{�Bz�B}�B�B�B�B�B�B�7B�PB�PB�DB�B�B�B�B�1B�=B�PB�hB��B��B��B��B��B��B�B�-B�9B�XB�jB�wB�}B��B��B�}B��BBBBÖBƨBɺBɺBɺB��B��B��B��B��B��B��B�B�#B�;B�BB�NB�ZB�`B�sB�B�B��B��B��B��B��B	B	B	%B	PB	oB	{B	�B	�B	�B	"�B	,B	/B	1'B	2-B	33B	7LB	>wB	C�B	F�B	K�B	F�B	E�B	E�B	E�B	H�B	N�B	I�B	H�B	I�B	J�B	M�B	T�B	YB	[#B	ZB	ZB	[#B	_;B	ffB	hsB	iyB	iyB	iyB	iyB	jB	l�B	n�B	q�B	s�B	t�B	w�B	y�B	{�B	|�B	}�B	� B	�B	�%B	�+B	�+B	�1B	�1B	�7B	�7B	�7B	�=B	�PB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�-B	�-B	�3B	�'B	�3B	�FB	�LB	�?B	�9B	�?B	�9B	�-B	�FB	�dB	�jB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447212012010314472120120103144721  AO  ARGQ                                                                        20111130142248  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142248  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144721  IP                  G�O�G�O�G�O�                