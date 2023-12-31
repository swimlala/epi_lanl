CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:24Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               wA   AO  20111130141856  20190522121826  1727_5046_119                   2C  D   APEX                            2143                            040306                          846 @ԯyr�@
1   @ԯzQ��@6�\(���d�-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B?��BG��BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D\��D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DyY�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�33A��A!��AA��Aa��A���A���A���A���A���Aљ�A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@  BH  BPffBX��B`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@33CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD��DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��D fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1  D1� D2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\� D]  D]� D^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm� DnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDy` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��`A��`A��`A��yA��A��A��A��A��A��A��A��A���A���A���A���A��A��yA��A��A��A��A��A��A��A��A��A��A��A��#A���A���A�`BA��jA�&�A��A���A�A���A���A�S�A��!A�ĜA��jA�5?A�G�A�"�A�ĜA��!A�7LA�~�A��\A�ffA�7LA��!A�VA�ffA�ĜA��A���A�"�A�bA���A���A�x�A�hsA�I�A���A���A��yA�A�(�A��;A��A�M�A��
A��A�
=A�ĜA�+A���A��A�VA���A��9A���A���A��DA�K�A��!A���A��+A��A��\A�-A��A���A�XA�hsA�%A��+A��
A};dAy�hAx�Av-As�Aq�ApAm33Al  Ak`BAj�jAi;dAe�
A`ĜA\�AYG�AV�+AVQ�AV$�AU�FAR�AL�/AJ�yAJA�AIAIoAHjAG��AGoAD��AB-AA�ABAAt�A@�jA?�A?C�A>bA<��A;l�A9�PA8{A7+A733A6�A5�FA5dZA4ȴA3�wA3|�A2ffA1�#A1?}A/;dA.��A-S�A+C�A*�RA)/A'�^A& �A$�A$A#�^A#��A#|�A#O�A"9XA!��A!�A $�AA�uA��A�A��A��AVA�
A�A`BA��A�AhsA�A��A�A��A��A��A��AĜA�wA�A��AO�A
ZA
VA
�A	x�A��A�jA��AJA/AQ�A�wA�A��A=qA�A1A r�@�-@�b@���@�/@���@�(�@�o@���@��@�M�@�j@�A�@���@�|�@��H@�x�@�@��@�G�@�1'@�R@���@�`B@���@��;@�n�@�p�@���@�Ĝ@��@�dZ@�5?@���@�X@܃@�=q@؛�@ו�@��@�V@�1'@мj@��;@��;@Χ�@�7L@̛�@���@ˮ@˕�@˕�@ˍP@�5?@�p�@ȋD@�S�@��y@�E�@�(�@�@�V@��@���@���@�1@�;d@�ȴ@���@�G�@��@��/@��j@�I�@�+@�E�@��@��@��u@���@���@�t�@�33@��+@��9@��`@��@���@��y@��\@��@��h@��@��D@�Q�@��F@�o@�v�@�@�/@�9X@�  @���@�C�@�+@���@�v�@�E�@��^@�x�@�?}@�7L@�7L@�7L@���@��9@���@��@�Z@���@��F@�ƨ@�ƨ@�|�@�K�@�@���@�M�@��@�{@���@��@���@�b@�  @��@��;@��@��@�M�@��@��@��@�7L@���@�1'@��;@��
@��
@��w@��H@�=q@��-@��h@�7L@��9@�1@�|�@�S�@��y@��@�Z@�1@��F@��@�|�@�\)@�;d@�;d@�;d@�;d@��y@���@��!@�E�@��T@��@�J@���@��@��-@��@��/@��@���@���@�t�@�+@���@��H@�=q@�x�@��@���@���@���@�Ĝ@��@��u@�I�@��@�S�@�C�@�C�@�+@�
=@��@���@��@�ȴ@���@��+@�~�@�ff@�E�@���@�/@��@���@��@���@���@���@�Ĝ@��9@��9@���@�Z@�1@�\)@��@��\@�n�@�E�@��#@��-@���@��h@�?}@�%@��/@��u@��@�r�@�9X@���@��P@��@���@���@�~�@�@���@��h@�x�@���@��-@�O�@��@�Ĝ@�(�@��
@���@�|�@�+@���@�V@�$�@��#@���@�Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��`A��`A��`A��yA��A��A��A��A��A��A��A��A���A���A���A���A��A��yA��A��A��A��A��A��A��A��A��A��A��A��#A���A���A�`BA��jA�&�A��A���A�A���A���A�S�A��!A�ĜA��jA�5?A�G�A�"�A�ĜA��!A�7LA�~�A��\A�ffA�7LA��!A�VA�ffA�ĜA��A���A�"�A�bA���A���A�x�A�hsA�I�A���A���A��yA�A�(�A��;A��A�M�A��
A��A�
=A�ĜA�+A���A��A�VA���A��9A���A���A��DA�K�A��!A���A��+A��A��\A�-A��A���A�XA�hsA�%A��+A��
A};dAy�hAx�Av-As�Aq�ApAm33Al  Ak`BAj�jAi;dAe�
A`ĜA\�AYG�AV�+AVQ�AV$�AU�FAR�AL�/AJ�yAJA�AIAIoAHjAG��AGoAD��AB-AA�ABAAt�A@�jA?�A?C�A>bA<��A;l�A9�PA8{A7+A733A6�A5�FA5dZA4ȴA3�wA3|�A2ffA1�#A1?}A/;dA.��A-S�A+C�A*�RA)/A'�^A& �A$�A$A#�^A#��A#|�A#O�A"9XA!��A!�A $�AA�uA��A�A��A��AVA�
A�A`BA��A�AhsA�A��A�A��A��A��A��AĜA�wA�A��AO�A
ZA
VA
�A	x�A��A�jA��AJA/AQ�A�wA�A��A=qA�A1A r�@�-@�b@���@�/@���@�(�@�o@���@��@�M�@�j@�A�@���@�|�@��H@�x�@�@��@�G�@�1'@�R@���@�`B@���@��;@�n�@�p�@���@�Ĝ@��@�dZ@�5?@���@�X@܃@�=q@؛�@ו�@��@�V@�1'@мj@��;@��;@Χ�@�7L@̛�@���@ˮ@˕�@˕�@ˍP@�5?@�p�@ȋD@�S�@��y@�E�@�(�@�@�V@��@���@���@�1@�;d@�ȴ@���@�G�@��@��/@��j@�I�@�+@�E�@��@��@��u@���@���@�t�@�33@��+@��9@��`@��@���@��y@��\@��@��h@��@��D@�Q�@��F@�o@�v�@�@�/@�9X@�  @���@�C�@�+@���@�v�@�E�@��^@�x�@�?}@�7L@�7L@�7L@���@��9@���@��@�Z@���@��F@�ƨ@�ƨ@�|�@�K�@�@���@�M�@��@�{@���@��@���@�b@�  @��@��;@��@��@�M�@��@��@��@�7L@���@�1'@��;@��
@��
@��w@��H@�=q@��-@��h@�7L@��9@�1@�|�@�S�@��y@��@�Z@�1@��F@��@�|�@�\)@�;d@�;d@�;d@�;d@��y@���@��!@�E�@��T@��@�J@���@��@��-@��@��/@��@���@���@�t�@�+@���@��H@�=q@�x�@��@���@���@���@�Ĝ@��@��u@�I�@��@�S�@�C�@�C�@�+@�
=@��@���@��@�ȴ@���@��+@�~�@�ff@�E�@���@�/@��@���@��@���@���@���@�Ĝ@��9@��9@���@�Z@�1@�\)@��@��\@�n�@�E�@��#@��-@���@��h@�?}@�%@��/@��u@��@�r�@�9X@���@��P@��@���@���@�~�@�@���@��h@�x�@���@��-@�O�@��@�Ĝ@�(�@��
@���@�|�@�+@���@�V@�$�@��#@���@�Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�\B�+B� B� B|�By�Bx�Bq�Bk�BcTBcTB`BBs�B�B�+B�=B�By�Bq�B~�B�%Bs�Bm�Bm�BhsB]/BXBS�BN�BE�B:^B1'B.B,B'�B �B �BuB��B��B�B�BB��Bz�B`BBG�B�B
�fB
ǮB
�RB
�!B
�B
�B
�B
�B
�B
��B
�DB
y�B
y�B
{�B
z�B
|�B
� B
z�B
y�B
l�B
XB
L�B
@�B
$�B
B	�B	�B	ĜB	�!B	��B	��B	��B	�oB	�VB	�%B	bNB	6FB	"�B	uB	B	B	B	B��B�B�sB�fB�TB�;B�/B�B�B��BÖB��B��B�B��B��BɺBB�XB�B��B��B��B��B��B�B�3B�XB�LB�LB�RB�FB�?B�3B�-B�B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�VB�JB�=B�1B�+B�B�B�B� B}�B{�Bz�Bx�Bw�Bv�Bt�Bs�Bq�Bp�Bn�Bl�Bk�BiyBhsBffBdZBcTBcTBbNBaHBaHBaHB`BB_;B^5B^5B]/B]/B\)B[#B[#B[#B[#B[#B[#B]/B]/B]/B]/B_;BdZBcTBdZBe`Be`BdZBdZBbNBaHBbNBdZBdZBcTBe`BffBffBe`Be`BdZBffBl�Bs�By�B{�B|�B{�B{�B{�B}�B~�B� B}�B{�Bx�Bz�B{�B{�B}�B�B�B�B�+B�1B�1B�+B�1B�+B�%B�+B�+B�%B�+B�=B�PB�bB�bB�bB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B�!B�9B�FB�FB�?B�FB�jB�}B��B��B��B�}B�}BÖBƨB��B��B��B��B��B�B�HB�`B�mB�yB�yB�B�B�B��B��B��B��B	  B	B	1B	PB	\B	hB	oB	�B	�B	�B	 �B	!�B	"�B	"�B	#�B	$�B	%�B	$�B	%�B	&�B	+B	/B	/B	/B	/B	2-B	5?B	:^B	;dB	<jB	>wB	?}B	A�B	D�B	F�B	F�B	F�B	E�B	G�B	I�B	M�B	N�B	P�B	R�B	VB	YB	ZB	YB	ZB	\)B	]/B	^5B	`BB	`BB	aHB	dZB	hsB	jB	l�B	o�B	q�B	r�B	w�B	x�B	z�B	{�B	{�B	{�B	{�B	{�B	~�B	�B	�B	�+B	�1B	�DB	�DB	�JB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�dB	�wB	�}B	B	ĜB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	��B	��B	��B	��B	�
B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�oB�hB�7B�%B�%B�B{�Bs�Bp�BffBgmBbNBs�B�B�PB�\B�%B|�Bq�B� B�DBv�Bp�Bu�Bp�BaHBZBW
BT�BM�B?}B2-B.B-B)�B!�B#�B�B��B��B��B�B��B�Be`BQ�B&�B
�B
��B
�dB
�3B
�B
�B
�B
�B
�B
�'B
��B
z�B
|�B
}�B
|�B
}�B
�B
� B
|�B
q�B
ZB
O�B
J�B
.B
%B	��B	�)B	��B	�9B	�B	��B	��B	�{B	�oB	�\B	o�B	A�B	+B	�B	%B	%B	%B		7B	1B�B�B�sB�`B�HB�;B�)B�HB�BĜB��B�B�B�
B��B��BǮB�qB�?B��B��B��B��B�B�B�?B�dB�RB�^B�XB�RB�^B�?B�?B�-B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�hB�VB�JB�DB�7B�=B�B�B�B�B~�B|�Bz�Bz�By�Bv�Bu�Bt�Bs�Br�Bo�Bn�Bl�BiyBjBgmBcTBdZBdZBbNBbNBaHBaHBaHB`BB_;B^5B_;B]/B^5B]/B_;B_;B^5B^5B^5B^5B^5B_;BaHBe`BhsBgmBe`BffBe`Be`BdZBcTBe`BffBffBffBgmBgmBgmBgmBhsBffBgmBl�Bt�B{�B}�B}�B|�B}�B� B�B�B�B� B� B~�B|�B{�B{�B� B�B�B�B�+B�1B�1B�7B�7B�1B�1B�1B�1B�7B�7B�=B�PB�hB�hB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�LB�LB�RB�FB�dBBBB��B��B��BÖBǮB��B��B��B��B��B�B�NB�fB�mB�yB�yB�B�B��B��B��B��B��B	  B	B	1B	PB	\B	hB	uB	�B	�B	�B	!�B	!�B	#�B	#�B	$�B	$�B	%�B	%�B	&�B	'�B	,B	/B	/B	/B	0!B	2-B	5?B	:^B	;dB	=qB	?}B	@�B	B�B	E�B	F�B	F�B	F�B	G�B	H�B	J�B	M�B	O�B	Q�B	S�B	W
B	YB	[#B	\)B	[#B	]/B	^5B	^5B	`BB	`BB	aHB	dZB	hsB	jB	m�B	o�B	q�B	s�B	x�B	x�B	z�B	{�B	{�B	{�B	{�B	|�B	� B	�B	�B	�+B	�7B	�DB	�DB	�PB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�dB	�}B	��B	ÖB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�
B	�B	�
B	�
B	�B	�B	��B	��B	�
B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447152012010314471520120103144715  AO  ARGQ                                                                        20111130141856  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141856  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144715  IP                  G�O�G�O�G�O�                