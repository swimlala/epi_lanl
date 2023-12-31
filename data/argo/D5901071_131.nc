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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142142  20190522121827  1727_5046_131                   2C  D   APEX                            2143                            040306                          846 @Ծ�3Ŀ�1   @Ծ���@@7O��-V�d����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�33B�33B�  B�33B�33C �C�C�C  C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fD  D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+� D,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>  D>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A�^5A�M�A�E�A�=qA�5?A�1'A�5?A�9XA�;dA�9XA�9XA�7LA�7LA�;dA�?}A�C�A�K�A�VA�\)A�dZA�v�A�p�A�I�A�33A��A�ȴA�z�A��A��wA��uA�x�A�v�A�r�A�z�A�-A��A���A��hA�l�A�C�A�bA�%A�A��mA��HA���A��A�C�A���A���A���A���A��DA�|�A�?}A�(�A��A�%A�z�A��A��
A��RA��A���A��7A�M�A�I�A�?}A��A�VA���A��!A��A��A�A�;dA�G�A��A�t�A��jA��A�  A��A�ȴA�t�A��A�l�A�9XA�~�A�n�A�z�A�ZA�ȴA�p�A���A��DA���A���A��/A��A�-A���A�l�A�oA�A���A�A���A�I�A�|�A�-A��9A�"�A���A�A�A���A���A��`A���A���A}�FA|�A{ƨAy�Aw�hAuAq�Ao�#Am?}Ak��Ak7LAjVAh�!Af�Ad�jAb��A^jA\�9A[K�AY?}AX��AX��AW�AU|�AS�TAR�AQG�AO�7ANȴAM�AK��AK�hAJbNAI�AH�9AH-AGC�AF�9AE?}ADVAChsAC+ABM�AAl�A@�9A@��A@9XA?A=��A<�A:�+A8$�A7�PA7+A6��A5�A3�wA2n�A1dZA0��A0v�A/�hA.  A,z�A+�-A*^5A(VA&ȴA%��A$�A#�-A#�A"A�A!��A��A�hA�A��A��A=qA�Ar�AC�A%A�AbA`BA$�A��A7LAdZA�A�#A�PA%AĜA�+A�TA�A1'A�A��A�A
1A	33A��AbNA-A��A�A�7A�uA��AO�A��AA
=A 5?A {A b@�33@�7L@�\)@���@�l�@���@�j@�E�@�z�@���@�@�|�@��-@�\)@�
=@��T@�Q�@�D@�@��@�P@��@�F@�33@��@�7@��@�K�@�-@�t�@�@�Ĝ@�^5@�`B@�A�@�+@ղ-@�`B@�Z@�$�@д9@�l�@θR@�5?@�x�@���@̬@�A�@��@�{@ɡ�@�9X@��H@�$�@��m@¸R@�+@�~�@��T@��h@���@��/@�j@� �@�~�@�9X@��@�@�@��7@���@��@�=q@�V@�J@��h@�b@��
@��;@���@���@���@�$�@�-@�{@���@��m@��w@��@��@��@��\@��+@�~�@�~�@�n�@�-@���@�j@��F@���@�I�@��D@���@��j@�(�@�ƨ@�t�@�S�@�S�@�33@�+@��@��\@�$�@���@�%@�A�@���@�v�@��@�@�x�@�&�@��9@�j@�j@�z�@�j@�(�@��
@���@���@�|�@�S�@�
=@��\@�v�@��@�p�@�&�@���@���@�r�@��w@���@�ƨ@��
@��@�"�@��!@�n�@�$�@���@�/@���@�j@�A�@�b@��m@�t�@���@�v�@��@���@���@�&�@��u@�(�@��F@�\)@�o@�ȴ@�{@���@�`B@�r�@���@��F@���@�dZ@�"�@�@��R@�E�@���@�O�@��@�%@���@���@���@�9X@��@���@�M�@��@�V@���@��u@�Z@��@��F@��@��@��@��@���@�t�@��@�@�@�@���@��y@��@��R@��R@��R@��R@���@�n�@�$�@���@�`B@�O�@�%@���@��D@��u@�bN@�bN@�b@��w@��P@�dZ@�@���@�n�@�M�@�$�@��#@���@��^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A�^5A�M�A�E�A�=qA�5?A�1'A�5?A�9XA�;dA�9XA�9XA�7LA�7LA�;dA�?}A�C�A�K�A�VA�\)A�dZA�v�A�p�A�I�A�33A��A�ȴA�z�A��A��wA��uA�x�A�v�A�r�A�z�A�-A��A���A��hA�l�A�C�A�bA�%A�A��mA��HA���A��A�C�A���A���A���A���A��DA�|�A�?}A�(�A��A�%A�z�A��A��
A��RA��A���A��7A�M�A�I�A�?}A��A�VA���A��!A��A��A�A�;dA�G�A��A�t�A��jA��A�  A��A�ȴA�t�A��A�l�A�9XA�~�A�n�A�z�A�ZA�ȴA�p�A���A��DA���A���A��/A��A�-A���A�l�A�oA�A���A�A���A�I�A�|�A�-A��9A�"�A���A�A�A���A���A��`A���A���A}�FA|�A{ƨAy�Aw�hAuAq�Ao�#Am?}Ak��Ak7LAjVAh�!Af�Ad�jAb��A^jA\�9A[K�AY?}AX��AX��AW�AU|�AS�TAR�AQG�AO�7ANȴAM�AK��AK�hAJbNAI�AH�9AH-AGC�AF�9AE?}ADVAChsAC+ABM�AAl�A@�9A@��A@9XA?A=��A<�A:�+A8$�A7�PA7+A6��A5�A3�wA2n�A1dZA0��A0v�A/�hA.  A,z�A+�-A*^5A(VA&ȴA%��A$�A#�-A#�A"A�A!��A��A�hA�A��A��A=qA�Ar�AC�A%A�AbA`BA$�A��A7LAdZA�A�#A�PA%AĜA�+A�TA�A1'A�A��A�A
1A	33A��AbNA-A��A�A�7A�uA��AO�A��AA
=A 5?A {A b@�33@�7L@�\)@���@�l�@���@�j@�E�@�z�@���@�@�|�@��-@�\)@�
=@��T@�Q�@�D@�@��@�P@��@�F@�33@��@�7@��@�K�@�-@�t�@�@�Ĝ@�^5@�`B@�A�@�+@ղ-@�`B@�Z@�$�@д9@�l�@θR@�5?@�x�@���@̬@�A�@��@�{@ɡ�@�9X@��H@�$�@��m@¸R@�+@�~�@��T@��h@���@��/@�j@� �@�~�@�9X@��@�@�@��7@���@��@�=q@�V@�J@��h@�b@��
@��;@���@���@���@�$�@�-@�{@���@��m@��w@��@��@��@��\@��+@�~�@�~�@�n�@�-@���@�j@��F@���@�I�@��D@���@��j@�(�@�ƨ@�t�@�S�@�S�@�33@�+@��@��\@�$�@���@�%@�A�@���@�v�@��@�@�x�@�&�@��9@�j@�j@�z�@�j@�(�@��
@���@���@�|�@�S�@�
=@��\@�v�@��@�p�@�&�@���@���@�r�@��w@���@�ƨ@��
@��@�"�@��!@�n�@�$�@���@�/@���@�j@�A�@�b@��m@�t�@���@�v�@��@���@���@�&�@��u@�(�@��F@�\)@�o@�ȴ@�{@���@�`B@�r�@���@��F@���@�dZ@�"�@�@��R@�E�@���@�O�@��@�%@���@���@���@�9X@��@���@�M�@��@�V@���@��u@�Z@��@��F@��@��@��@��@���@�t�@��@�@�@�@���@��y@��@��R@��R@��R@��R@���@�n�@�$�@���@�`B@�O�@�%@���@��D@��u@�bN@�bN@�b@��w@��P@�dZ@�@���@�n�@�M�@�$�@��#@���@��^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB)�B)�B,B,B,B+B,B,B,B-B.B.B.B0!B1'B33B5?B7LB:^B?}BA�BH�BR�B\)B`BBbNBcTBdZBcTB^5BYBT�BR�BVBZB]/BZB`BBl�BhsBcTB^5BVBS�BR�B\)BiyBl�Bk�BjBiyBiyBiyBiyBjBl�B{�B�B�7B�DB�JB�DB�7B�1B�1B�+B�%B�%B�1B�=B�JB�JB�JB�DB�1B|�Bp�BdZBP�B/B�B��B�DBw�B\)BK�B<jB49B,B'�B�B��B�B�HB�BŢB�FB�B��B�hBx�BgmBM�B<jB2-B)�B�B
��B
�B
�B
�NB
�dB
��B
�{B
�DB
�B
}�B
w�B
p�B
e`B
S�B
C�B
+B
 �B
�B
JB
  B	��B
  B
B	��B	��B	�B	�B	�BB	�
B	��B	�?B	�hB	�uB	��B	�%B	�B	� B	|�B	ZB	O�B	O�B	H�B	A�B	=qB	8RB	49B	:^B	49B	'�B	"�B	 �B	�B	"�B	�B	 �B	�B	#�B	%�B	!�B	 �B	�B	�B	�B	\B	+B��B�B�yB�sB�ZB�BĜB�dB�FB�3B�'B�B��B��B��B��B��B��B��B��B�{B�oB�hB�PB�=B�1B�%B�%B�B�B�B~�B}�B}�B|�Bz�By�Bz�Bq�Bo�Bn�Bl�Bl�Bk�Bn�Bm�Bm�Bm�BjBiyBjBjBjBiyBgmBffBffBdZBhsBhsBffBffBffBdZBcTBbNB^5B\)B]/B^5B]/B[#BYBT�BQ�BN�BK�BG�BD�BC�BB�BA�B=qB=qB=qB=qB=qBE�BB�BC�BF�BL�BP�BP�BL�BH�BR�BVB[#B]/B\)B[#B^5B_;B`BBbNBffBffBffBjBl�Bm�Bn�Bn�Bn�Bo�Bp�Bq�Bs�Bt�Bu�Bp�Bl�BjBe`BiyBp�Bs�Bx�B|�B�B� B�B�B|�B|�B�B�B�B�%B�7B�PB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�-B�'B�!B�!B�9B�9B�^BƨBɺB��B��B��B��B�
B�B�)B�/B�/B�;B�`B�B�B�B��B��B��B	  B	B	B	B	B	B	B	B	B	B	+B	1B		7B		7B	
=B	JB	hB	bB	{B	�B	�B	�B	�B	 �B	'�B	)�B	/B	33B	5?B	7LB	9XB	=qB	>wB	B�B	C�B	E�B	G�B	H�B	J�B	J�B	L�B	Q�B	S�B	W
B	XB	\)B	`BB	e`B	hsB	jB	m�B	n�B	p�B	u�B	x�B	y�B	� B	�B	�%B	�+B	�1B	�=B	�=B	�JB	�VB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�RB	�XB	�XB	�dB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	��B	B	B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B)�B+B,B,B,B+B,B,B,B-B.B.B.B0!B1'B33B5?B7LB:^B?}BA�BH�BR�B]/B`BBbNBdZBe`Be`B`BBZBT�BR�BVBZB_;B\)B`BBm�BiyBdZB_;BVBS�BR�B\)BiyBn�Bl�Bl�BjBiyBjBiyBjBm�B|�B�B�=B�VB�VB�PB�=B�1B�1B�1B�+B�%B�1B�DB�JB�PB�PB�JB�=B}�Br�BhsBT�B8RB��B�B�bB�BaHBQ�B?}B6FB-B,B%�BB�B�ZB�5B��B�RB�-B��B��B}�Bq�BT�B?}B49B0!B!�BB
�B
�B
�B
B
�B
��B
�PB
�%B
~�B
y�B
s�B
jB
[#B
L�B
.B
"�B
 �B
oB
1B
B
%B
DB
  B	��B	�B	�B	�sB	�)B	��B	��B	��B	��B	��B	�+B	�B	�B	�B	]/B	R�B	R�B	L�B	C�B	@�B	<jB	5?B	=qB	7LB	(�B	#�B	"�B	 �B	&�B	"�B	#�B	�B	&�B	(�B	#�B	 �B	 �B	 �B	�B	oB	VB��B�B�B�yB�mB�;BȴB�wB�RB�9B�9B�3B�B��B��B��B��B��B��B��B��B��B�uB�uB�\B�7B�%B�+B�B�B�B�B~�B~�B~�B|�B}�B~�Bv�Bu�Bp�Bo�Bm�Bm�Bo�Bn�Bo�Bp�Bm�Bk�BjBl�Bn�Bl�BiyBgmBgmBiyBjBjBiyBiyBhsBgmBffBe`BaHB]/B]/B`BBaHB^5B^5BXBT�BP�BO�BJ�BE�BD�BC�BD�BA�B>wB?}B@�B=qBH�BB�BB�BF�BM�BQ�BQ�BN�BK�BS�BYB^5B_;B]/B^5B_;BaHBaHBdZBgmBhsBiyBl�Bn�Bn�Bo�Bo�Bo�Bp�Bq�Bs�Bt�Bu�Bw�Br�Bm�Bn�BgmBiyBq�Bt�By�B|�B�B�B�B�B�B~�B�B�B�B�%B�7B�PB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�B�!B�'B�-B�-B�'B�'B�-B�?B�9B�XBƨBɺB��B��B��B�B�
B�B�)B�/B�5B�BB�fB�B�B�B��B��B��B	  B	B	B	B	B	B	B	B	B	%B	+B	1B		7B		7B	DB	PB	hB	hB	�B	�B	�B	�B	�B	!�B	'�B	)�B	/B	49B	6FB	8RB	:^B	>wB	?}B	C�B	D�B	F�B	G�B	H�B	J�B	K�B	M�B	Q�B	S�B	W
B	YB	]/B	aHB	ffB	iyB	k�B	m�B	n�B	q�B	v�B	y�B	z�B	�B	�B	�%B	�+B	�1B	�=B	�=B	�PB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�RB	�XB	�XB	�jB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	��B	B	ÖB	B	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447202012010314472020120103144720  AO  ARGQ                                                                        20111130142142  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142142  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144720  IP                  G�O�G�O�G�O�                