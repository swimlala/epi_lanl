CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:46Z UW 3.1 conversion   
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143420  20190522121828  1727_5046_198                   2C  D   APEX                            2143                            040306                          846 @��C���1   @���$�@6��"��`�c�Ƨ1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyٚD�6fD�` D��fD��3D�)�D�c3D���D�ٚD�fD�` D���D��3D�0 D�L�Dڬ�D���D�fD�ffD�3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�33A��A!��AA��Ac33A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^  C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn33Cp�Cr�Ct�Cv  Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%� D&fD&�fD'fD'�fD(fD(�fD)fD)�fD*�D*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4  D4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ��DK�DK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ��DRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDd  Dd�fDefDe�fDffDf�fDgfDg�fDh  Dh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn��Do�Do�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDts3Dy� D�9�D�c3D���D��fD�,�D�ffD�� D���D��D�c3D���D��fD�33D�P Dڰ D�� D��D�i�D�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��+A�Q�A��A���A��A�x�A�l�A�`BA�VA�I�A�A�A�=qA�33A�+A��A�bA�
=A���A���A�
=A��DA�~�A�l�A�C�A��!A�?}A��
A���A�dZA�E�A��A��DA�VA� �A�bA�A���A��mA���A��^A��FA��RA��wA�ƨA�ȴA���A���A�ƨA��jA���A�v�A�;dA��+A���A���A�ZA��A�A�A�A�ƨA�dZA�M�A�~�A��/A�jA��jA�oA��PA�`BA�C�A�l�A��hA��hA��A���A�\)A���A�\)A��FA�5?A�ƨA���A�dZA��DA��;A�ȴA��A��;A��
A��A�A�33A��!A���A���A�hsA�G�A��A��/A�M�A��A��`A��A�G�A��+A��!A��TA���A�
A&�A~�9A}��A{K�Ay+Av$�Aq��Ao
=Al��Ak�Aj$�AeoA`��A_?}A^�A^��A]�A\AYx�AV�+AT�uAT�9AS&�APv�AOl�AM��AM�AK�PAJ5?AH�+AH�AG"�AE�ADr�ADJAC|�AA�PA?p�A=��A=p�A<��A;x�A9C�A8�A8�A8Q�A7A6�+A4�/A3�A3;dA3�A2z�A0�A.�A-dZA,�A)�
A)`BA)oA(�HA(ĜA(��A'��A'VA&ĜA&v�A&VA&$�A%�#A%
=A#�FA"~�A"-A!�A!|�A �HAƨA��A&�A�7A��A5?AĜA�A�A��AA�AĜA5?A~�A�A-Ax�AVA
�\AJA�A��A(�A\)Ar�A`BA -@��R@���@��9@��P@�v�@�@��u@�t�@�"�@�5?@��j@�@��`@�ȴ@�5?@���@�F@�E�@��T@�h@�Q�@�S�@�!@�v�@��#@噚@��@�Ĝ@��m@�+@�J@�@�l�@ޟ�@��@�b@��
@�dZ@�n�@ܴ9@ݡ�@�?}@�j@�t�@�@���@�V@�X@ԣ�@ӝ�@�v�@��;@�`B@�j@� �@ʏ\@�7L@���@Ǿw@ư!@���@��H@��@��H@�v�@��#@�7L@�Ĝ@ēu@�9X@��
@�dZ@�$�@�hs@��`@��@�I�@��m@�v�@�7L@��D@�  @�|�@��@�hs@��`@��@�I�@��@�t�@�~�@��@��`@��@�dZ@�ȴ@�ff@�J@�7L@���@�\)@���@��^@�V@��/@�r�@��w@�l�@�;d@��H@���@��R@��+@�ff@�ff@�^5@�ff@�J@���@��^@���@�p�@�hs@�`B@�`B@�`B@�`B@�/@��u@��m@���@��P@��H@�V@���@�hs@�O�@�X@��@��j@��@���@�r�@��@�t�@��#@���@�p�@���@���@� �@�ƨ@���@���@���@��@�|�@�S�@�33@�"�@��H@�~�@�V@�E�@�{@��T@���@��^@���@��@���@��D@��@�r�@�r�@�r�@�Z@���@��+@��@��-@���@���@�O�@��@���@���@�A�@��P@�n�@��@��-@�`B@�7L@�/@�&�@��@�%@��u@��@���@��@���@���@��P@�l�@�C�@��@���@�^5@���@�?}@���@�j@��j@���@�V@��`@��9@�j@��w@�\)@��@�t�@�t�@�\)@���@��@���@��7@�p�@�G�@�V@��@�r�@� �@��@��m@��;@��m@��@��@���@��F@��@�S�@�K�@�;d@�@���@�V@��@��@��#@���@�@��-@���@��7@��@�x�@�p�@�p�@�hs@�G�@�7L@�&�@�%@��`@��/@���@��@�9X@���@���@��^@w\)@k@_;d@Yx�@P  @I��@F5?@=�@7��@2�@-V@(bN@#�m@��@��@��@��@�#@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��+A�Q�A��A���A��A�x�A�l�A�`BA�VA�I�A�A�A�=qA�33A�+A��A�bA�
=A���A���A�
=A��DA�~�A�l�A�C�A��!A�?}A��
A���A�dZA�E�A��A��DA�VA� �A�bA�A���A��mA���A��^A��FA��RA��wA�ƨA�ȴA���A���A�ƨA��jA���A�v�A�;dA��+A���A���A�ZA��A�A�A�A�ƨA�dZA�M�A�~�A��/A�jA��jA�oA��PA�`BA�C�A�l�A��hA��hA��A���A�\)A���A�\)A��FA�5?A�ƨA���A�dZA��DA��;A�ȴA��A��;A��
A��A�A�33A��!A���A���A�hsA�G�A��A��/A�M�A��A��`A��A�G�A��+A��!A��TA���A�
A&�A~�9A}��A{K�Ay+Av$�Aq��Ao
=Al��Ak�Aj$�AeoA`��A_?}A^�A^��A]�A\AYx�AV�+AT�uAT�9AS&�APv�AOl�AM��AM�AK�PAJ5?AH�+AH�AG"�AE�ADr�ADJAC|�AA�PA?p�A=��A=p�A<��A;x�A9C�A8�A8�A8Q�A7A6�+A4�/A3�A3;dA3�A2z�A0�A.�A-dZA,�A)�
A)`BA)oA(�HA(ĜA(��A'��A'VA&ĜA&v�A&VA&$�A%�#A%
=A#�FA"~�A"-A!�A!|�A �HAƨA��A&�A�7A��A5?AĜA�A�A��AA�AĜA5?A~�A�A-Ax�AVA
�\AJA�A��A(�A\)Ar�A`BA -@��R@���@��9@��P@�v�@�@��u@�t�@�"�@�5?@��j@�@��`@�ȴ@�5?@���@�F@�E�@��T@�h@�Q�@�S�@�!@�v�@��#@噚@��@�Ĝ@��m@�+@�J@�@�l�@ޟ�@��@�b@��
@�dZ@�n�@ܴ9@ݡ�@�?}@�j@�t�@�@���@�V@�X@ԣ�@ӝ�@�v�@��;@�`B@�j@� �@ʏ\@�7L@���@Ǿw@ư!@���@��H@��@��H@�v�@��#@�7L@�Ĝ@ēu@�9X@��
@�dZ@�$�@�hs@��`@��@�I�@��m@�v�@�7L@��D@�  @�|�@��@�hs@��`@��@�I�@��@�t�@�~�@��@��`@��@�dZ@�ȴ@�ff@�J@�7L@���@�\)@���@��^@�V@��/@�r�@��w@�l�@�;d@��H@���@��R@��+@�ff@�ff@�^5@�ff@�J@���@��^@���@�p�@�hs@�`B@�`B@�`B@�`B@�/@��u@��m@���@��P@��H@�V@���@�hs@�O�@�X@��@��j@��@���@�r�@��@�t�@��#@���@�p�@���@���@� �@�ƨ@���@���@���@��@�|�@�S�@�33@�"�@��H@�~�@�V@�E�@�{@��T@���@��^@���@��@���@��D@��@�r�@�r�@�r�@�Z@���@��+@��@��-@���@���@�O�@��@���@���@�A�@��P@�n�@��@��-@�`B@�7L@�/@�&�@��@�%@��u@��@���@��@���@���@��P@�l�@�C�@��@���@�^5@���@�?}@���@�j@��j@���@�V@��`@��9@�j@��w@�\)@��@�t�@�t�@�\)@���@��@���@��7@�p�@�G�@�V@��@�r�@� �@��@��m@��;@��m@��@��@���@��F@��@�S�@�K�@�;d@�@���@�V@��@��@��#@���@�@��-@���@��7@��@�x�@�p�@�p�@�hs@�G�@�7L@�&�@�%@��`@��/@���@��@�9X@���@���@��^@w\)@k@_;d@Yx�@P  @I��@F5?@=�@7��@2�@-V@(bN@#�m@��@��@��@��@�#@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�ZB�B�BJB�B:^BJ�BT�BO�BP�BP�BQ�BQ�BQ�BXB^5BaHBbNBffBiyBl�Bp�Bs�Br�Bo�Bl�Be`BM�B=qB5?B2-B(�B\B��B��B�5B��BĜB�dB�FB�!B��B��B��B�BȴB�BB�fB�yB�#B��B��BɺBB�B��B�\B�BXB%�B�B�`B�`B��B�B��B�DBz�Bm�BQ�BM�BL�BG�B�BB
�B
��B
��B
��B
�=B
{�B
jB
M�B
D�B
@�B
=qB
49B
#�B
�B
B	�B	�TB	�B	��B	��B	�^B	�B	��B	��B	��B	��B	�oB	|�B	iyB	[#B	dZB	T�B	=qB	7LB	0!B	)�B	 �B	�B	hB	uB	�B	�B	�B	�B	oB	DB	%B	B	B	B	B��B�B�B�B�B�`B�;B�)B�B�B�B��BĜB�}B�XB�9B�-B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�DB�7B�+B�B}�Bz�Bx�Bu�Bt�Br�Bn�Br�Bl�Bl�BiyBdZBffB_;BXBW
BVBW
BS�BT�BW
BQ�BQ�BQ�BR�BQ�BQ�BR�BVBVBVBYB`BB`BBaHB_;BZBS�BO�BM�BL�BL�BM�BN�BP�BQ�BR�BT�BT�BW
BXBZB]/B_;BaHBbNBdZBgmBjBl�B�B�\B�bB�PB�=B�1B�bB��B��B��B�uB�hB�DB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�XB��BŢBƨBǮBǮB��B��B�
B�/B�/B�;B�HB�ZB�`B�`B�ZB�`B�sB�B�B�B�B�B�B��B��B��B	B	%B	
=B	bB	bB	VB	PB	JB	DB	DB	DB	DB	PB	VB	VB	\B	bB	{B	�B	�B	�B	�B	�B	"�B	"�B	"�B	"�B	$�B	)�B	0!B	2-B	1'B	6FB	:^B	<jB	?}B	@�B	?}B	A�B	C�B	C�B	B�B	B�B	C�B	F�B	M�B	N�B	O�B	R�B	T�B	VB	XB	YB	YB	[#B	\)B	]/B	^5B	`BB	`BB	bNB	e`B	gmB	gmB	hsB	jB	k�B	l�B	l�B	m�B	n�B	n�B	n�B	o�B	o�B	p�B	r�B	r�B	p�B	r�B	t�B	t�B	t�B	v�B	v�B	w�B	z�B	� B	�B	�B	�1B	�7B	�DB	�JB	�JB	�JB	�JB	�JB	�\B	�hB	�oB	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�FB	�FB	�9B	�-B	�9B	�RB	�XB	�^B	�^B	�XB	�^B	�^B	�^B	�^B	�dB	�dB	�qB	�}B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�;B	�BB	�HB	�B
B
VB
�B
#�B
(�B
,B
/B
7LB
=qB
C�B
G�B
N�B
R�B
ZB
\)B
`BB
cTB
hsB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�5B�`B�B�BJB�B:^BK�BVBO�BP�BP�BQ�BQ�BQ�BXB^5BaHBbNBffBiyBl�Bp�Bs�Br�Bp�Bm�BhsBP�B?}B6FB49B-BoB��B��B�TB��BǮB�qB�XB�3B��B��B��B��BǮB�BB�fB�B�HB��B��B��B��B�FB��B�oB�VBbNB6FB�B�mB�B�B�FB��B�bB~�Bu�BR�BN�BO�BS�B$�B1B
��B
�B
�B
��B
�\B
�B
x�B
Q�B
F�B
B�B
A�B
<jB
,B
�B
hB	��B	�B	�B	��B	�)B	ĜB	�!B	��B	��B	��B	��B	��B	�B	n�B	[#B	hsB	\)B	@�B	;dB	2-B	.B	$�B	�B	uB	�B	!�B	�B	�B	�B	�B	uB	JB	%B	%B	B	
=B��B�B�B�B�B�yB�NB�/B�B�B�B��BǮBÖB�}B�?B�3B�-B�!B�!B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�DB�7B�+B� B}�B|�Bz�Bx�Bt�Bs�Bw�Bn�Bn�Bl�BgmBk�BcTBZBYBXBYBW
BXBYBS�BR�BS�BT�BR�BS�BT�BW
BXBXB\)BcTBcTBbNBaHB\)BVBP�BN�BN�BN�BN�BO�BQ�BR�BS�BVBVBXBZB\)B_;B`BBbNBe`BdZBhsBk�BiyB�B�bB�hB�\B�PB�DB�uB��B��B��B��B��B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�FB�^B��BƨBƨBȴBɺB��B��B�B�5B�BB�BB�NB�`B�`B�fB�`B�mB�yB�B�B�B�B�B��B��B��B��B	B	1B	DB	bB	hB	\B	VB	JB	JB	DB	DB	DB	PB	VB	VB	\B	hB	{B	�B	�B	�B	�B	�B	"�B	"�B	"�B	"�B	%�B	+B	0!B	2-B	2-B	7LB	;dB	=qB	?}B	@�B	@�B	A�B	C�B	C�B	B�B	C�B	D�B	F�B	M�B	N�B	P�B	S�B	VB	W
B	XB	YB	YB	[#B	\)B	]/B	^5B	`BB	aHB	cTB	e`B	gmB	gmB	hsB	jB	k�B	l�B	m�B	n�B	n�B	n�B	n�B	o�B	o�B	p�B	t�B	t�B	q�B	s�B	t�B	t�B	u�B	v�B	v�B	x�B	{�B	�B	�B	�B	�1B	�=B	�DB	�JB	�JB	�JB	�JB	�PB	�bB	�hB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�FB	�FB	�?B	�3B	�9B	�RB	�XB	�^B	�dB	�^B	�dB	�^B	�^B	�^B	�dB	�jB	�qB	��B	��B	��B	B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�BB	�BB	�HB	�B
B
VB
�B
#�B
(�B
,B
/B
7LB
=qB
C�B
G�B
N�B
R�B
ZB
\)B
`BB
cTB
gmB
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447432012010314474320120103144743  AO  ARGQ                                                                        20111130143420  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143420  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144743  IP                  G�O�G�O�G�O�                