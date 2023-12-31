CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:41Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143537  20190522121828  1728_5048_007                   2C  D   APEX                            2142                            040306                          846 @�<hoO��1   @�<i��@3��Q��c�~��"�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @��@�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A���A�  A�33B   B  B  B  B   B(  B0ffB8ffB@ffBHffBPffBX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�33B�33B�  B���B�  B�  B�  C   C�fC  C  C  C
  C  C  C�fC�C�C�fC�fC  C�C�C �C"  C$�C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>�C@  CB  CD  CF  CG�fCJ  CL  CM�fCO�fCQ�fCS�fCU�fCX  CZ  C\  C^�C`  Ca�fCd  Cf�Ch  Cj  Cl  Cm�fCo�fCq�fCt�Cv  Cw�fCz  C|  C}�fC�  C��C��C��C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C��C�  C�  C�  C��3C��3C�  C��C��C��C��C�  C�  C��C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C��3C��3C��C��C�  C��3C��3C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D�fD��D� D  D� DfDy�D��D� DfD� D��D� D  D� D��D	� D
  D
� D
��D� DfD� D��D� D  Dy�D  D� D��D� D  D� DfD� D  D�fDfD�fD  Dy�D  D�fD  D� D��D� DfD� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!y�D!��D"y�D#  D#�fD$fD$� D%  D%� D&  D&� D'  D'y�D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/y�D0  D0�fD1  D1y�D2  D2� D3  D3� D4  D4�fD5  D5y�D6  D6� D7fD7� D7��D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@y�D@��DA� DBfDB�fDCfDC� DD  DD� DEfDEy�DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DTy�DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�D`��Day�Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Du  Du� Dv  Dv� Dw  Dw�fDy��D��D�@ D�VfD�� D��fD�@ D�` D�� D���D�  D�i�Dǰ D��D�33D�|�D� D���D�&fD�` D�Ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@s33@���@���A��A<��A\��A|��A�ffA�ffA�33A�ffA�33A�ffA홚A�ffB33B33B33B33B'33B/��B7��B?��BG��BO��BW33B_33Bg33Bo��Bw33B33B���B���B���B���B���B���B�ffB�ffB���B���B���B���B���B���B���B���BÙ�BǙ�B�ffBϙ�B���Bי�Bۙ�Bߙ�B���B���B뙚B�ffB�B���B���B���C�3C��C��C��C	��C��C��C�3C�fC�fC�3C�3C��C�fC�fC�fC!��C#�fC%��C'��C)��C+��C-��C/��C1��C3�fC5��C7��C9��C;��C=�fC?��CA��CC��CE��CG�3CI��CK��CM�3CO�3CQ�3CS�3CU�3CW��CY��C[��C]�fC_��Ca�3Cc��Ce�fCg��Ci��Ck��Cm�3Co�3Cq�3Cs�fCu��Cw�3Cy��C{��C}�3C��C��3C��3C��3C��fC��fC��fC��fC�ٚC��fC��3C��fC��fC��fC�ٚC��fC��3C��3C��3C��fC��fC��fC��fC�ٚC�ٚC��fC��fC�ٚC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��fC�ٚC�ٚC��fC��3C��fC��fC��fC��3C��fC��fC��fC�ٚC�ٚC��fC��3C��3C��3C��3C��fC��fC��3C��3C��fC�ٚC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��3C�ٚC�ٚC��3C��3C��fC�ٚC�ٚC��fC�ٚC��fC��fC��fC��fC��3C��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��3C��fC��fC��fC��fC�ٚC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fD s3D �3Dy�D��Ds3D�3Ds3D��Dl�D��Ds3D��Ds3D��Ds3D�3Ds3D��D	s3D	�3D
s3D
��Ds3D��Ds3D��Ds3D�3Dl�D�3Ds3D��Ds3D�3Ds3D��Ds3D�3Dy�D��Dy�D�3Dl�D�3Dy�D�3Ds3D��Ds3D��Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!l�D!��D"l�D"�3D#y�D#��D$s3D$�3D%s3D%�3D&s3D&�3D'l�D'��D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-l�D-�3D.s3D.�3D/l�D/�3D0y�D0�3D1l�D1�3D2s3D2�3D3s3D3�3D4y�D4�3D5l�D5�3D6s3D6��D7s3D7��D8s3D8�3D9s3D9�3D:y�D:�3D;s3D;�3D<s3D<��D=s3D=�3D>s3D>�3D?s3D?�3D@l�D@��DAs3DA��DBy�DB��DCs3DC�3DDs3DD��DEl�DE�3DFy�DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOy�DO�3DPs3DP�3DQs3DQ�3DRy�DR�3DSs3DS�3DTl�DT�3DUs3DU�3DVl�DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`l�D`��Dal�Da��Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df��Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dml�Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds��Dtl�Dt�3Dus3Du�3Dvs3Dv�3Dwy�Dy��D�fD�9�D�P D���D�� D�9�D�Y�D�ɚD��3D��D�c3Dǩ�D��3D�,�D�vfD���D��3D�  D�Y�D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A�  A�  A���A���A���AҸRAҕ�A�z�A�O�A��A��/AѴ9Aѕ�Aщ7AхAсA�~�A�t�A�p�A�n�A�jA�hsA�dZA�bNA�`BA�ZA�VA�Q�A�Q�A�Q�A�O�A�M�A�M�A�K�A�G�A�E�A�A�A�;dA�/A��A�A�r�A�;dA�hsA�hsA��+A�S�A�+A�XA�l�A�A�A�A�A�5?A�l�A�  A��A�jA�
=A���A�VA��A��A��+A�^5A��A�`BA�p�A���A���A��A�?}A��RA�ȴA��FA��hA�bA��TA��
A�p�A��TA�=qA���A�33A�;dA�jA��TA�`BA��RA�+A�ffA�%A�ȴA�hsA�%A��A��A��A��A�`BA�5?A�C�A���A�
=A���A�E�A�A��DA�=qA�5?A���A��^A��Az�+Au��AtVAs�Ar�Ap�AnQ�Al{Ai�hAh  Ah��Ahz�Af=qAa��A`  A^jA]
=A\ffA[&�AYdZAX�AW�AW
=AU&�AQ&�AMC�AJ��AG�#AD�/AB��A@��A>�A;;dA8�A7dZA5�TA3O�A1��A0��A/��A.��A-�A*��A*bA)�wA)�A(�`A'��A%��A$�A$(�A#�7A!�TA�TAA�RA1'A��A%AJA��AVA��AK�AjA�
A�+A+A�A$�A9XA�A;dA	C�A�-A�!A+A�A?}A�A�A��A?}A �!A ^5A 9XA @��@���@�7L@�9X@�S�@��@�M�@��@�S�@�Q�@�n�@�=q@�-@�7L@���@�@��@�
=@�G�@�;d@�1'@�+@�^@�V@��D@�Q�@�(�@�1'@��
@��@���@�%@�|�@ڗ�@�@�O�@���@�bN@׶F@�v�@���@�l�@�o@��@�+@�t�@��
@�V@�`B@�&�@�Ĝ@��@�@�~�@��@��/@��@�x�@̋D@�l�@��#@��/@��@�dZ@��@ư!@Ɵ�@�V@��#@Ł@���@���@�V@�&�@�G�@�`B@ũ�@ź^@��@�bN@Å@�\)@��H@�5?@��@�7L@�V@��h@��#@��#@���@�hs@���@��@�  @���@��m@��@���@��@�&�@�7L@��/@���@���@���@�@�J@�-@�J@��^@���@�7L@�%@��`@��@�  @�  @�  @��;@���@�\)@�+@��@�ff@��T@��#@��#@�`B@���@�(�@��;@�ȴ@�G�@��;@�dZ@�S�@�t�@��@�ƨ@��@�+@��y@�^5@��7@��@�I�@���@�33@�o@��@���@�~�@�~�@�~�@�^5@�{@���@���@�`B@�&�@��u@�(�@��w@�;d@��!@�5?@�@��@��#@���@���@��-@���@��@�O�@���@�A�@�  @��
@��@���@�|�@�;d@�o@���@��\@�E�@�@��@�@�x�@��`@�9X@���@��F@�S�@�+@�@��!@�^5@�5?@�$�@�{@���@��@�%@��@�I�@���@��
@�ƨ@��@�C�@��R@��@�`B@�?}@��@��`@��u@�A�@��@���@�o@�p�@��@���@���@��/@��u@��;@�\)@�33@��@���@���@��#@��h@�`B@��@���@��`@�Ĝ@��u@�Q�@��@���@��F@��@�C�@���@�=q@��-@��h@�hs@�7L@�V@��9@� �@��@���@�\)@�@�~�@��T@��^@��@�O�@�&�@���@��/@�Ĝ@��D@�I�@� �@�  @���@��@��@�l�@��@���@��@��@��#@���@�O�@���@�bN@��@�  @���@�t�@�+@��-@}��@v{@nȴ@i%@a&�@X��@O�@F5?@?\)@8�`@5O�@/�;@(�u@!��@��@%@�@�w@dZ@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�  A�  A�  A���A���A���AҸRAҕ�A�z�A�O�A��A��/AѴ9Aѕ�Aщ7AхAсA�~�A�t�A�p�A�n�A�jA�hsA�dZA�bNA�`BA�ZA�VA�Q�A�Q�A�Q�A�O�A�M�A�M�A�K�A�G�A�E�A�A�A�;dA�/A��A�A�r�A�;dA�hsA�hsA��+A�S�A�+A�XA�l�A�A�A�A�A�5?A�l�A�  A��A�jA�
=A���A�VA��A��A��+A�^5A��A�`BA�p�A���A���A��A�?}A��RA�ȴA��FA��hA�bA��TA��
A�p�A��TA�=qA���A�33A�;dA�jA��TA�`BA��RA�+A�ffA�%A�ȴA�hsA�%A��A��A��A��A�`BA�5?A�C�A���A�
=A���A�E�A�A��DA�=qA�5?A���A��^A��Az�+Au��AtVAs�Ar�Ap�AnQ�Al{Ai�hAh  Ah��Ahz�Af=qAa��A`  A^jA]
=A\ffA[&�AYdZAX�AW�AW
=AU&�AQ&�AMC�AJ��AG�#AD�/AB��A@��A>�A;;dA8�A7dZA5�TA3O�A1��A0��A/��A.��A-�A*��A*bA)�wA)�A(�`A'��A%��A$�A$(�A#�7A!�TA�TAA�RA1'A��A%AJA��AVA��AK�AjA�
A�+A+A�A$�A9XA�A;dA	C�A�-A�!A+A�A?}A�A�A��A?}A �!A ^5A 9XA @��@���@�7L@�9X@�S�@��@�M�@��@�S�@�Q�@�n�@�=q@�-@�7L@���@�@��@�
=@�G�@�;d@�1'@�+@�^@�V@��D@�Q�@�(�@�1'@��
@��@���@�%@�|�@ڗ�@�@�O�@���@�bN@׶F@�v�@���@�l�@�o@��@�+@�t�@��
@�V@�`B@�&�@�Ĝ@��@�@�~�@��@��/@��@�x�@̋D@�l�@��#@��/@��@�dZ@��@ư!@Ɵ�@�V@��#@Ł@���@���@�V@�&�@�G�@�`B@ũ�@ź^@��@�bN@Å@�\)@��H@�5?@��@�7L@�V@��h@��#@��#@���@�hs@���@��@�  @���@��m@��@���@��@�&�@�7L@��/@���@���@���@�@�J@�-@�J@��^@���@�7L@�%@��`@��@�  @�  @�  @��;@���@�\)@�+@��@�ff@��T@��#@��#@�`B@���@�(�@��;@�ȴ@�G�@��;@�dZ@�S�@�t�@��@�ƨ@��@�+@��y@�^5@��7@��@�I�@���@�33@�o@��@���@�~�@�~�@�~�@�^5@�{@���@���@�`B@�&�@��u@�(�@��w@�;d@��!@�5?@�@��@��#@���@���@��-@���@��@�O�@���@�A�@�  @��
@��@���@�|�@�;d@�o@���@��\@�E�@�@��@�@�x�@��`@�9X@���@��F@�S�@�+@�@��!@�^5@�5?@�$�@�{@���@��@�%@��@�I�@���@��
@�ƨ@��@�C�@��R@��@�`B@�?}@��@��`@��u@�A�@��@���@�o@�p�@��@���@���@��/@��u@��;@�\)@�33@��@���@���@��#@��h@�`B@��@���@��`@�Ĝ@��u@�Q�@��@���@��F@��@�C�@���@�=q@��-@��h@�hs@�7L@�V@��9@� �@��@���@�\)@�@�~�@��T@��^@��@�O�@�&�@���@��/@�Ĝ@��D@�I�@� �@�  @���@��@��@�l�@��@���@��@��@��#@���@�O�@���@�bN@��@�  @���@�t�@�+@��-@}��@v{@nȴ@i%@a&�@X��@O�@F5?@?\)@8�`@5O�@/�;@(�u@!��@��@%@�@�w@dZ@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\B\B\B\BVBJBDB
=B	7B1B%BBBBB  B  B  B  BBB  B  B  B  B  B  B  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B�B8RB>wB\)B��B�B��B��B��B��B��B�BP�B{�B�+B�VB�hB�uB��B��B��B��B�B�!B�9BBȴB�wB�B��B��B�bB�oB��B�B�?B�B��B��B��B��B�uB�BcTB?}B5?B,B�BB	7B	7BB��B��B�sB��B��B�DBm�B>wB2-B#�BhBPB
��B
�B
�qB
�JB
D�B
uB	��B	��B	�FB	�!B	�B	��B	��B	�B	o�B	_;B	[#B	y�B	�7B	� B	[#B	D�B	.B	 �B	�B	�B	uB	�B	$�B	7LB	+B	�B	JB		7B��B�B�BĜB�!B��B��B��B��B�VB�%B�B�B�+B�%B�+B�DB�oB�uB�{B��B��B��B�{B�{B��B�uB�oB�bB�\B�PB�DB�JB�DB�1B�1B�%B�1B�JB�oB�{B�JB�+B�%B�B�B~�B{�Bz�B}�B� B�B�B�B�%B�%B�1B�7B�7B�1B�DB�JB�\B�bB�\B�VB�VB�JB�bB�{B��B��B��B�B�!B�-B�-B�-B�-B�B��B��B��B��B��B��B��B��B��B�B�B�B�9B�FB�dB�qB�wB��BÖBȴB��B�B�;B�NB�ZB�sB�B��B��B��B	B	B	%B	B	B��B��B��B��B��B�B��B��B	  B	B	B	B	%B	
=B	PB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	!�B	%�B	(�B	,B	1'B	49B	2-B	49B	<jB	D�B	F�B	G�B	I�B	K�B	N�B	Q�B	W
B	XB	YB	XB	W
B	VB	W
B	W
B	XB	T�B	W
B	]/B	aHB	dZB	iyB	m�B	q�B	s�B	t�B	v�B	w�B	z�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�=B	�7B	�+B	�%B	�B	}�B	z�B	y�B	z�B	{�B	}�B	�B	�B	�B	�B	�B	�7B	�=B	�=B	�+B	�+B	�1B	�7B	�=B	�JB	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�?B	�FB	�FB	�RB	�RB	�XB	�^B	�dB	�qB	�qB	�wB	�}B	B	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�/B	�/B	�;B	�HB	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
DB
JB
VB
VB
hB
bB
uB
{B
�B
�B
!�B
&�B
.B
49B
<jB
D�B
J�B
O�B
S�B
YB
^5B
ffB
k�B
n�B
q�B
w�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B\B\B\B\B\BPBJBDBDB
=B1BBBBB  B  B  B  BBB  B  B  B  B  B  B  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BB�BK�BT�BA�BhsB�RB��B%B	7BJB+B+B�BW
B�%B�VB�uB��B��B��B��B��B�B�'B�RB��B��B�BɺB�RB�?B��B�oB��B��B�RB��B�9B�!B�!B�B��B��B��Bs�BH�BB�B?}B'�B1BPB\BDB%BBB�`B��B��B�BH�B>wB33B�B!�B\B
��B
�B
�XB
bNB
49B
�B	�B	��B	�XB	�?B	�XB	�-B	��B	~�B	gmB	[#B	�B	��B	��B	hsB	O�B	7LB	'�B	%�B	#�B	�B	�B	.B	J�B	F�B	6FB	 �B	�B	oB	B�mB�
BǮB�FB�'B�3B�B��B�\B�VB�JB�{B�uB�PB�VB��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�uB��B��B�bB�VB�PB�\B��B��B��B��B��B�uB�\B�hB�=B�B�%B�%B�%B�%B�+B�1B�=B�=B�DB�DB�JB�VB��B��B�uB�uB�hB�hB�uB��B��B��B��B��B��B�B�3B�?B�LB�^B�^B�FB�B��B��B��B��B��B��B�B�B�!B�-B�3B�LB�XB�qB�}B��BÖBȴB��B��B�B�;B�NB�TB�fB�B��B��B	B	B	+B	1B	1B		7B	B	  B��B��B��B��B��B��B	  B	B	B	B	%B	JB	PB	{B	�B	�B	�B	�B	�B	�B	 �B	%�B	!�B	&�B	(�B	.B	33B	49B	2-B	33B	<jB	D�B	G�B	H�B	I�B	M�B	P�B	Q�B	W
B	XB	]/B	XB	XB	VB	YB	[#B	XB	W
B	W
B	\)B	aHB	e`B	jB	n�B	q�B	t�B	u�B	x�B	y�B	z�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�JB	�DB	�7B	�=B	�7B	�B	z�B	y�B	z�B	{�B	}�B	�B	�B	�B	�%B	�1B	�DB	�PB	�=B	�1B	�1B	�7B	�7B	�DB	�JB	�JB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�?B	�FB	�LB	�LB	�XB	�RB	�XB	�dB	�dB	�qB	�qB	�}B	��B	ĜB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�B	�/B	�/B	�5B	�5B	�5B	�/B	�;B	�5B	�BB	�;B	�NB	�HB	�TB	�ZB	�`B	�mB	�fB	�fB	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
DB
PB
VB
\B
oB
hB
uB
{B
�B
�B
!�B
&�B
.B
5?B
=qB
D�B
J�B
O�B
S�B
YB
_;B
ffB
k�B
n�B
r�B
w�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<��=L��<�`B<#�
<D��<�9X<��
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<T��<#�
<#�
<u<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<49X<u<�9X<�o<#�
<D��<���<�t�<#�
<#�
<#�
<#�
<#�
<e`B<���<���<���<�/<��
<#�
<D��<u<T��<��
<�j<�`B=49X=49X<�h=o=C�<���<#�
<#�
<#�
<�o<���<�t�<u<#�
<#�
<#�
<�1<�`B<T��<49X<#�
<#�
<#�
<D��<#�
<#�
<#�
<���<���<���<��
<�9X<�1<�o<u<�t�<�j<�C�<#�
<e`B<�C�<49X<#�
<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<49X<#�
<D��<T��<u<T��<T��<e`B<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451552012011014515520120110145155  AO  ARGQ                                                                        20111130143537  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143537  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145155  IP                  G�O�G�O�G�O�                