CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:25Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170925  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�]��h�1   @�^[�p@7dZ��b�~��"�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�
D�${D�W
D���D�ؤD��D�W�D��=D��RD��D�P D���D���D�\D�Y�Dڙ�D��fD�RD�UD�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�(�@�(�A{A:{AZ{Az{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7��C9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs��Cu�HCw�HCy�HC{�HC}�HC�HC�ФC�ФC��qC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC���C���C�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC���C�ФC�ФC�ФC�ФC�ФC�ФC�ФC��qC�ФC�ФC�ФC���C�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФC�ФD hRD �RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RD	hRD	�RD
hRD
�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDa�D�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RDhRD�RD hRD �RD!hRD!�RD"hRD"�RD#hRD#�RD$hRD$�RD%hRD%�RD&hRD&�RD'hRD'�RD(hRD(�RD)hRD)�RD*hRD*�RD+hRD+�RD,hRD,�RD-hRD-��D.hRD.�RD/hRD/�RD0hRD0�RD1hRD1�RD2hRD2�RD3hRD3�RD4hRD4�RD5hRD5�D6hRD6�RD7hRD7�RD8hRD8�RD9hRD9�RD:hRD:�RD;hRD;�RD<hRD<�RD=hRD=�RD>hRD>�RD?hRD?�RD@hRD@�RDAhRDA�RDBhRDB�RDChRDC�RDDhRDD�RDEhRDE�RDFhRDF�RDGhRDG�RDHhRDH�RDIhRDI�RDJhRDJ�RDKhRDK�RDLhRDL�RDMhRDM�RDNhRDN�RDOhRDO�RDPhRDP�RDQhRDQ�RDRhRDR�RDShRDS�RDThRDT�RDUhRDU�RDVhRDV�RDWhRDW�RDXhRDX�RDYhRDY�RDZhRDZ�RD[hRD[�RD\hRD\�RD]hRD]�RD^hRD^�RD_hRD_�RD`hRD`�RDahRDa�RDbhRDb�RDchRDc�RDdhRDd�RDehRDe�RDfhRDf�RDghRDg�RDha�Dh�RDihRDi�RDjhRDj�RDkhRDk�RDlhRDl�RDmhRDm�RDnhRDn�RDohRDo�RDphRDp��DqhRDq�RDrhRDr�RDshRDs�RDthRDt��Dy\D��D�K3D�|�D���D��D�K�D��fD��{D��D�D)D���D�� D��D�NDڎD�ڏD�{D�IHD�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�t�A�v�A�v�A�v�A�x�A�x�A�z�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�z�A��A�~�A�|�A�z�A�hsA�"�A��#A�\)A�XA�&�A���A��A��jA��uA���A�
=A���A�A�A��;A��9A�r�A�K�A�n�A�z�A�7LA��A���A���A��\A�n�A�(�A��A�9XA�{A�
=A��A�
=A�1A� �A�$�A�r�A��jA�z�A��-A�A��A���A��A�\)A�jA���A��A�oA��RA�~�A���A��A���A���A��-A���A� �A���A��FA���A��uA�(�A�$�A�A�A� �A���A��A�\)A���A��^A�z�A��A���A��A�I�A��PA�(�A���A���A��TA�A��A���A� �A��A��7A���A��A��A�S�A`BA}��AzQ�Av~�Au��As��An-Ak��AkC�AjM�Ai33Ae&�Ac;dAb1'Aa��A_��A\�`AZ~�AY��AYVAX��AV�DAT �ASXAR�AQ�ANĜAL��AL5?AK�AJ^5AI��AG�AF�`AFr�AD�AC��A@��A=�A<bA;"�A:n�A8��A7A5�A4Q�A2��A2ffA1�#A0ZA/ƨA/;dA.ZA-`BA,�A+t�A*�A)x�A'�#A&��A%?}A#�#A#��A#XA"��A"1'A!VA �AAȴA1A�!AA�hA&�A��A�A��A�^AjA��AA\)A1AA�AA�A�-A
��A	C�A��AI�A��A��A��A�yA��A�DAt�A�A�/A1AA b@���@��@�Ĝ@�n�@�X@��@�|�@��@�t�@�9X@@�9@�F@�-@�j@�|�@���@�\@��@䛦@�Q�@�1'@���@�\)@��#@��u@�
=@�p�@�b@�@�Ĝ@�Z@��m@�\)@��@��/@�1'@�S�@��@�p�@Л�@�b@ϥ�@�^5@��@˶F@ɉ7@ȓu@��;@Ǯ@�v�@��T@ŉ7@�?}@���@ļj@��;@���@���@���@��;@���@�@�p�@�%@���@�z�@���@��F@�@�J@���@���@��@�Ĝ@��9@�&�@��!@�1@�33@�ff@�/@�z�@�b@�l�@��y@��\@�$�@���@��@�A�@�"�@���@�{@��@���@�t�@�|�@�S�@��y@�=q@��@��#@�%@���@��@�;d@�@��
@�  @�A�@�r�@�b@���@���@�hs@�/@�7L@�&�@�V@�Ĝ@��@�+@�@��-@��/@�r�@��w@�b@��@��j@��;@��@���@��!@���@��/@�j@��@�^5@��#@�hs@���@��h@��u@�Z@�(�@��;@��F@��@�M�@���@��7@��#@��^@�@��-@��7@�&�@�Z@�1@�|�@��@�ȴ@��@�
=@�;d@�;d@�ȴ@��@�@�@�;d@�+@�o@��@���@�V@��\@�hs@��@���@�Q�@�ƨ@�t�@���@��m@��;@���@�|�@��@���@�K�@�K�@�K�@�K�@�|�@��@�l�@��@�n�@�-@��#@���@�`B@���@��@�z�@�  @��@���@�C�@��@���@���@���@�v�@�^5@�V@�5?@�$�@�{@�{@��@�@��h@��7@�/@��@�l�@�@��y@��H@�@��y@�ff@��#@���@��`@���@�x�@�&�@��9@�z�@�ƨ@�1'@�r�@��u@���@��@���@��T@��@��#@��^@�x�@�?}@�7L@�%@��`@�r�@� �@��;@�dZ@��R@�M�@��@�n�@�$�@��^@�J@{C�@s\)@l�4@e��@a�@Y@P`�@I��@C�@;@O@4�4@/j�@*�6@$�@$�@�:@�@y�@C-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�t�A�t�A�v�A�v�A�v�A�x�A�x�A�z�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�z�A��A�~�A�|�A�z�A�hsA�"�A��#A�\)A�XA�&�A���A��A��jA��uA���A�
=A���A�A�A��;A��9A�r�A�K�A�n�A�z�A�7LA��A���A���A��\A�n�A�(�A��A�9XA�{A�
=A��A�
=A�1A� �A�$�A�r�A��jA�z�A��-A�A��A���A��A�\)A�jA���A��A�oA��RA�~�A���A��A���A���A��-A���A� �A���A��FA���A��uA�(�A�$�A�A�A� �A���A��A�\)A���A��^A�z�A��A���A��A�I�A��PA�(�A���A���A��TA�A��A���A� �A��A��7A���A��A��A�S�A`BA}��AzQ�Av~�Au��As��An-Ak��AkC�AjM�Ai33Ae&�Ac;dAb1'Aa��A_��A\�`AZ~�AY��AYVAX��AV�DAT �ASXAR�AQ�ANĜAL��AL5?AK�AJ^5AI��AG�AF�`AFr�AD�AC��A@��A=�A<bA;"�A:n�A8��A7A5�A4Q�A2��A2ffA1�#A0ZA/ƨA/;dA.ZA-`BA,�A+t�A*�A)x�A'�#A&��A%?}A#�#A#��A#XA"��A"1'A!VA �AAȴA1A�!AA�hA&�A��A�A��A�^AjA��AA\)A1AA�AA�A�-A
��A	C�A��AI�A��A��A��A�yA��A�DAt�A�A�/A1AA b@���@��@�Ĝ@�n�@�X@��@�|�@��@�t�@�9X@@�9@�F@�-@�j@�|�@���@�\@��@䛦@�Q�@�1'@���@�\)@��#@��u@�
=@�p�@�b@�@�Ĝ@�Z@��m@�\)@��@��/@�1'@�S�@��@�p�@Л�@�b@ϥ�@�^5@��@˶F@ɉ7@ȓu@��;@Ǯ@�v�@��T@ŉ7@�?}@���@ļj@��;@���@���@���@��;@���@�@�p�@�%@���@�z�@���@��F@�@�J@���@���@��@�Ĝ@��9@�&�@��!@�1@�33@�ff@�/@�z�@�b@�l�@��y@��\@�$�@���@��@�A�@�"�@���@�{@��@���@�t�@�|�@�S�@��y@�=q@��@��#@�%@���@��@�;d@�@��
@�  @�A�@�r�@�b@���@���@�hs@�/@�7L@�&�@�V@�Ĝ@��@�+@�@��-@��/@�r�@��w@�b@��@��j@��;@��@���@��!@���@��/@�j@��@�^5@��#@�hs@���@��h@��u@�Z@�(�@��;@��F@��@�M�@���@��7@��#@��^@�@��-@��7@�&�@�Z@�1@�|�@��@�ȴ@��@�
=@�;d@�;d@�ȴ@��@�@�@�;d@�+@�o@��@���@�V@��\@�hs@��@���@�Q�@�ƨ@�t�@���@��m@��;@���@�|�@��@���@�K�@�K�@�K�@�K�@�|�@��@�l�@��@�n�@�-@��#@���@�`B@���@��@�z�@�  @��@���@�C�@��@���@���@���@�v�@�^5@�V@�5?@�$�@�{@�{@��@�@��h@��7@�/@��@�l�@�@��y@��H@�@��y@�ff@��#@���@��`@���@�x�@�&�@��9@�z�@�ƨ@�1'@�r�@��u@���@��@���@��T@��@��#@��^@�x�@�?}@�7L@�%@��`@�r�@� �@��;@�dZ@��R@�M�@��@�n�@�$�G�O�@�J@{C�@s\)@l�4@e��@a�@Y@P`�@I��@C�@;@O@4�4@/j�@*�6@$�@$�@�:@�@y�@C-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB49B49B33B33B49B33B33B33B33B33B33B33B49B5?B6FB6FB6FB6FB7LB5?B6FB7LB7LB9XB?}BE�BQ�BffB~�B�B�B�+B�JB��B�^BȴB�#B��B��BPBuB)�B8RB>wB?}B9XB2-B33B0!B'�B"�B\B	7BDBVB\BVB�B�B2-BH�BXB[#BXBR�BJ�BA�B@�BE�BE�BN�BQ�BN�BL�BN�BO�BR�BL�BG�BB�B?}B7LB+B'�B#�B�BDBB��B�B�fB�BɺB�3B��B��B�hB�+Bu�Bk�BcTB\)BL�B6FB"�B1B
�sB
�FB
�VB
�B
R�B
L�B
>wB
6FB
$�B
�B
+B	�TB	�#B	��B	��B	�\B	�JB	�1B	�B	{�B	ffB	cTB	_;B	VB	I�B	5?B	33B	2-B	5?B	1'B	$�B	�B	�B	PB	B�B�B�B�B�mB�HB�B��B��BƨBÖB�XB�FB�3B�B�B��B��B��B�uB�hB�bB�\B�JB�JB�+B�+B�B� B~�B{�Bw�Br�Bm�BjBjBjBhsBgmBdZBcTB_;B_;B^5B\)B[#BZBZBXBW
BT�BP�BN�BI�BH�BB�B@�B>wB<jB>wB;dB8RB:^B6FB5?B49B49B49B5?B33B2-B2-B1'B/B/B-B-B,B,B,B-B)�B)�B&�B'�B)�B(�B+B)�B'�B(�B+B+B,B,B+B,B-B-B-B-B-B.B/B2-B49B5?B6FB6FB6FB6FB6FB7LB8RB9XB:^B=qBA�BG�BG�BH�BH�BI�BJ�BK�BL�BM�BQ�BP�BO�BN�BO�BO�BN�BQ�BS�BW
BZB[#B_;BaHBaHBcTBffBhsBhsBhsBk�Bm�Bp�Br�Bs�Bs�Bx�B�B�\B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�XB�wB��B��BĜBƨBȴB��B��B��B��B�
B�B�/B�HB�HB�NB�B�B�B��B��B��B��B��B��B��B	B	B	+B	
=B	
=B	
=B	
=B	1B		7B	JB	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	"�B	%�B	%�B	'�B	(�B	,B	33B	33B	7LB	8RB	8RB	<jB	D�B	B�B	C�B	D�B	J�B	K�B	N�B	Q�B	T�B	VB	W
B	YB	ZB	[#B	_;B	bNB	e`B	hsB	o�B	o�B	r�B	t�B	w�B	z�B	{�B	{�B	|�B	|�B	}�B	�B	� B	� B	�B	�B	�B	�B	�7B	�DB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�?B	�RB	�^B	�^B	�qB	�}B	�}B	��B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ǮB	ŢB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�NB	�TB	�ZB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�mB	�fB	�sB	�mB	�mB	�mB	�B	�B	�B	�B	�B
'B
�B
�B
)�B
5?B
<B
B�B
IB
J�B
QNB
T�B
[�B
abB
ezB
l=B
pB
r�B
wL11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B'B'B&B&B'B&B&B&B&B&B&B&B'B(B)B)B)B)B*B(B)B*B*B,*B2NB8sBD�BY4Bq�Bt�Bu�By�BB�{B�%B�zB��B�B�B B3B�B+B11B27B,B$�B%�B"�B�B�BB��B�BBBBRBpB$�B;nBJ�BM�BJ�BE�B={B4EB3?B8^B8^BA�BD�BA�B?�BA�BB�BE�B?�B:mB5NB2=B*B�B�B�BXB�B��B�B�B�2B��B��B�B��B�cB�?BzBh�B^`BV0BOB?�B)'B�B
�B
�]B
�6B
�JB
vB
E�B
?�B
1uB
)EB
�B
�B	�0B	�^B	�.B	��B	�B	�pB	^B	{FB	tB	n�B	Y�B	VnB	RVB	I B	<�B	(_B	&TB	%NB	(`B	$IB	 B	�B		�B	 vB�4B��B��B�BްBژB�tB�DB�,B�B��B��B��B�yB�gB�OB�=B�B��B��B��B��B��B��B�B�BzfBzfBvNBs<Br6Bo$BkBe�B`�B]�B]�B]�B[�BZ�BW�BV�BR}BR}BQwBOlBNfBM`BM`BKTBJNBHBBD*BBB= B;�B5�B3�B1�B/�B1�B.�B+�B-�B)�B(�B'�B'�B'�B(�B&B%zB%zB$tB"hB"hB \B \BVBVBWB ]BKBKB9B@BLBFBRBMBABGBSBSBYBYBSBYB `B `B `B `B `B!fB"mB%B'�B(�B)�B)�B)�B)�B)�B*�B+�B,�B-�B0�B4�B:�B:�B<B<B=B>B?B@BA%BE=BD7BC1BB+BC1BC1BB+BE>BGJBJ\BMoBNuBR�BT�BT�BV�BY�B[�B[�B[�B^�B`�Bc�Bf BgBgBl%BwgB��B��B��B�	B�B�B�"B�;B�AB�GB�SB�_B�wB��B��B��B��B��B��B��B��B�B�!B�:B�FB�RB�_B�vBԏBԏBՕB��B��B��B�B�B�B�B�&B�,B�8B�JB�cB�oB��B��B��B��B�uB�{B��B		�B	�B	�B	�B	�B	�B	B	B	B	B	%B	%B	2B	8B	IB	&tB	&tB	*�B	+�B	+�B	/�B	7�B	5�B	6�B	7�B	> B	?B	BB	E*B	H<B	IBB	JHB	LUB	M[B	NaB	RxB	U�B	X�B	[�B	b�B	b�B	e�B	g�B	k
B	nB	o!B	o!B	p(B	p(B	q.B	t@B	s:B	s:B	t@B	wSB	wSB	xYB	|pB	~}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�,B	�2B	�8B	�>B	�JB	�WB	�oB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�1B	�=B	�CB	�JB	ՀB	ֆB	׋B	ٗB	ڞB	ڞB	ڞB	ڞB	ۤB	ۤB	ۤB	ۤB	ڞB	ٗB	ۤB	ڟB	ڟB	ڟB	޶B	߼G�O�B	�PB	�3B	�WB
 B
#B
)B
(lB
/.B
6
B
<IB
=�B
DzB
G�B
N�B
T�B
X�B
_gB
c1B
f)B
jv11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.37 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170925    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170925  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170925  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                