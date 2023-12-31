CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:37Z AOML 3.0 creation; 2016-06-01T00:08:15Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230837  20160531170815  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               >A   AO  4055_7112_062                   2C  D   APEX                            5374                            041511                          846 @�ɨ�9?�1   @�ɩο�@:��+J�d	?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    >A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D��D�C3D�� D�ٚD��D�9�D�i�D��3D��fD�<�D��fD��fD���D�<�Dڙ�D��3D���D�@ D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt�Dy��D�(�D�O\D��)D���D��D�E�D�u�D��\D��D�H�D���D��D��D�H�Dڥ�D��\D��D�L)D�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��mA��yA��yA��yA��yA��yA��A���A���A���A���A���A���A���A���A���A�  A�A�  A��A�A�  A�A�$�A�=qA�O�A�XA�\)A�x�AÁA¼jA��PA�?}A�VA��-A��^A��HA��mA�-A��A��A�
=A��A�|�A�VA�=qA���A�%A���A�bA��jA���A��jA�$�A�M�A��A�E�A�|�A��A���A�;dA�O�A��FA���A��A�^5A�ZA�E�A�-A���A��7A��A��-A��\A�O�A�oA��^A���A�5?A���A��-A��A��^A�(�A��FA���A���A�(�A���A��A�ĜA�;dA��A��A��TA���A� �A��#A�hsA���A~��A~n�A|{AyS�AxVAw�Aw
=Avn�Au�Aup�At�RAp�HAj�\AhĜAg\)AfJA`��A_?}A]�hA\-AZ�`AX��AWdZAVZAU�FAS��AQ�TAO�mAOp�AN�AL��ALM�AJI�AIG�AGO�AF�`AE�AD��AD��AD~�AC;dAA�
A@�uA?�;A?�A>��A=�^A;�A:ffA9/A8bNA7��A6�A6��A6�A6^5A4��A4(�A4�A4A4  A3��A3�mA3�wA3�hA3hsA3?}A2��A2bNA2=qA2{A1�7A0-A/A/��A-�-A,M�A+�
A+�A+;dA*�A*��A*A�A)�A)�wA)C�A&�yA&  A%;dA$n�A#��A#XA"��A ��A
=A%A5?A�wAZAC�A��AO�AQ�A��A�/AK�AI�Av�A��AC�A
��A
-A	�-A	�7A	
=AAG�A^5A9XA�A�wA�AVA��AȴAVA �\A 1'@�ƨ@��y@��j@��
@�o@�7L@�O�@�Q�@�  @�33@��y@�@���@�1'@��#@�D@�R@�%@�n�@�7@��m@��
@�ƨ@߅@�5?@���@��/@�ȴ@��@ԣ�@��;@�33@Ѻ^@ύP@�
=@Ώ\@��@�Z@��;@�dZ@�
=@ʸR@�5?@�`B@��m@�|�@��H@���@�1@�
=@�@�@�`B@�1'@�
=@��+@��T@�&�@���@�A�@��@���@�t�@�o@��H@�M�@�?}@�bN@��m@�33@���@�`B@�A�@��w@��P@�5?@��@��/@�z�@�  @�ƨ@�l�@���@��@��7@�X@�G�@�&�@�%@��@�z�@��F@�t�@�+@��H@�~�@���@�&�@��@��@�n�@�@�%@���@���@��u@�j@�Z@�I�@�1@�ƨ@���@�t�@�"�@�
=@��y@���@��R@��!@���@�~�@�=q@�J@��#@�x�@���@�bN@�9X@� �@�1@��@��H@��+@���@���@���@�;d@���@���@�p�@���@���@�\)@�+@��!@�ff@�$�@��@�@�p�@���@�Ĝ@���@��D@�Z@�b@���@�|�@�K�@�C�@�33@���@�ȴ@�n�@�$�@�J@��T@���@�@��7@�7L@���@�  @���@�C�@�@�ȴ@�ff@�=q@��@���@�X@��@��`@��D@��m@�\)@�@�M�@�hs@�G�@�/@��@���@��u@�r�@�bN@��;@�K�@�"�@�@��@���@�n�@�M�@�5?@��@��7@�7L@�&�@�&�@��@��/@��j@���@�bN@�Z@�9X@� �@�b@�1@�  @�@��@�P@l�@\)@K�@;d@+@+@
=@~�+@|z�@{�@z�\@z-@z�@zJ@y��@y��@y�@xr�@w�;@w��@w|�@w\)@wl�@w\)@w\)@w\)@w;d@w;d@w+@w;d@w�@v�y@v�+@v$�@u/@p��@ko@c��@[dZ@P��@K��@B��@?�P@:�@1�7@-V@*^5@%�T@�+@��@;d@�F@p�@
��@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A��HA��mA��yA��yA��yA��yA��yA��A���A���A���A���A���A���A���A���A���A�  A�A�  A��A�A�  A�A�$�A�=qA�O�A�XA�\)A�x�AÁA¼jA��PA�?}A�VA��-A��^A��HA��mA�-A��A��A�
=A��A�|�A�VA�=qA���A�%A���A�bA��jA���A��jA�$�A�M�A��A�E�A�|�A��A���A�;dA�O�A��FA���A��A�^5A�ZA�E�A�-A���A��7A��A��-A��\A�O�A�oA��^A���A�5?A���A��-A��A��^A�(�A��FA���A���A�(�A���A��A�ĜA�;dA��A��A��TA���A� �A��#A�hsA���A~��A~n�A|{AyS�AxVAw�Aw
=Avn�Au�Aup�At�RAp�HAj�\AhĜAg\)AfJA`��A_?}A]�hA\-AZ�`AX��AWdZAVZAU�FAS��AQ�TAO�mAOp�AN�AL��ALM�AJI�AIG�AGO�AF�`AE�AD��AD��AD~�AC;dAA�
A@�uA?�;A?�A>��A=�^A;�A:ffA9/A8bNA7��A6�A6��A6�A6^5A4��A4(�A4�A4A4  A3��A3�mA3�wA3�hA3hsA3?}A2��A2bNA2=qA2{A1�7A0-A/A/��A-�-A,M�A+�
A+�A+;dA*�A*��A*A�A)�A)�wA)C�A&�yA&  A%;dA$n�A#��A#XA"��A ��A
=A%A5?A�wAZAC�A��AO�AQ�A��A�/AK�AI�Av�A��AC�A
��A
-A	�-A	�7A	
=AAG�A^5A9XA�A�wA�AVA��AȴAVA �\A 1'@�ƨ@��y@��j@��
@�o@�7L@�O�@�Q�@�  @�33@��y@�@���@�1'@��#@�D@�R@�%@�n�@�7@��m@��
@�ƨ@߅@�5?@���@��/@�ȴ@��@ԣ�@��;@�33@Ѻ^@ύP@�
=@Ώ\@��@�Z@��;@�dZ@�
=@ʸR@�5?@�`B@��m@�|�@��H@���@�1@�
=@�@�@�`B@�1'@�
=@��+@��T@�&�@���@�A�@��@���@�t�@�o@��H@�M�@�?}@�bN@��m@�33@���@�`B@�A�@��w@��P@�5?@��@��/@�z�@�  @�ƨ@�l�@���@��@��7@�X@�G�@�&�@�%@��@�z�@��F@�t�@�+@��H@�~�@���@�&�@��@��@�n�@�@�%@���@���@��u@�j@�Z@�I�@�1@�ƨ@���@�t�@�"�@�
=@��y@���@��R@��!@���@�~�@�=q@�J@��#@�x�@���@�bN@�9X@� �@�1@��@��H@��+@���@���@���@�;d@���@���@�p�@���@���@�\)@�+@��!@�ff@�$�@��@�@�p�@���@�Ĝ@���@��D@�Z@�b@���@�|�@�K�@�C�@�33@���@�ȴ@�n�@�$�@�J@��T@���@�@��7@�7L@���@�  @���@�C�@�@�ȴ@�ff@�=q@��@���@�X@��@��`@��D@��m@�\)@�@�M�@�hs@�G�@�/@��@���@��u@�r�@�bN@��;@�K�@�"�@�@��@���@�n�@�M�@�5?@��@��7@�7L@�&�@�&�@��@��/@��j@���@�bN@�Z@�9X@� �@�b@�1@�  @�@��@�P@l�@\)@K�@;d@+@+@
=@~�+@|z�@{�@z�\@z-@z�@zJ@y��@y��@y�@xr�@w�;@w��@w|�@w\)@wl�@w\)@w\)@w\)@w;d@w;d@w+@w;d@w�@v�y@v�+@v$�G�O�@p��@ko@c��@[dZ@P��@K��@B��@?�P@:�@1�7@-V@*^5@%�T@�+@��@;d@�F@p�@
��@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhBhBhBbBhBhBhBbBbBhBhBhBoBoBoBoBoBuBuBuB�BuBoBuB�B!�B'�B)�B,B5?B7LB&�B  B�B�/B��B��B�-B�{B{�Bl�B]/B8RB�B�BuBJBB��B�B�mB�`B�;B��BǮB�}B�^B�FB��B��B�uB�=B}�Bs�B_;B@�B5?B49B1'B.B&�BVB��B�B��B�dB��B��B��B�hBt�BdZBYBP�BE�B-B�BJBB
�yB
�B
��B
ÖB
�qB
�B
��B
�7B
q�B
_;B
W
B
I�B
9XB
33B
!�B
\B
1B
B	��B	��B	��B	�B	�yB	��B	��B	�uB	�1B	x�B	ZB	N�B	D�B	<jB	49B	)�B	"�B	�B	�B	\B	B��B��B��B�B�B�NB�5B�B�B��B��B��B��B��BȴBƨBĜBB��B�qB�XB�FB�9B�3B�'B�!B�!B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�bB�\B�JB�%B�B� B~�B}�B|�Bx�Bu�Bp�Bk�BhsBe`BaHB]/B[#BW
BQ�BO�BJ�BF�BA�B:^B33B1'B0!B/B.B-B+B)�B(�B'�B'�B&�B%�B$�B"�B!�B�B�B�B�B�B�B�B�B�BuBuBoBhBhBbB\BVBJBJBDB	7B1B%B+B1B1B+B%BBBB%B+B+B+B+B+B
=B
=B
=B
=BJBJBJBJBJBDBJBVBVBPBVBbBhBhBoBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B#�B%�B#�B1'B5?B9XB:^B<jB?}B@�BA�BE�BH�BJ�BK�BK�BK�BL�BL�BN�BR�BS�BT�BW
BYB^5B`BBe`BjBn�Bp�Bv�By�By�By�Bz�B{�B{�B|�B~�B� B� B�B�B�B�B�B�B�B�%B�+B�1B�7B�=B�VB�bB�hB�oB�oB�{B��B��B��B��B��B�B�B�B�LB��BĜBŢBǮB��B��B��B��B��B�B�B�)B�/B�5B�;B�HB�NB�ZB�fB�fB�fB�sB�B�B�B�B�B�B��B��B��B��B	B	%B		7B	DB	JB	\B	bB	oB	{B	�B	�B	�B	�B	"�B	&�B	)�B	/B	7LB	9XB	9XB	<jB	>wB	@�B	B�B	D�B	I�B	M�B	N�B	N�B	P�B	R�B	T�B	VB	W
B	YB	^5B	aHB	bNB	bNB	bNB	e`B	ffB	hsB	jB	k�B	l�B	m�B	n�B	n�B	o�B	p�B	q�B	q�B	r�B	r�B	r�B	r�B	s�B	s�B	s�B	u�B	� B	�B	�=B	�JB	�JB	�JB	�PB	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ƨB	�/B	�B

=B
{B
"�B
&�B
.B
;dB
B�B
F�B
I�B
R�B
VB
\)B
aHB
iyB
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   BQBUBOBKBQBQBOBKBKBVBRBRBYBYBYBWBYB_B_B\B|B\BYB_B�B!�B'�B)�B+�B5)B73B&�B��B�kB�B��B�nB�B�_B{�BlpB]B83B�BwBWB)B�B��B�~B�LB�@B�BͰBǍB�\B�=B�&B��B��B�RB�B}�Bs�B_B@[B5B4B1B-�B&�B.B��B�dB��B�CB��B��B��B�BBt�Bd5BX�BP�BE|B,�B�B)B�B
�TB
��B
˥B
�rB
�OB
��B
�B
�B
q�B
_B
V�B
I�B
99B
3B
!�B
<B
B
�B	��B	��B	��B	�B	�\B	ʤB	��B	�XB	�B	x�B	ZB	N�B	D�B	<QB	4 B	)�B	"�B	�B	�B	AB	B��B��B��B�B�hB�8B� B��B��B��B��B��B;B˯BȟBƎBćB�yB�mB�YB�@B�1B�#B�B�B�
B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�wB�oB�hB�eB�YB�SB�KB�NB�EB�4B�B��B�B~�B}�B|�Bx�Bu�Bp�BkqBh\BeKBa4B]B[BV�BQ�BO�BJ�BF�BAuB:KB3 B1B/�B/B-�B,�B*�B)�B(�B'�B'�B&�B%�B$�B"�B!�B�B�B�B|BuB�B}B^BPBaB`BAB:B9B4BFB(B5BB1B		BBB�BBB�BB�B�B�BBB�BB�BB
B
B
B
BBBB4B5BB5B%B%BB@BLB7BPB[B<BeBhBVBpB{B�B�BiB�B�BtB�B�B�B�B �B!�B"�B#�B%�B#�B1B5&B9@B:EB<PB?dB@iBAoBE�BH�BJ�BK�BK�BK�BL�BL�BN�BR�BS�BT�BV�BX�B^B`'BeDBjcBn{Bp�Bv�By�By�By�Bz�B{�B{�B|�B~�B�B�B��B��B��B��B��B�B�B�B�B�B�B�B�8B�EB�KB�PB�SB�]B�vB�|B��B��B��B��B��B��B�*B�hB�yBŀBǊB˨BͱBϾB��B��B��B��B�B�B�B�B�&B�.B�9B�CB�CB�BB�QB�^B�pB��B�B�B�B��B��B��B��B	�B	B		B	B	'B	:B	=B	KB	WB	fB	tB	zB	�B	"�B	&�B	)�B	.�B	7&B	92B	91B	<DB	>QB	@]B	BiB	DuB	I�B	M�B	N�B	N�B	P�B	R�B	T�B	U�B	V�B	X�B	^B	aB	b%B	b%B	b&B	e7B	f>B	hMB	jVB	k^B	ldB	mjB	nqB	nqB	ouB	pzB	q�B	q�B	r�B	r�B	r�B	r�B	s�B	s�B	s�B	u�B	�B	��B	�B	� B	�#B	� B	�*B	�(B	�:B	�RB	�\B	�cB	�jB	�qB	�kB	�qB	�pB	�rB	�vB	�xB	�xB	�xB	�xB	�yB	��B	��G�O�B	��B	�}B	�B	�}B

B
MB
"�B
&�B
-�B
;7B
BcB
F|B
I�B
R�B
U�B
[�B
aB
iKB
l]B
pu11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708152016053117081520160531170815  AO  ARCAADJP                                                                    20140721230837    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230837  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230837  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170815  IP                  G�O�G�O�G�O�                