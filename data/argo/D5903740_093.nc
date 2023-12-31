CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-10-04T12:01:23Z AOML 3.0 creation; 2016-06-01T00:08:21Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20141004120123  20160531170821  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ]A   AO  4055_7112_093                   2C  D   APEX                            5374                            041511                          846 @�
B.@1   @�
Ϥ��@9wKƧ��d�z�H1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ]A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BXffB_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� DfD� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�fD�C3D��3D���D�3D�<�D��fD���D�3D�I�D�i�D�VfD��D�@ D�y�D���D�fD�0 D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�(�A{A&{AF{Af{A�
=A�
=A��
A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCz�CaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RD�D�RD�D�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3�D3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt��Dy��D�"�D�O\D��\D���D�\D�H�D���D���D�\D�U�D�u�D�b�D��D�L)Dڅ�D���D��D�<)D�u�D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aߕ�A�K�A�"�A�
=A��yA���AޮA�hsA�(�A�A���Aݺ^A�v�A�{A��A��A���A�XA�l�A���A���AѼjA�^5A�JA�\)A�-A�A��jA�A��A���A�bNA���A�{A��-A��wA�/A�7LA��A���A���A���A��+A��HA��A�K�A���A��!A���A�XA�ƨA�oA�-A�r�A�A�VA�VA��A��A��uA�p�A��A�A�A��TA�7LA��A�9XA�hsA��+A� �A�n�A�(�A���A�ƨA�A�ZA���A�v�A�(�A��hA���A���A���A�9XA��9A�z�A�oA�(�A�$�A��7A�
=A��^A�$�A���A�PA}�TAz��Aw�Au��AuK�As�As/As
=Ar��Aq��Ap5?An�`Al�AiƨAioAhȴAh�Ag
=Ad��Ac\)Ac7LAbI�Aa��A`1'A_"�A_oA_A^��A^�uA^M�A^  A]�hA\�+A[�7AZQ�AXI�AVVAU�
AU�AU�AT��AR(�AP{AO;dANQ�ALE�AJ�+AI��AG�FAFȴAFA�AD�+AC"�AB�!AA�hA?�A?�A?/A>A�A=�-A=�hA=�A<�!A<bNA;�wA:��A:$�A9�;A9�A8ȴA89XA6�jA5�A4�uA4JA3�A3XA2(�A0��A/�#A/�A.��A.bNA-��A-;dA,A�A+��A+�A*I�A)�FA)�A(�jA(1'A'A%��A$E�A#;dA"��A �A�^A33A(�A;dA�A��A�`A�!AffA(�AC�A�+A1'A�-AAv�A��AVA��A�\AA�A��A�AffAO�A
bA�\AQ�A�AO�A/AA��AbNA�A�uAJA�#A�A�A �!A n�@�|�@��\@���@��/@�ƨ@�o@���@�@�t�@���@�Ĝ@�o@�M�@��@���@�r�@�bN@� �@���@�{@��@�j@�1@�|�@�
=@ޗ�@�E�@ݩ�@���@�Z@�\)@���@�1'@��@��y@�M�@�hs@�I�@җ�@�O�@·+@�hs@�Ĝ@��
@���@Ɂ@�j@��@�K�@��@�&�@�(�@î@�;d@���@�?}@�Z@�ƨ@�@���@��+@�@��`@�  @�;d@�@�V@�(�@�dZ@�+@���@��^@��`@���@�A�@��@�o@��+@��-@�p�@���@�I�@� �@��@��R@��^@�hs@�?}@��@�%@��@�r�@�1'@�l�@���@���@�b@��@��7@��@��;@�C�@���@�^5@�p�@�9X@�C�@�M�@��@��-@�?}@�%@��@��j@���@�Q�@� �@�1@��m@�ƨ@��@�|�@�\)@�"�@�o@���@�M�@��@��@���@�/@�z�@�1'@��@�ƨ@�C�@�~�@�n�@�ff@�-@�x�@���@��/@���@��9@��@�(�@���@�+@��H@��+@�J@�O�@�j@���@�dZ@��y@���@�@��h@�p�@�G�@�?}@��@��/@�Ĝ@��@���@��@��@�r�@�I�@�1'@�(�@��@�  @���@�K�@��@��\@�$�@�&�@���@�33@��y@���@��+@�ff@�^5@�V@�E�@�=q@�{@�@��-@���@��h@�O�@�V@���@��/@���@��j@��@�j@�9X@��@�1@��
@�l�@�;d@�"�@��@�
=@��!@�E�@���@�X@��@���@���@�Ĝ@��9@��D@�bN@�I�@�1'@� �@��@�1@�  @��@�@~�R@~{@}/@|9X@{S�@z~�@z�@y��@y��@y�7@yx�@yX@x�`@x  @w+@v�+@v@t��@t9X@n�R@fV@^�+@X�9@O�;@K33@EV@?�P@:M�@2�@.{@)%@#��@`B@x�@�@�^@V@ƨ@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aߕ�A�K�A�"�A�
=A��yA���AޮA�hsA�(�A�A���Aݺ^A�v�A�{A��A��A���A�XA�l�A���A���AѼjA�^5A�JA�\)A�-A�A��jA�A��A���A�bNA���A�{A��-A��wA�/A�7LA��A���A���A���A��+A��HA��A�K�A���A��!A���A�XA�ƨA�oA�-A�r�A�A�VA�VA��A��A��uA�p�A��A�A�A��TA�7LA��A�9XA�hsA��+A� �A�n�A�(�A���A�ƨA�A�ZA���A�v�A�(�A��hA���A���A���A�9XA��9A�z�A�oA�(�A�$�A��7A�
=A��^A�$�A���A�PA}�TAz��Aw�Au��AuK�As�As/As
=Ar��Aq��Ap5?An�`Al�AiƨAioAhȴAh�Ag
=Ad��Ac\)Ac7LAbI�Aa��A`1'A_"�A_oA_A^��A^�uA^M�A^  A]�hA\�+A[�7AZQ�AXI�AVVAU�
AU�AU�AT��AR(�AP{AO;dANQ�ALE�AJ�+AI��AG�FAFȴAFA�AD�+AC"�AB�!AA�hA?�A?�A?/A>A�A=�-A=�hA=�A<�!A<bNA;�wA:��A:$�A9�;A9�A8ȴA89XA6�jA5�A4�uA4JA3�A3XA2(�A0��A/�#A/�A.��A.bNA-��A-;dA,A�A+��A+�A*I�A)�FA)�A(�jA(1'A'A%��A$E�A#;dA"��A �A�^A33A(�A;dA�A��A�`A�!AffA(�AC�A�+A1'A�-AAv�A��AVA��A�\AA�A��A�AffAO�A
bA�\AQ�A�AO�A/AA��AbNA�A�uAJA�#A�A�A �!A n�@�|�@��\@���@��/@�ƨ@�o@���@�@�t�@���@�Ĝ@�o@�M�@��@���@�r�@�bN@� �@���@�{@��@�j@�1@�|�@�
=@ޗ�@�E�@ݩ�@���@�Z@�\)@���@�1'@��@��y@�M�@�hs@�I�@җ�@�O�@·+@�hs@�Ĝ@��
@���@Ɂ@�j@��@�K�@��@�&�@�(�@î@�;d@���@�?}@�Z@�ƨ@�@���@��+@�@��`@�  @�;d@�@�V@�(�@�dZ@�+@���@��^@��`@���@�A�@��@�o@��+@��-@�p�@���@�I�@� �@��@��R@��^@�hs@�?}@��@�%@��@�r�@�1'@�l�@���@���@�b@��@��7@��@��;@�C�@���@�^5@�p�@�9X@�C�@�M�@��@��-@�?}@�%@��@��j@���@�Q�@� �@�1@��m@�ƨ@��@�|�@�\)@�"�@�o@���@�M�@��@��@���@�/@�z�@�1'@��@�ƨ@�C�@�~�@�n�@�ff@�-@�x�@���@��/@���@��9@��@�(�@���@�+@��H@��+@�J@�O�@�j@���@�dZ@��y@���@�@��h@�p�@�G�@�?}@��@��/@�Ĝ@��@���@��@��@�r�@�I�@�1'@�(�@��@�  @���@�K�@��@��\@�$�@�&�@���@�33@��y@���@��+@�ff@�^5@�V@�E�@�=q@�{@�@��-@���@��h@�O�@�V@���@��/@���@��j@��@�j@�9X@��@�1@��
@�l�@�;d@�"�@��@�
=@��!@�E�@���@�X@��@���@���@�Ĝ@��9@��D@�bN@�I�@�1'@� �@��@�1@�  @��@�@~�R@~{@}/@|9X@{S�@z~�@z�@y��@y��@y�7@yx�@yX@x�`@x  @w+@v�+@v@t��@t9X@n�R@fV@^�+@X�9@O�;@K33@EV@?�P@:M�@2�@.{@)%@#��@`B@x�@�@�^@V@ƨ@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B�uB�uB�\B�VB�=B�Bs�B]/BJ�B>wB;dB7LB,B�B�B1B�B��B�FB��B��B��B�VB~�Bo�BcTB_;BXBQ�BB�B1'B$�B�BuB\B1B��B�fB�NB�BB�;B�#B��B��B�}B�FB�B��B��B��B��B��B��B�hB�Bm�BT�BC�B5?B$�B�BVBB�yB��BĜB�LB�B��B��B�bB�Bo�B\)B:^B �B�BoB1B
��B
�B
B
�XB
�-B
��B
�VB
~�B
n�B
S�B
<jB
0!B
)�B
�B
�B
�B
{B
DB
  B	�B	�5B	��B	ȴB	ĜB	��B	�RB	��B	��B	��B	��B	��B	�JB	�+B	�%B	�%B	�B	�B	�B	~�B	z�B	t�B	n�B	ffB	]/B	T�B	S�B	Q�B	O�B	J�B	A�B	8RB	33B	0!B	'�B	 �B	�B	uB	VB	PB	1B	B	B��B�B�B�B�B�sB�mB�`B�TB�HB�5B�#B�B�
B�B��B��B��B��BɺBȴBǮBŢB��B�qB�dB�^B�RB�FB�9B�-B�B�B�B��B��B��B��B��B��B��B�bB�JB�1B�B{�Bt�Bq�Bl�BgmBdZBcTBbNBaHB_;B\)BZBXBT�BP�BM�BK�BJ�BI�BI�BH�BF�BD�BB�B?}B=qB=qB<jB<jB;dB:^B8RB5?B49B33B2-B2-B2-B1'B1'B0!B/B/B/B.B-B-B,B+B)�B'�B$�B!�B$�B$�B$�B%�B%�B$�B#�B$�B"�B&�B'�B'�B&�B&�B%�B$�B$�B$�B#�B!�B"�B"�B$�B%�B$�B#�B#�B$�B"�B#�B"�B �B�B�B�B�B�B�B�B�B�B �B �B �B"�B$�B%�B&�B'�B&�B&�B)�B+B,B/B1'B33B5?B5?B5?B7LB9XB:^B:^B<jB>wB@�BE�BF�BH�BL�BL�BM�BR�BW
BXBYBYBYBYB[#B\)B^5BcTBdZBe`BffBk�Bl�Bq�Bs�Bu�Bv�By�B� B�B�=B�DB�PB�bB�hB�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�3B�RB�RB�RB�^B�}BÖBĜBŢBƨBǮB��B��B��B��B�B�#B�HB�B�B�B��B��B��B	B	B	B	B	B	+B	1B		7B	
=B	DB	DB	DB	PB	VB	VB	\B	\B	oB	uB	�B	�B	�B	�B	+B	-B	/B	1'B	1'B	33B	33B	33B	49B	49B	6FB	:^B	:^B	;dB	=qB	E�B	J�B	L�B	L�B	M�B	M�B	M�B	O�B	Q�B	R�B	T�B	XB	[#B	\)B	\)B	\)B	\)B	_;B	bNB	gmB	jB	l�B	n�B	o�B	p�B	p�B	r�B	s�B	t�B	t�B	u�B	u�B	v�B	v�B	w�B	y�B	z�B	|�B	� B	�B	�%B	�1B	�DB	�JB	�JB	�PB	�PB	�PB	�VB	�hB	�oB	��B	��B	��B	��B	��B	ȴB	�`B	�B
+B
bB
�B
$�B
-B
8RB
>wB
E�B
L�B
T�B
ZB
`BB
dZB
hsB
l�B
o�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B�tB�gB�bB�LB�IB�,B��Bs�B]BJ�B>bB;SB78B+�B�BpBB�nB��B�+B��B��B�kB�>B~�Bo�Bc:B_BW�BQ�BBnB1B$�B`BSB;BB��B�FB�.B�"B�B�B��BʝB�ZB�$B��B��B��B��B��B�|B�iB�CB��BmoBT�BCuB5B$�B`B1B�B�UBϸB�wB�(B��B��B�nB�>B��Bo{B\B:;B �BmBGBB
��B
��B
�kB
�4B
�
B
��B
�4B
~�B
nwB
S�B
<JB
/�B
)�B
�B
B
rB
\B
#B	��B	�B	�B	̭B	ȘB	ĀB	�dB	�4B	��B	��B	��B	��B	�bB	�-B	�B	�B	�B	��B	��B	��B	~�B	z�B	t�B	n}B	fJB	]B	T�B	S�B	Q�B	O�B	J�B	AnB	86B	3B	0B	'�B	 �B	�B	\B	=B	6B	B	�B	 �B��B�B�B�B�mB�YB�QB�HB�=B�1B� B�
B��B��B��B��B��B��B˯BɡBȝBǖBŋB�qB�XB�JB�GB�:B�.B�!B�B�B��B��B��B��B��B��B��B��B�kB�LB�2B�B��B{�Bt�Bq�BltBgXBdEBc@Bb:Ba3B_&B\BZBW�BT�BP�BM�BK�BJ�BI�BI�BH�BF�BD�BB{B?gB=_B=BB<UB<VB;QB:GB8=B5)B4"B3B2B1�B2B1B0�B0B/B/B/B-�B,�B,�B+�B*�B)�B'�B$�B!�B$�B$�B$�B%�B%�B$�B#�B$�B"�B&�B'�B'�B&�B&�B%�B$�B$�B$�B#�B!�B"�B"�B$�B%�B$�B#�B#�B$�B"�B#�B"�B �B�B�BzB�B�B�B�B�B�B �B �B �B"�B$�B%�B&�B'�B&�B&�B)�B*�B+�B/B1B3B5(B5'B5(B74B9@B:EB:EB<PB>^B@jBE�BF�BH�BL�BL�BM�BR�BV�BW�BX�BX�BX�BX�B[	B\B^Bc9Bd>BeDBfKBkiBlpBq�Bs�Bu�Bv�By�B�B��B�B�*B�2B�EB�JB�JB�VB�VB�bB�gB�pB�vB�}B�zB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�2B�2B�2B�>B�\B�vB�{B�BƆBǍBʡBηB��B��B��B�B�(B�]B�|B�B��B��B��B	�B	�B	�B	�B	�B	B	B		B	
B	 B	B	B	.B	1B	4B	7B	9B	JB	SB	^B	oB	|B	�B	*�B	,�B	.�B	1B	1B	3B	3B	3B	4B	4B	6 B	:9B	::B	;?B	=IB	EB	J�B	L�B	L�B	M�B	M�B	M�B	O�B	Q�B	R�B	T�B	W�B	Z�B	\ B	\ B	\B	\B	_B	b%B	gGB	jXB	lcB	nsB	ouB	p{B	p|B	r�B	s�B	t�B	t�B	u�B	u�B	v�B	v�B	w�B	y�B	z�B	|�B	�B	��B	��B	�B	�B	�#B	�!B	�'B	�'B	�'B	�/B	�@B	�EB	�UB	�VB	�eB	�vB	��B	ȊB	�5B	�B
 B
5B
sB
$�B
,�B
8'B
>KB
EuB
L�B
T�B
Y�B
`B
d-B
hFB
l_B
orB
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708212016053117082120160531170821  AO  ARCAADJP                                                                    20141004120123    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141004120123  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141004120123  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170821  IP                  G�O�G�O�G�O�                