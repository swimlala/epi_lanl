CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-07T03:15:38Z AOML 3.0 creation; 2016-06-01T00:08:23Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150307031538  20160531170823  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               lA   AO  4055_7112_108                   2C  D   APEX                            5374                            041511                          846 @�?x&�1   @�?x� ?�@:�+J�d6��`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    lA   A   A   @�ff@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�C3D�� D���D���D�,�D�vfD��fD�3D�P D�y�Dǩ�D�fD�I�Dڜ�D�� D�3D�9�D�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�(�A{A&{AF{Ag�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�=qC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+�D+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp��DqRDq�RDrRDr�RDsRDs�RDtRDt�RDu�Dy��D�%�D�O\D��)D���D��D�8�D���D�ҏD�\D�\)D���Dǵ�D�"�D�U�Dڨ�D��)D�\D�E�D��D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�x�A�x�A�|�A�|�A�|�A�|�A�|�A�z�A�A�A��#A��DA�ĜA�dZA��A��#A�A���A���A��A��wA��A�p�A�jA�ffA�XA�&�A��TA���A��^A��A�ffA�9XA��A�ƨA��FA���A�VA��yA���A���A���A�O�A�=qA��TA�(�A�VA�9XA� �A��A��7A�XA�`BA�jA��A��A�hsA���A�ZA��TA�dZA��A�`BA�A~�DA{��AyXAv=qAs�wAsp�ArjAqp�Ao��Al��AjE�Ai&�Ag`BAe�FAdz�Ac�;Ac33A_�PA]�-A\�A\I�A[AY\)AXjAWl�AV��AVn�AU�AU
=AT�RAT-AS��AS?}ARffAQ`BAP~�AP�AO�AO�^AO�AOO�ANAJv�AJ  AI�7AIp�AI33AIoAH�/AH1'AG
=AFI�AE��ADv�AC��ACAB-AA�A@^5A?dZA>bNA=��A=K�A<ĜA;��A9K�A7�A6��A6�\A6r�A6A5
=A4VA3dZA2$�A0�/A0ffA/A.bA*M�A)33A(��A(��A'�mA'oA&�`A&ĜA& �A%+A$�A$bNA$9XA#�TA#\)A!��A ȴA E�A/A-Ax�A�An�A  A�#A��A�^AC�A�jAE�A�A �A�jA��A�!A5?A�A��AdZA;dAoA��A��A��A|�A�AbA��A&�A
�/A
��A
�jA
�A
�uA
v�A
 �A	�A	��A	�^A	A�;AO�A�wAȴA�/A(�A7LA ff@��@�E�@�?}@��@�o@���@�j@���@�@���@��`@�@�@��@�p�@���@�I�@� �@��T@�r�@�33@��@�ȴ@�V@�bN@߅@ާ�@���@��@ە�@��@�Ĝ@���@�"�@�J@�X@Դ9@�ƨ@ӍP@�ȴ@҇+@�V@�G�@��@�t�@�33@��@�ff@���@�ȴ@���@�z�@ǅ@�
=@���@�V@��`@�A�@��y@��;@���@�C�@�J@�x�@��u@�ƨ@�;d@��!@�@���@��j@�1'@�S�@�33@�ff@���@��@�O�@���@�Ĝ@�t�@��@��-@�/@��u@�ƨ@�t�@�@��@�ȴ@���@�%@��j@��;@�
=@��h@�j@�1'@��F@���@���@��@�z�@�bN@�Z@�I�@�9X@��w@�J@�p�@���@��w@�V@��@���@��@�7L@�r�@���@�o@���@�J@��T@���@��^@�x�@�X@�%@��@�Z@���@���@���@��P@��P@�|�@�t�@�dZ@�S�@��@��R@��\@�n�@�V@��@�@��T@��-@�x�@�7L@���@�1@��@�|�@�|�@�+@���@��+@�5?@��@���@���@�l�@�C�@�33@���@��R@�~�@�M�@�-@���@��T@��-@���@��7@�X@�?}@�&�@�V@��`@��@�Z@�(�@��;@���@�|�@�K�@�"�@��@�
=@��@���@���@�^5@�-@�J@��#@��^@���@���@��@�/@��u@�1@l�@�@~��@~v�@~E�@}�@}�@}O�@}V@|�j@{t�@z�\@zJ@y&�@x�9@xQ�@xA�@x1'@xA�@xbN@xr�@x�@xQ�@xb@w�;@w��@w|�@w+@v�@vȴ@vȴ@v�R@v�+@vV@vE�@vE�@v5?@v{@v@u�T@u@u��@up�@u�@t��@t�D@tz�@tj@tI�@tZ@tj@tj@tZ@sS�@qG�@d�j@[��@W�@R�@K�F@Fff@=�@8r�@3o@*J@$�/@\)@��@O�@��@/@��@�@"�@ Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�t�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�t�A�v�A�v�A�x�A�x�A�|�A�|�A�|�A�|�A�|�A�z�A�A�A��#A��DA�ĜA�dZA��A��#A�A���A���A��A��wA��A�p�A�jA�ffA�XA�&�A��TA���A��^A��A�ffA�9XA��A�ƨA��FA���A�VA��yA���A���A���A�O�A�=qA��TA�(�A�VA�9XA� �A��A��7A�XA�`BA�jA��A��A�hsA���A�ZA��TA�dZA��A�`BA�A~�DA{��AyXAv=qAs�wAsp�ArjAqp�Ao��Al��AjE�Ai&�Ag`BAe�FAdz�Ac�;Ac33A_�PA]�-A\�A\I�A[AY\)AXjAWl�AV��AVn�AU�AU
=AT�RAT-AS��AS?}ARffAQ`BAP~�AP�AO�AO�^AO�AOO�ANAJv�AJ  AI�7AIp�AI33AIoAH�/AH1'AG
=AFI�AE��ADv�AC��ACAB-AA�A@^5A?dZA>bNA=��A=K�A<ĜA;��A9K�A7�A6��A6�\A6r�A6A5
=A4VA3dZA2$�A0�/A0ffA/A.bA*M�A)33A(��A(��A'�mA'oA&�`A&ĜA& �A%+A$�A$bNA$9XA#�TA#\)A!��A ȴA E�A/A-Ax�A�An�A  A�#A��A�^AC�A�jAE�A�A �A�jA��A�!A5?A�A��AdZA;dAoA��A��A��A|�A�AbA��A&�A
�/A
��A
�jA
�A
�uA
v�A
 �A	�A	��A	�^A	A�;AO�A�wAȴA�/A(�A7LA ff@��@�E�@�?}@��@�o@���@�j@���@�@���@��`@�@�@��@�p�@���@�I�@� �@��T@�r�@�33@��@�ȴ@�V@�bN@߅@ާ�@���@��@ە�@��@�Ĝ@���@�"�@�J@�X@Դ9@�ƨ@ӍP@�ȴ@҇+@�V@�G�@��@�t�@�33@��@�ff@���@�ȴ@���@�z�@ǅ@�
=@���@�V@��`@�A�@��y@��;@���@�C�@�J@�x�@��u@�ƨ@�;d@��!@�@���@��j@�1'@�S�@�33@�ff@���@��@�O�@���@�Ĝ@�t�@��@��-@�/@��u@�ƨ@�t�@�@��@�ȴ@���@�%@��j@��;@�
=@��h@�j@�1'@��F@���@���@��@�z�@�bN@�Z@�I�@�9X@��w@�J@�p�@���@��w@�V@��@���@��@�7L@�r�@���@�o@���@�J@��T@���@��^@�x�@�X@�%@��@�Z@���@���@���@��P@��P@�|�@�t�@�dZ@�S�@��@��R@��\@�n�@�V@��@�@��T@��-@�x�@�7L@���@�1@��@�|�@�|�@�+@���@��+@�5?@��@���@���@�l�@�C�@�33@���@��R@�~�@�M�@�-@���@��T@��-@���@��7@�X@�?}@�&�@�V@��`@��@�Z@�(�@��;@���@�|�@�K�@�"�@��@�
=@��@���@���@�^5@�-@�J@��#@��^@���@���@��@�/@��u@�1@l�@�@~��@~v�@~E�@}�@}�@}O�@}V@|�j@{t�@z�\@zJ@y&�@x�9@xQ�@xA�@x1'@xA�@xbN@xr�@x�@xQ�@xb@w�;@w��@w|�@w+@v�@vȴ@vȴ@v�R@v�+@vV@vE�@vE�@v5?@v{@v@u�T@u@u��@up�@u�@t��@t�D@tz�@tj@tI�@tZ@tj@tj@tZ@sS�@qG�@d�j@[��@W�@R�@K�F@Fff@=�@8r�@3o@*J@$�/@\)@��@O�@��@/@��@�@"�@ Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�sB�NB�)BɺB�!B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B�uB�oB�hB�7B}�Bz�Bv�Br�BiyBN�BC�B1'B�B�BuB1B�5B��B�B#�B  B
�)B
��B
�-B
�B
��B
��B
�hB
� B
s�B
ffB
J�B
49B
�B
	7B
B	��B	�B	�`B	��B	�XB	�'B	��B	��B	�oB	�PB	�B	v�B	m�B	hsB	cTB	`BB	]/B	[#B	]/B	cTB	cTB	]/B	]/B	_;B	_;B	]/B	YB	T�B	Q�B	N�B	N�B	N�B	N�B	M�B	K�B	D�B	6FB	33B	1'B	0!B	/B	-B	+B	'�B	!�B	�B	�B	oB	PB	
=B	%B	B	B��B��B��B��B��B��B�TB��B��B�;B�BB�/B�B�;B�/B�B��B��BƨB�qB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�=B�1B�%B�B�B�B�B� B~�B|�By�Bt�Bo�BhsBbNB^5B\)B[#B[#BZBYBXBVBQ�BM�BJ�BH�BF�BE�BD�BC�BC�BB�BB�BB�BA�B@�B@�BA�B@�B=qB;dB7LB33B/B,B)�B'�B&�B%�B$�B#�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B{B{BuBoBhBbB\B\B\B\BbBhBhBbBbBbB\BbB\B\BbB\BbBoBoB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B+B-B.B/B0!B0!B0!B1'B1'B1'B2-B5?B6FB5?B6FB<jB@�BB�BA�BD�BJ�BJ�BL�BN�BP�BQ�BQ�BQ�BP�BR�BW
BW
B[#B`BBhsBo�Bp�Bq�Bs�Bu�Bw�By�By�By�By�By�B{�B�B�7B�DB�hB��B��B��B��B��B��B��B�B�B�9B�?B�FB�FB�RB�XB�dB�wB��BÖBĜBŢBŢBŢBƨBƨBƨBǮBȴB��B��B��B��B��B��B��B��B��B�B�#B�BB�`B�fB�`B�sB�B�B�B��B	B	B	B	+B	1B	
=B	PB	bB	hB	uB	�B	�B	�B	�B	�B	"�B	$�B	%�B	'�B	)�B	,B	/B	2-B	5?B	7LB	8RB	;dB	<jB	<jB	=qB	>wB	@�B	B�B	E�B	G�B	I�B	L�B	N�B	O�B	P�B	Q�B	VB	]/B	e`B	iyB	k�B	m�B	o�B	o�B	q�B	s�B	t�B	u�B	w�B	}�B	�B	�B	�B	�+B	�7B	�=B	�DB	�VB	�VB	�\B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	�mB	��B
B
VB
�B
'�B
/B
6FB
B�B
J�B
P�B
XB
_;B
dZB
iyB
o�B
r�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B�PB�,B�BɗB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�kB�OB�IB�CB�B}�Bz�Bv�Br�BiSBN�BCnB0�B�BkBMBB�B��B��B#�B
��B
�B
�gB
�
B
��B
��B
�rB
�DB
�B
s�B
fEB
J�B
4B
�B
	B
 B	��B	�B	�CB	˨B	�<B	�
B	��B	��B	�PB	�3B	�B	v�B	muB	hXB	c;B	`%B	]B	[B	]B	c8B	c8B	]B	]B	_B	_B	]B	X�B	T�B	Q�B	N�B	N�B	N�B	N�B	M�B	K�B	D�B	6*B	3B	1B	0B	/B	,�B	*�B	'�B	!�B	�B	B	TB	7B	
"B	B	�B	 �B��B��B��B��B��B��B�<B��B��B�"B�(B�B��B�#B�B��B��B˯BƐB�VB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�jB�\B�JB�9B�%B�B�B�B�B��B��B�B~�B|�By�Bt�Bo�Bh_Bb6B^!B\B[B[BZBY BW�BU�BQ�BM�BJ�BH�BF�BE�BD�BC�BC�BBxBB|BByBAtB@oB@mBAwB@nB=]B;OB76B3B/B+�B)�B'�B&�B%�B$�B#�B#�B"�B!�B!�B �B�B�B�B}BxB�BkB~BlBNBhBDB?B:BLBFB,BFB-BMBSB9BLB0BNB,B1BEBFB2BDB2BWBYBOBgB�B�B�B�BlBvBsB�B�B�B�B�B�B�B�B!�B$�B*�B,�B-�B/ B0
B0	B0B1B1B1B2B5%B6-B5%B6+B<PB@jBBwBAmBDBJ�BJ�BL�BN�BP�BQ�BQ�BQ�BP�BR�BV�BV�B[
B`(BhWBo�Bp�Bq�Bs�Bu�Bw�By�By�By�By�By�B{�B� B�B�$B�HB��B��B��B��B��B��B��B��B��B�B�"B�%B�&B�/B�7B�BB�WB�aB�tB�~BŁB�BŀBƄBƇBƇBǌBȓBʡB̪BͰBͱBϾB��B��B��B��B��B� B�B�:B�BB�=B�PB�aB�lB�{B��B	 �B	�B	�B	B	B	
B	(B	>B	EB	NB	\B	bB	{B	B	�B	"�B	$�B	%�B	'�B	)�B	+�B	.�B	2B	5B	7&B	8.B	;<B	<DB	<DB	=IB	>SB	@]B	BhB	E{B	G�B	I�B	L�B	N�B	O�B	P�B	Q�B	U�B	]B	e9B	iOB	k^B	miB	owB	ovB	q�B	s�B	t�B	u�B	w�B	}�B	��B	��B	��B	�B	�B	�B	�B	�,B	�.B	�3B	�3B	�@B	�QB	�WB	�cB	�fB	�rB	�wB	�yB	�wB	�tB	�{B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�CB	��B
 �B
)B
hB
'�B
.�B
6B
BaB
J�B
P�B
W�B
_B
d*B
iKB
onB
r�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708232016053117082320160531170823  AO  ARCAADJP                                                                    20150307031538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150307031538  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150307031538  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170823  IP                  G�O�G�O�G�O�                