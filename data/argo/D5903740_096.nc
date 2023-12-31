CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-11-04T06:00:54Z AOML 3.0 creation; 2016-06-01T00:08:21Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141104060054  20160531170821  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               `A   AO  4055_7112_096                   2C  D   APEX                            5374                            041511                          846 @� ��. 1   @� �]��@9�\(��d 9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    `A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��DyffD��D�@ D�ffD�� D��D�I�D�s3D�� D��D�P D�� D�� D� D�  Dډ�D��fD��fD�<�D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B���B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt�Dy~�D�%�D�L)D�r�D��)D�(�D�U�D�\D��)D�%�D�\)D��)D��)D�)D�,)Dڕ�D�ҏD��D�H�D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϏ\A�r�A�$�A���Aδ9AΩ�AΡ�AΛ�AΕ�A΋DA·+A�dZA�O�A�I�A�;dA�-A�"�A�{A��A��A�9XA�7LA�t�A�33A�C�A��
A��A�hsA��A�1'A�A�dZA��PA��hA�p�A��TA�;dA�JA�A�A��hA�?}A�\)A�(�A�A�-A�t�A�G�A�=qA�VA�A�oA�dZA��RA�VA��uA�VA���A�p�A��\A��A���A��hA��RA��A��RA��TA�t�A�M�A��9A��\A��A��A��A�hsA���A��!A�%A�ȴA�&�A��A��\A�x�A���A�&�A��A~�A~�A{dZAw��Aw
=Aux�As\)AqXAp�uAo�AoO�An��An=qAm?}Al^5Ak�#Ak�PAk/Aj�/Aj^5Ai��AghsAe�Ab�!A`v�A_��A^�A^1A]�FA]dZA\�yA[�TAZ^5AW�wAVĜAV�AT��AT~�AT$�AS��AS\)AQ�AOXAM�AL��AK�mAK�AI|�AH�\AG��AG%AD�yAC;dACAB��AB^5ABAA�-AAl�A@v�A?VA=�#A=�A;�A;;dA:��A:ȴA:jA:{A9�TA8�A7S�A6{A4ffA3��A3%A2I�A1�;A1XA0ĜA0bNA0A/�hA.��A-oA,^5A+�A+p�A*�A)O�A)�A(ĜA(ffA'��A'"�A&�9A&A%�hA$~�A#�hA"E�A!/A ��A��A�A�PA{Ap�A��AffA"�A�A��A��Ax�AVA��A��A
=A�A$�A��AVA�!A�A��A��An�A��A�PAx�A�uA
��A	��A	VAn�A&�AA�A�mA��AC�A�uAS�A Q�@�ȴ@�^5@�&�@�r�@�$�@�-@�%@�33@���@�S�@�7L@�\)@�ff@�@�9@�ȴ@�b@�  @��m@�\)@�@��@�V@�5?@��@ٲ-@��@�r�@��@ׅ@�M�@ԣ�@�S�@�"�@�"�@��@��@�"�@�o@җ�@Ѳ-@���@�A�@ϕ�@ͺ^@�j@˕�@�ff@�A�@ǥ�@�@�@Ĭ@�S�@���@�%@���@��+@��@��`@��;@�t�@��y@��@��@�l�@�n�@��@���@��;@�+@�{@�@�x�@�V@���@�ȴ@��\@�7L@�r�@��@��m@�\)@��T@�G�@��j@��F@�S�@�;d@�@���@�ȴ@���@��!@��\@�ff@�E�@�5?@���@��@�r�@��@���@���@�=q@��h@�r�@��@�S�@��@��T@��j@�Z@��;@�dZ@�
=@��+@�p�@��/@�j@���@��@��R@�$�@���@���@�G�@� �@��@�+@���@���@�V@�@��^@�/@��@��j@�r�@� �@��m@���@��H@�{@���@���@�p�@�%@��9@��@�bN@��w@�t�@�S�@�;d@�
=@��+@���@�@�hs@�&�@��@��@��@��@��u@���@��9@�Ĝ@�Ĝ@��9@�Z@�b@�ƨ@�S�@�"�@���@��@��H@���@�n�@�M�@�J@��#@��#@��^@��@��7@�x�@�?}@�&�@��@���@���@���@�z�@�I�@��@�1@��m@��@���@���@�t�@�33@��R@�~�@�~�@�n�@�V@�=q@�@��^@��7@�X@�/@���@���@���@�Ĝ@��@���@���@��@�bN@�Q�@�I�@�b@�w@�@l�@~��@~V@~@}�-@}/@|��@|�@|j@|(�@{�
@{��@{S�@{"�@{"�@{"�@z��@z-@y�@y�7@x�9@xr�@x1'@wl�@v�y@v�R@vff@u�@u?}@s�
@h1'@^��@X�`@Rn�@L��@E�@>v�@:�@3dZ@,��@(1'@!��@Z@1'@I�@��@O�@
-@ff@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϏ\A�r�A�$�A���Aδ9AΩ�AΡ�AΛ�AΕ�A΋DA·+A�dZA�O�A�I�A�;dA�-A�"�A�{A��A��A�9XA�7LA�t�A�33A�C�A��
A��A�hsA��A�1'A�A�dZA��PA��hA�p�A��TA�;dA�JA�A�A��hA�?}A�\)A�(�A�A�-A�t�A�G�A�=qA�VA�A�oA�dZA��RA�VA��uA�VA���A�p�A��\A��A���A��hA��RA��A��RA��TA�t�A�M�A��9A��\A��A��A��A�hsA���A��!A�%A�ȴA�&�A��A��\A�x�A���A�&�A��A~�A~�A{dZAw��Aw
=Aux�As\)AqXAp�uAo�AoO�An��An=qAm?}Al^5Ak�#Ak�PAk/Aj�/Aj^5Ai��AghsAe�Ab�!A`v�A_��A^�A^1A]�FA]dZA\�yA[�TAZ^5AW�wAVĜAV�AT��AT~�AT$�AS��AS\)AQ�AOXAM�AL��AK�mAK�AI|�AH�\AG��AG%AD�yAC;dACAB��AB^5ABAA�-AAl�A@v�A?VA=�#A=�A;�A;;dA:��A:ȴA:jA:{A9�TA8�A7S�A6{A4ffA3��A3%A2I�A1�;A1XA0ĜA0bNA0A/�hA.��A-oA,^5A+�A+p�A*�A)O�A)�A(ĜA(ffA'��A'"�A&�9A&A%�hA$~�A#�hA"E�A!/A ��A��A�A�PA{Ap�A��AffA"�A�A��A��Ax�AVA��A��A
=A�A$�A��AVA�!A�A��A��An�A��A�PAx�A�uA
��A	��A	VAn�A&�AA�A�mA��AC�A�uAS�A Q�@�ȴ@�^5@�&�@�r�@�$�@�-@�%@�33@���@�S�@�7L@�\)@�ff@�@�9@�ȴ@�b@�  @��m@�\)@�@��@�V@�5?@��@ٲ-@��@�r�@��@ׅ@�M�@ԣ�@�S�@�"�@�"�@��@��@�"�@�o@җ�@Ѳ-@���@�A�@ϕ�@ͺ^@�j@˕�@�ff@�A�@ǥ�@�@�@Ĭ@�S�@���@�%@���@��+@��@��`@��;@�t�@��y@��@��@�l�@�n�@��@���@��;@�+@�{@�@�x�@�V@���@�ȴ@��\@�7L@�r�@��@��m@�\)@��T@�G�@��j@��F@�S�@�;d@�@���@�ȴ@���@��!@��\@�ff@�E�@�5?@���@��@�r�@��@���@���@�=q@��h@�r�@��@�S�@��@��T@��j@�Z@��;@�dZ@�
=@��+@�p�@��/@�j@���@��@��R@�$�@���@���@�G�@� �@��@�+@���@���@�V@�@��^@�/@��@��j@�r�@� �@��m@���@��H@�{@���@���@�p�@�%@��9@��@�bN@��w@�t�@�S�@�;d@�
=@��+@���@�@�hs@�&�@��@��@��@��@��u@���@��9@�Ĝ@�Ĝ@��9@�Z@�b@�ƨ@�S�@�"�@���@��@��H@���@�n�@�M�@�J@��#@��#@��^@��@��7@�x�@�?}@�&�@��@���@���@���@�z�@�I�@��@�1@��m@��@���@���@�t�@�33@��R@�~�@�~�@�n�@�V@�=q@�@��^@��7@�X@�/@���@���@���@�Ĝ@��@���@���@��@�bN@�Q�@�I�@�b@�w@�@l�@~��@~V@~@}�-@}/@|��@|�@|j@|(�@{�
@{��@{S�@{"�@{"�@{"�@z��@z-@y�@y�7@x�9@xr�@x1'@wl�@v�y@v�R@vff@u�@u?}@s�
@h1'@^��@X�`@Rn�@L��@E�@>v�@:�@3dZ@,��@(1'@!��@Z@1'@I�@��@O�@
-@ff@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBo�Bn�Bm�Bl�Bl�Bl�Bl�Bk�Bk�Bk�Bk�BjBiyBiyBhsBgmBgmBe`B]/BuB�B�DB^5BVBP�BJ�B=qB49B-B#�B�B�B�BoBDB%B��B��B�B�fB�B��B��BȴB��B�-B��B��B��B�1B}�Bx�Br�Bk�BdZB[#BVBJ�BD�B<jB49B-B�BoB�B�)B��B�RB�B��B� BjBN�BB�B49BhBB
��B
�/B
ƨB
��B
�+B
x�B
p�B
bNB
\)B
VB
<jB
#�B
�B
\B
B	��B	�B	�B	�sB	�ZB	�;B	��B	��B	ǮB	ĜB	��B	�dB	�?B	�B	��B	�B	q�B	ffB	_;B	ZB	YB	XB	T�B	Q�B	K�B	B�B	7LB	33B	/B	(�B	'�B	%�B	"�B	�B	�B	�B	�B	�B	{B	hB	VB	JB		7B	%B	B��B��B��B��B��B��B��B�B�B�B�yB�fB�`B�ZB�TB�NB�HB�BB�#B�
B��B��BȴBŢBB��B�wB�jB�^B�RB�?B�'B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�=B�%B�B�B� B}�By�Bu�Bo�Bk�BiyBgmBbNB_;B]/B\)BZBVBQ�BN�BL�BJ�BI�BG�BF�BE�BC�BA�B@�B>wB=qB<jB:^B6FB49B2-B0!B-B(�B$�B"�B!�B �B�B�B�B�B�B�B�BuBbB\BVBJBDB
=B	7B	7B1B+B+B%B%B%BBB��BBBBB  B  B  B  B��B��B  BBBBBBBBBBBBB%B%B+B	7BJB
=B	7BPBhB�B�B�B �B"�B#�B$�B'�B'�B(�B)�B/B1'B33B7LB7LB8RB:^B=qB=qB>wB>wBB�BE�BE�BI�BL�BM�BN�BN�BT�BW
BYB^5B`BB`BBaHBbNBbNBcTBcTBdZBdZBe`Be`BffBl�Bo�Bp�Bw�Bz�B|�B� B�+B�=B�PB�VB��B��B��B��B��B��B��B�B�B�'B�3B�?B�LB�XB�dB�dB�wBĜBǮBȴB��B��B��B��B��B�B�B�#B�5B�BB�HB�NB�fB�B�B�B�B�B��B��B��B��B	B	B	B	B		7B	JB	VB	hB	{B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	)�B	,B	/B	1'B	33B	7LB	9XB	;dB	;dB	<jB	=qB	@�B	B�B	E�B	G�B	G�B	H�B	J�B	J�B	J�B	M�B	M�B	N�B	O�B	Q�B	S�B	T�B	W
B	YB	ZB	[#B	^5B	^5B	_;B	`BB	bNB	e`B	hsB	iyB	jB	jB	k�B	l�B	n�B	o�B	q�B	r�B	t�B	v�B	v�B	w�B	w�B	x�B	x�B	y�B	z�B	{�B	{�B	~�B	� B	�B	�B	�B	�1B	�=B	�DB	�PB	�VB	�VB	�\B	�bB	�hB	�oB	�{B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�TB	�B
  B
VB
�B
%�B
-B
8RB
@�B
F�B
O�B
VB
\)B
aHB
e`B
jB
m�B
q�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bo�Bn�BmBlxBlyBlyBlvBkuBktBkpBktBjjBibBicBhcBgZBg^BeMB]BaB��B�&B^BU�BP�BJ�B=SB4B,�B#�B�B�BvBRB%BB��B��B�pB�CB��B��BθBȕB�bB�B��B��B�_B�B}�Bx�Br�BkcBd3B[ BU�BJ�BDyB<DB4B,�B�BLB�B�BϻB�-B��B�[B�BjZBN�BBlB4BEB�B
��B
�
B
ƂB
��B
�
B
x�B
p�B
b.B
\
B
U�B
<HB
#�B
�B
?B
�B	��B	�B	�hB	�SB	�;B	�B	��B	̮B	ǑB	�B	�dB	�JB	�!B	��B	�cB	�B	q�B	fJB	_"B	ZB	X�B	W�B	T�B	Q�B	K�B	BsB	7/B	3B	/B	(�B	'�B	%�B	"�B	�B	�B	�B	sB	hB	bB	OB	=B	0B		B	B	 �B��B��B��B��B��B��B��B�B�~B�nB�aB�MB�HB�CB�=B�4B�1B�)B�
B��B��B˭BȝBŊB�vB�jB�^B�RB�HB�9B�)B�B��B��B��B��B��B��B��B��B��B�rB�kB�^B�LB�?B�(B�B��B��B�B}�By�Bu�Bo�BkqBicBgWBb8B_'B]B\BZ
BU�BQ�BN�BL�BJ�BI�BG�BF�BE�BC�BAsB@mB>aB=DB<UB:JB62B4&B2B0
B,�B(�B$�B"�B!�B �B�B�BsB�ByBXBnBaB5BGB(BBB
B	B	&BBBB�B�B�B�B�B��B �B �B �B �B��B��B��B��B��B��B��B�B�B�B�B�B�B �B�B�B�B�B�B�B�BB	B4B
B	B:B7BVB�B�B �B"�B#�B$�B'�B'�B(�B)�B/B1B3B73B73B88B:EB=WB=YB>]B>_BBtBE�BE�BI�BL�BM�BN�BN�BT�BV�BX�B^B`'B`&Ba,Bb4Bb2Bc8Bc7Bd=Bd>BeEBeCBfKBlnBo�Bp�Bw�Bz�B|�B�B�B�B�2B�8B�oB��B��B��B��B��B��B��B��B�B�B� B�-B�7B�EB�CB�VB�{BǉBȓBʠB̫BͳB��B��B��B��B�B�B�!B�$B�/B�CB�gB�vB�rB�|B�B��B��B��B��B	 �B	�B	�B	�B		B	%B	3B	FB	XB	cB	tB	}B	�B	!�B	#�B	$�B	%�B	)�B	+�B	.�B	1B	3B	7&B	92B	;?B	;@B	<CB	=MB	@]B	BjB	E~B	G�B	G�B	H�B	J�B	J�B	J�B	M�B	M�B	N�B	O�B	Q�B	S�B	T�B	V�B	X�B	Y�B	Z�B	^B	^B	_B	`B	b)B	e9B	hLB	iSB	jUB	jWB	k^B	ldB	nrB	ovB	q�B	r�B	t�B	v�B	v�B	w�B	w�B	x�B	x�B	y�B	z�B	{�B	{�B	~�B	�B	��B	��B	��B	�B	�B	�B	�(B	�-B	�+B	�2B	�:B	�>B	�GB	�RB	�XB	�QB	�QB	�_B	�dB	�iB	�jB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	˚B	�*B	�B	��B
)B
zB
%�B
,�B
8&B
@VB
FyB
O�B
U�B
[�B
aB
e2B
jQB
mdB
q|B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708212016053117082120160531170821  AO  ARCAADJP                                                                    20141104060054    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141104060054  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141104060054  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170821  IP                  G�O�G�O�G�O�                