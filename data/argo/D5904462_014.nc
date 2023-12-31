CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:14Z AOML 3.0 creation; 2016-08-07T21:51:11Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221414  20160807145111  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_014                   2C  D   APEX                            6529                            072314                          846 @����?�1   @��/h`@2�\(��c�$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�� D�@ D��3D���D���D�S3D��fD��fD�	�D�VfD��3D��fD���D�9�D�p D��D��D�<�D�vfD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B��\B�B�Bȏ\B�B�B�B�B�B���B���B�B�\B�B�B�B�C aHCz�Cz�CaHCaHC
aHCaHCaHCaHCaHCaHCaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRz�CTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDu�Dy��D��)D�L)D��\D���D��D�_\D���D�D��D�b�D��\D��D���D�E�D�|)D���D�%�D�H�D�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�~�A܃A܃A܅A܇+A܇+A܉7A܋DA܍PA܍PA܍PA܅A�z�A�`BA�M�A�G�A�G�A�G�A�G�A�E�A�C�A�C�A�E�A�E�A�E�A�G�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�A�A�?}A�=qA�;dA�;dA�1'A�%A۸RA�dZA��A��yA��AБhAϡ�A�ƨA�=qAɑhA�`BA�M�A�  A�|�A��yA���AƸRA���A�t�A��A��uA��TA���A���A���A�
=A�XA���A�ĜA��
A���A��A��;A�-A��A���A��A�  A�(�A���A�ffA��HA�+A���A���A�+A��A���A��yA�ƨA�~�A��A�O�A�A�A�K�A��7A�JA���A��A��mA�O�A{?}At��As%Ao��Ak�AedZAc��A`�A]hsAY�7AUO�AS�AQ�AOS�AM33AKXAJZAI��AE�wAC�;AA�A@r�A>��A=p�A;K�A:Q�A9+A7�hA6��A6JA2�A0��A/�^A/A,��A*r�A*9XA(��A'��A&ȴA%��A%x�A$E�A"�yA"1'A!x�A �At�AO�Ax�A�A�7A��AZA��A33A�!A�
A(�A7LAQ�A�#Ax�A%A-A7LAr�A�A��A�Ar�A��A�HAz�A	�wAv�A
=A�\A��A/A��A��A�A��A�#AO�A �yA �9A   @�~�@���@��@�j@�A�@��
@��R@���@���@�7L@��m@��@��F@��j@���@���@��
@�Z@��u@�Q�@�o@�-@��7@��/@�D@�  @��@��@�`B@�Z@�K�@��y@�~�@��@�?}@�1@�\)@��@��@�P@�o@��@�p�@���@�A�@�1@�b@�b@�@�ȴ@�^5@��@�J@�E�@��y@�"�@��@��H@�v�@��@�t�@�$�@�X@���@�z�@���@�33@ۥ�@܃@���@�(�@ڏ\@٩�@�x�@�{@�J@��T@���@�p�@�G�@��@��`@�z�@ׅ@��H@��H@�ȴ@֏\@�5?@�^5@�ff@պ^@�p�@�G�@ԛ�@�S�@ҟ�@�E�@�E�@�M�@���@���@Л�@�I�@�b@��;@�l�@��y@�=q@���@�x�@�&�@̃@�9X@ˍP@�v�@�=q@�J@ə�@��`@�1@�b@�1@���@�\)@��y@�V@��@�x�@�%@���@�r�@�K�@�ȴ@\@+@�=q@���@�X@��j@�j@��m@�;d@��R@�~�@�{@�`B@���@��@�K�@�;d@��H@�$�@��-@�hs@���@���@��@���@�^5@���@��@��#@��^@�p�@��@��@�  @���@��F@���@��P@�S�@��@���@�ȴ@��!@���@��\@�~�@�E�@�J@��T@���@�p�@�j@��@�  @���@��@�33@��@���@�~�@�E�@�-@�-@�J@�J@�E�@�V@�J@���@�G�@���@��D@�Z@� �@�1@��
@�;d@��@�V@���@�Ĝ@���@�j@���@�S�@�+@��\@�-@���@�p�@�?}@��@��9@��u@�z�@�r�@�1'@��@�;d@�+@��@���@��H@�n�@�V@�V@�M�@�{@���@���@�x�@�hs@�`B@�7L@�O�@�%@��j@��@�Q�@�(�@�b@��@��
@���@�;d@��H@��+@�ff@�5?@��@��^@��7@�O�@��`@��@�b@��;@���@��@��@�t�@�;d@��@��@��7@�hs@��@���@��9@��u@�j@�A�@�1'@��;@���@�l�@�ȴ@�ff@�@�p�@�?}@���@�z�@���@�bN@�Ĝ@�hs@~��@s��@gK�@a%@Y��@R�!@I�@?�w@8��@3t�@.v�@(bN@"�@I�@��@�@E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�z�A�~�A܃A܃A܅A܇+A܇+A܉7A܋DA܍PA܍PA܍PA܅A�z�A�`BA�M�A�G�A�G�A�G�A�G�A�E�A�C�A�C�A�E�A�E�A�E�A�G�A�E�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�A�A�?}A�=qA�;dA�;dA�1'A�%A۸RA�dZA��A��yA��AБhAϡ�A�ƨA�=qAɑhA�`BA�M�A�  A�|�A��yA���AƸRA���A�t�A��A��uA��TA���A���A���A�
=A�XA���A�ĜA��
A���A��A��;A�-A��A���A��A�  A�(�A���A�ffA��HA�+A���A���A�+A��A���A��yA�ƨA�~�A��A�O�A�A�A�K�A��7A�JA���A��A��mA�O�A{?}At��As%Ao��Ak�AedZAc��A`�A]hsAY�7AUO�AS�AQ�AOS�AM33AKXAJZAI��AE�wAC�;AA�A@r�A>��A=p�A;K�A:Q�A9+A7�hA6��A6JA2�A0��A/�^A/A,��A*r�A*9XA(��A'��A&ȴA%��A%x�A$E�A"�yA"1'A!x�A �At�AO�Ax�A�A�7A��AZA��A33A�!A�
A(�A7LAQ�A�#Ax�A%A-A7LAr�A�A��A�Ar�A��A�HAz�A	�wAv�A
=A�\A��A/A��A��A�A��A�#AO�A �yA �9A   @�~�@���@��@�j@�A�@��
@��R@���@���@�7L@��m@��@��F@��j@���@���@��
@�Z@��u@�Q�@�o@�-@��7@��/@�D@�  @��@��@�`B@�Z@�K�@��y@�~�@��@�?}@�1@�\)@��@��@�P@�o@��@�p�@���@�A�@�1@�b@�b@�@�ȴ@�^5@��@�J@�E�@��y@�"�@��@��H@�v�@��@�t�@�$�@�X@���@�z�@���@�33@ۥ�@܃@���@�(�@ڏ\@٩�@�x�@�{@�J@��T@���@�p�@�G�@��@��`@�z�@ׅ@��H@��H@�ȴ@֏\@�5?@�^5@�ff@պ^@�p�@�G�@ԛ�@�S�@ҟ�@�E�@�E�@�M�@���@���@Л�@�I�@�b@��;@�l�@��y@�=q@���@�x�@�&�@̃@�9X@ˍP@�v�@�=q@�J@ə�@��`@�1@�b@�1@���@�\)@��y@�V@��@�x�@�%@���@�r�@�K�@�ȴ@\@+@�=q@���@�X@��j@�j@��m@�;d@��R@�~�@�{@�`B@���@��@�K�@�;d@��H@�$�@��-@�hs@���@���@��@���@�^5@���@��@��#@��^@�p�@��@��@�  @���@��F@���@��P@�S�@��@���@�ȴ@��!@���@��\@�~�@�E�@�J@��T@���@�p�@�j@��@�  @���@��@�33@��@���@�~�@�E�@�-@�-@�J@�J@�E�@�V@�J@���@�G�@���@��D@�Z@� �@�1@��
@�;d@��@�V@���@�Ĝ@���@�j@���@�S�@�+@��\@�-@���@�p�@�?}@��@��9@��u@�z�@�r�@�1'@��@�;d@�+@��@���@��H@�n�@�V@�V@�M�@�{@���@���@�x�@�hs@�`B@�7L@�O�@�%@��j@��@�Q�@�(�@�b@��@��
@���@�;d@��H@��+@�ff@�5?@��@��^@��7@�O�@��`@��@�b@��;@���@��@��@�t�@�;d@��@��@��7@�hs@��@���@��9@��u@�j@�A�@�1'@��;@���@�l�@�ȴ@�ff@�@�p�@�?}@���G�O�@���@�bN@�Ĝ@�hs@~��@s��@gK�@a%@Y��@R�!@I�@?�w@8��@3t�@.v�@(bN@"�@I�@��@�@E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B$�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B"�B"�B"�B"�B"�B!�B�B�BPBoB �B"�B+B/BN�B�=B��B�B�B�LBĜB�
B�B�B�B�B�B+BgmBjBk�B�B�7B�JB��B�B��B��B��BɺB��B��BǮB�!B�Bl�BoB�HB�RB��B�B��B��B��Bz�BI�B(�BB
�TB
��B
ÖB
��B
{�B
R�B
)�B
�B
B	�fB	�dB	��B	�DB	y�B	ffB	K�B	?}B	,B	bB��B�B�sB�`B�HB�;B�B��B��BȴBŢBȴBȴBŢB��B��B��B��BÖBÖB��B�}B�qB�qB�dBĜBɺB��BȴB��B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�HB�NB�TB�ZB�`B�`B�mB�sB�fB�sB�B	B	B	B	B	B��B��B�B�B�B��B	B	DB	PB	VB	oB	uB	�B	�B	uB	�B	�B	�B	�B	�B	�B	�B	&�B	'�B	%�B	#�B	)�B	0!B	>wB	_;B	\)B	ZB	bNB	hsB	m�B	t�B	u�B	t�B	s�B	r�B	p�B	l�B	q�B	v�B	u�B	r�B	p�B	p�B	r�B	y�B	}�B	~�B	|�B	~�B	~�B	}�B	~�B	�B	�B	�B	�+B	�7B	�DB	�DB	�DB	�DB	�JB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�XB	�FB	�-B	�'B	�3B	�^B	�wB	�qB	�qB	�qB	�wB	��B	��B	�}B	�jB	�qB	��B	��B	��B	B	ĜB	ǮB	ƨB	ƨB	ŢB	ĜB	ĜB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�)B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�fB	�ZB	�ZB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�sB	�sB	�sB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B
  B
B
B
PB
bB
�B
�B
 �B
)�B
1'B
9XB
>wB
B�B
M�B
R�B
XB
\)B
aHB
e`B
k�B
p�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B$�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B"�B"�B"�B"�B"�B!�B�B�B3BRB �B"�B*�B.�BN�B�B��B��B��B�+B�B��B��B�BpB�BwB*�BgMBjdBkfB��B�B�'B��B��B��B˥B̭BɖB��B��BǍB��B��BljBLB�(B�3B��B��B��B��B��Bz�BI�B(�B�B
�3B
ͰB
�tB
��B
{�B
R�B
)�B
nB
B	�IB	�IB	�mB	�)B	y�B	fKB	K�B	?cB	+�B	JB��B�B�\B�IB�2B�'B�B��B;BȟBōBȞBȠBŋB�sB�lB�mB�qB�B�~B�iB�gB�YB�ZB�JBąBɢBοBȚB͸B��B��B��B��BνB̳BͷBͻB̳BοB��B��B��B��B��B��B��B��B��B�-B�4B�9B�>B�DB�CB�PB�WB�HB�WB�B	 �B	�B	�B	 �B	�B��B��B�B�B�B��B	�B	&B	0B	7B	NB	TB	bB	hB	UB	`B	�B	oB	kB	hB	eB	�B	&�B	'�B	%�B	#�B	)�B	/�B	>UB	_B	\B	Y�B	b*B	hQB	mnB	t�B	u�B	t�B	s�B	r�B	p�B	lhB	q�B	v�B	u�B	r�B	p�B	p�B	r�B	y�B	}�B	~�B	|�B	~�B	~�B	}�B	~�B	��B	��B	��B	�B	�B	� B	� B	� B	� B	�%B	�6B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�B	�B	�B	�B	�7B	�PB	�HB	�IB	�HB	�OB	�\B	�\B	�TB	�BB	�IB	�_B	�_B	�cB	�jB	�vB	ǆB	ƀB	ƀB	�{B	�tB	�tB	�yB	ƁB	ȋB	ɒB	̤B	βB	δB	γB	αB	βB	ͩB	̥B	̣B	ͫB	мB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	� B	��B	��B	� B	��B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�%B	�$B	�,B	�-B	�,B	�,B	�1B	�<B	�;B	�=B	�<B	�=B	�<B	�CB	�JB	�KB	�KB	�KB	�JB	�NB	�OB	�UB	�VB	�UB	�VB	�VB	�JB	�KB	�KB	�IB	�LB	�PB	�VB	�]B	�^B	�YB	�\B	�\B	�\B	�SB	�QB	�IB	�IB	�<B	�1B	�/B	�)B	�)B	�(B	�*B	�0B	�1B	�1B	�2B	�1B	�/B	�0B	�7B	�JB	�JB	�KB	�CB	�CB	�JB	�IB	�MB	�JB	�HB	�KB	�JB	�KB	�JB	�JB	�JB	�KB	�CB	�DB	�BB	�DB	�CB	�CB	�CB	�BB	�CB	�IB	�JB	�PB	�NB	�TB	�fB	�mB	�nB	�mB	�mB	�nB	�nB	�rB	�vB	�tB	�tB	�zB	�{B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��G�O�B
�B
%B
6B
tB
�B
 �B
)�B
0�B
9,B
>LB
BdB
M�B
R�B
W�B
[�B
aB
e4B
kYB
puB
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451112016080714511120160807145111  AO  ARCAADJP                                                                    20150226221414    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221414  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221414  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145111  IP                  G�O�G�O�G�O�                