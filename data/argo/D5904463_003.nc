CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:46Z AOML 3.0 creation; 2016-08-07T22:44:57Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221446  20160807154458  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_003                   2C  D   APEX                            6530                            072314                          846 @�񀑿�1   @����@*      �cr�-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B:ffB>��BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D�fD�6fD�vfD�ٚD�3D�0 D�p D��fD� D�S3D���D��3D��D�6fDڙ�D��3D�3D�L�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�(�A{A&{AF{Af{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B	�B�B�B!�B)�B1�B;�B@Q�BI�BQ�BY�Ba�Bi�Bq�By�B�B�B�B�B���B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C aHCaHCaHCaHCaHC
aHCaHCaHCaHCz�Cz�CaHCaHCaHCaHCaHC aHC"aHC$aHC&aHC(aHC*aHC,aHC.aHC0aHC2aHC4aHC6aHC8aHC:aHC<aHC>aHC@aHCBaHCDaHCFaHCHaHCJaHCLaHCNaHCPaHCRaHCTaHCVaHCXaHCZaHC\aHC^aHC`aHCbaHCdaHCfaHChaHCjaHClaHCnaHCpaHCraHCtaHCvaHCxaHCzaHC|aHC~aHC�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�C�0�D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDt޸Dy��D�"�D�B�D���D���D�\D�<)D�|)D�D�)D�_\D���D��\D��D�B�Dڥ�D��\D�\D�X�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aߡ�Aߡ�Aߧ�Aߩ�A߬A߮A߬A߬A߮A߮A߮A߲-Aߴ9A߬Aߥ�Aߥ�Aߥ�Aߩ�A߬A߮A߬A�bAڮA�XA�|�A�G�A�Q�A�A�A�A��A���A���A�A�A�VA��A��FA�z�A�ffA�=qA�K�A��HA�z�A���A��A��A�&�A�A�t�A�ffA��A��yA��A��DA�%A�5?A��A���A�z�A�ĜA�l�A���A�M�A��^A���A��A��Au;dAlbAh��Ad��AbbNA`jA]oAY�#AX(�AU�#AQ�PAN�AKx�AG+A?�A>  A=�A;A9�hA9/A8ȴA8��A8v�A8VA7A6ȴA6�jA6=qA6��A7O�A7`BA6��A5��A5O�A2  A/�PA/
=A/�A0�A0�A/7LA-��A+��A* �A)�7A(�!A(�+A(r�A(��A(bNA'�;A&��A%VA%G�A%t�A%�7A%VA$��A$��A$��A$^5A$M�A"�A ��A ~�A z�A =qA�;A�hAȴA^5Az�AƨA�AZA�wA��Ax�A;dAĜA�A�AA��A�jA��A��A�A��A��A��A~�A=qA�mA�A��A��A5?A��A|�A�A^5A�TAA�hAS�AoA�HA�jA��An�AI�AA�A9XA�A�A��A��A~�Av�A�A��A�RA��A�\AI�A1A�;A�FA|�AXA%AĜAv�A^5A9XAAt�AVA�/A�A��A�!AVA�A�FA"�A
��A
A�A	��A	XA	"�A	A��A�mAG�AAĜA5?A�TA�^A�hA`BAVA=qA�A�^A��AbA��Ax�A%A�RA�A9XA��AoA n�@���@��y@��\@�$�@���@��j@�A�@�  @��F@�o@��\@�M�@�O�@���@�V@��@��@��D@��m@�|�@�+@��@�=q@�`B@��j@�A�@��m@�|�@��@�^5@���@�hs@�Ĝ@�(�@�w@��H@@�~�@�E�@��@홚@�%@� �@�ƨ@�l�@�ȴ@��@�$�@�@�@�b@���@�\)@�~�@�V@�x�@�A�@�9X@�9X@���@�"�@�R@�~�@�5?@���@�Ĝ@��
@�\)@���@��@���@ۅ@�K�@ڰ!@���@ّh@�9X@�\)@�@և+@���@�Ĝ@�j@��@ҸR@ѩ�@���@Ѓ@�l�@Χ�@�E�@�@�r�@��;@�t�@�o@ʟ�@��@Ɂ@��@�(�@Ǯ@�o@���@ŉ7@�%@�z�@�b@Õ�@��@���@���@���@�p�@��`@�Ĝ@���@�z�@�(�@��F@�C�@��R@�v�@�5?@��^@�p�@�G�@��`@�Z@�(�@��@�@���@���@��@��@���@��@��@�Ĝ@��D@�bN@� �@���@�C�@��y@���@�~�@�v�@�v�@�M�@���@�p�@���@� �@��;@�ƨ@���@���@�dZ@�o@��R@��@���@��@�z�@� �@�ƨ@�l�@�;d@�
=@�ȴ@���@�{@��7@��@��/@�Ĝ@�Ĝ@��9@��@�1@�dZ@��@�ff@��@��h@���@���@�z�@�Q�@�(�@�1@��F@��@�33@���@���@�@�?}@��@�9X@���@�+@�"�@�
=@��R@�=q@���@�x�@�/@�%@���@�r�@��@��@�V@�=q@�J@�?}@��/@�Ĝ@�9X@��@�"�@���@��+@�ff@�E�@�$�@��#@�&�@���@��@�1'@���@�dZ@��y@�^5@��T@�`B@�?}@��@�j@���@�@���@��h@�V@x��@pQ�@iG�@aX@W��@PĜ@I%@A��@<Z@5�@,�/@'�P@"�H@��@�y@�!@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aߡ�Aߡ�Aߧ�Aߩ�A߬A߮A߬A߬A߮A߮A߮A߲-Aߴ9A߬Aߥ�Aߥ�Aߥ�Aߩ�A߬A߮A߬A�bAڮA�XA�|�A�G�A�Q�A�A�A�A��A���A���A�A�A�VA��A��FA�z�A�ffA�=qA�K�A��HA�z�A���A��A��A�&�A�A�t�A�ffA��A��yA��A��DA�%A�5?A��A���A�z�A�ĜA�l�A���A�M�A��^A���A��A��Au;dAlbAh��Ad��AbbNA`jA]oAY�#AX(�AU�#AQ�PAN�AKx�AG+A?�A>  A=�A;A9�hA9/A8ȴA8��A8v�A8VA7A6ȴA6�jA6=qA6��A7O�A7`BA6��A5��A5O�A2  A/�PA/
=A/�A0�A0�A/7LA-��A+��A* �A)�7A(�!A(�+A(r�A(��A(bNA'�;A&��A%VA%G�A%t�A%�7A%VA$��A$��A$��A$^5A$M�A"�A ��A ~�A z�A =qA�;A�hAȴA^5Az�AƨA�AZA�wA��Ax�A;dAĜA�A�AA��A�jA��A��A�A��A��A��A~�A=qA�mA�A��A��A5?A��A|�A�A^5A�TAA�hAS�AoA�HA�jA��An�AI�AA�A9XA�A�A��A��A~�Av�A�A��A�RA��A�\AI�A1A�;A�FA|�AXA%AĜAv�A^5A9XAAt�AVA�/A�A��A�!AVA�A�FA"�A
��A
A�A	��A	XA	"�A	A��A�mAG�AAĜA5?A�TA�^A�hA`BAVA=qA�A�^A��AbA��Ax�A%A�RA�A9XA��AoA n�@���@��y@��\@�$�@���@��j@�A�@�  @��F@�o@��\@�M�@�O�@���@�V@��@��@��D@��m@�|�@�+@��@�=q@�`B@��j@�A�@��m@�|�@��@�^5@���@�hs@�Ĝ@�(�@�w@��H@@�~�@�E�@��@홚@�%@� �@�ƨ@�l�@�ȴ@��@�$�@�@�@�b@���@�\)@�~�@�V@�x�@�A�@�9X@�9X@���@�"�@�R@�~�@�5?@���@�Ĝ@��
@�\)@���@��@���@ۅ@�K�@ڰ!@���@ّh@�9X@�\)@�@և+@���@�Ĝ@�j@��@ҸR@ѩ�@���@Ѓ@�l�@Χ�@�E�@�@�r�@��;@�t�@�o@ʟ�@��@Ɂ@��@�(�@Ǯ@�o@���@ŉ7@�%@�z�@�b@Õ�@��@���@���@���@�p�@��`@�Ĝ@���@�z�@�(�@��F@�C�@��R@�v�@�5?@��^@�p�@�G�@��`@�Z@�(�@��@�@���@���@��@��@���@��@��@�Ĝ@��D@�bN@� �@���@�C�@��y@���@�~�@�v�@�v�@�M�@���@�p�@���@� �@��;@�ƨ@���@���@�dZ@�o@��R@��@���@��@�z�@� �@�ƨ@�l�@�;d@�
=@�ȴ@���@�{@��7@��@��/@�Ĝ@�Ĝ@��9@��@�1@�dZ@��@�ff@��@��h@���@���@�z�@�Q�@�(�@�1@��F@��@�33@���@���@�@�?}@��@�9X@���@�+@�"�@�
=@��R@�=q@���@�x�@�/@�%@���@�r�@��@��@�V@�=q@�J@�?}@��/@�Ĝ@�9X@��@�"�@���@��+@�ff@�E�@�$�@��#@�&�@���@��@�1'@���@�dZ@��y@�^5@��T@�`B@�?}@��@�jG�O�@�@���@��h@�V@x��@pQ�@iG�@aX@W��@PĜ@I%@A��@<Z@5�@,�/@'�P@"�H@��@�y@�!@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�}B
��B�B�9B��B�)B�BH�BM�Bs�B��B�oB�B�7B�oB�JB�oB��B�hB|�BdZB]/BB�B:^BF�B=qBBB+B�NB�B�7BhsBYB;dB{B
�B
�BB
ƨB
�uB
jB
W
B
M�B
;dB	�B	�hB	M�B	;dB	1'B	-B	0!B	.B	9XB	A�B	F�B	?}B	6FB	0!B	�B��B	B		7B	�B	"�B	)�B	6FB	;dB	O�B	gmB	�\B	��B	�FB	ǮB	�5B
VB
�B
)�B
=qB
;dB
)�B
)�B
+B
M�B
^5B
_;B
[#B
S�B
C�B
8RB
9XB
;dB
D�B
L�B
YB
]/B
\)B
S�B
O�B
[#B
aHB
cTB
aHB
`BB
cTB
dZB
dZB
n�B
gmB
T�B
N�B
O�B
VB
W
B
YB
S�B
XB
Q�B
O�B
I�B
J�B
G�B
K�B
O�B
S�B
Q�B
N�B
K�B
M�B
N�B
O�B
P�B
W
B
`BB
ffB
iyB
iyB
gmB
ffB
cTB
^5B
\)B
ZB
W
B
W
B
S�B
P�B
N�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
VB
VB
VB
W
B
YB
aHB
p�B
m�B
l�B
l�B
n�B
p�B
r�B
v�B
w�B
w�B
w�B
v�B
u�B
u�B
u�B
t�B
t�B
s�B
r�B
q�B
p�B
o�B
n�B
l�B
l�B
l�B
k�B
jB
hsB
ffB
e`B
aHB
\)B
YB
XB
VB
T�B
S�B
Q�B
N�B
K�B
J�B
I�B
H�B
G�B
F�B
F�B
E�B
D�B
C�B
C�B
E�B
I�B
K�B
J�B
I�B
G�B
E�B
E�B
D�B
C�B
A�B
@�B
=qB
<jB
;dB
:^B
9XB
7LB
6FB
6FB
7LB
7LB
6FB
6FB
5?B
6FB
6FB
6FB
6FB
6FB
5?B
5?B
5?B
49B
49B
33B
33B
2-B
1'B
1'B
0!B
/B
/B
.B
.B
.B
-B
-B
-B
-B
,B
,B
+B
)�B
)�B
)�B
(�B
'�B
'�B
'�B
&�B
#�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
{B
{B
{B
{B
{B
{B
uB
bB
VB
PB
PB
PB
PB
JB
DB
JB
JB
DB
DB
DB
DB
DB

=B
DB
DB

=B
DB
DB
DB
DB
DB
DB
JB
DB

=B
DB
JB
PB
PB
PB
VB
\B
VB
VB
\B
VB
VB
VB
VB
VB
VB
PB
JB
PB
PB
VB
VB
VB
VB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
hB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
oB
oB
oB
oB
oB
uB
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
/B
$�B
-B
2-B
7LB
=qB
B�B
G�B
K�B
P�B
T�B
YB
]/B
aHB
e`B
jB
m�B
q�B
t�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
��B
��B
��B
��B
��B
��B
��B
��B
�cB
̳B��B�B˦B�B�qBH�BM�Bs�B�tB�LB��B�B�OB�,B�PB�mB�DB|�Bd7B]BBjB:;BF�B=QB�B �BB�)B��B�BhQBX�B;CBVB
�B
� B
ƅB
�UB
j_B
V�B
M�B
;GB	�B	�NB	M�B	;JB	1B	,�B	0	B	-�B	9>B	AoB	F�B	?eB	6)B	0B	�B��B	B		B	fB	"�B	)�B	6)B	;GB	O�B	gLB	�=B	��B	�$B	ǎB	�B
/B
iB
)�B
=KB
;>B
)�B
)�B
*�B
M�B
^B
_B
Z�B
S�B
CoB
8+B
91B
;=B
DtB
L�B
X�B
]
B
\B
S�B
O�B
Z�B
aB
c.B
a!B
`B
c,B
d2B
d1B
npB
gCB
T�B
N�B
O�B
U�B
V�B
X�B
S�B
W�B
Q�B
O�B
I�B
J�B
G�B
K�B
O�B
S�B
Q�B
N�B
K�B
M�B
N�B
O�B
P�B
V�B
`B
f?B
iQB
iRB
gCB
f=B
c,B
^B
\B
Y�B
V�B
V�B
S�B
P�B
N�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
U�B
U�B
U�B
V�B
X�B
a!B
p{B
mgB
lcB
laB
nlB
p|B
r�B
v�B
w�B
w�B
w�B
v�B
u�B
u�B
u�B
t�B
t�B
s�B
r�B
q�B
pyB
ouB
noB
lcB
l`B
l`B
k]B
jUB
hKB
f=B
e6B
aB
[�B
X�B
W�B
U�B
T�B
S�B
Q�B
N�B
K�B
J�B
I�B
H�B
G�B
F~B
F}B
ExB
DqB
CpB
CmB
E{B
I�B
K�B
J�B
I�B
G�B
ExB
EzB
DtB
CmB
A`B
@YB
=HB
<@B
;;B
:5B
9/B
7!B
6B
6B
7$B
7#B
6B
6B
5B
6B
6B
6B
6B
6B
5B
5B
5B
4B
4B
3
B
3B
2B
0�B
0�B
/�B
.�B
.�B
-�B
-�B
-�B
,�B
,�B
,�B
,�B
+�B
+�B
*�B
)�B
)�B
)�B
(�B
'�B
'�B
'�B
&�B
#�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
}B
wB
wB
uB
uB
pB
iB
dB
cB
^B
WB
VB
VB
PB
WB
YB
XB
VB
QB
RB
QB
SB
QB
PB
LB
8B
,B
'B
%B
&B
%B
"B
B
!B
!B
B
B
B
B
B

B
B
B

B
B
B
B
B
B
B
!B
B

B
B
 B
#B
'B
'B
+B
3B
,B
-B
1B
.B
,B
,B
+B
,B
-B
'B
 B
%B
'B
+B
,B
,B
+B
%B
%B
+B
,B
*B
-B
,B
-B
2B
2B
1B
7B
1B
1B
4B
2B
3B
9B
9B
>B
>B
>B
@B
7B
;B
9B
<B
?B
>B
>B
=B
EB
AB
EB
BB
CB
DB
DB
LB
JB
JB
JB
OB
JB
MB
DB
DB
EB
CB
BB
JB
QB
UB
OB
VB
YB
TB
]B
ZB
[B
\B
cB
cB
ZB
bB
iB
hB
oB
nB
nB
nB
tB
uB
uB
{B
}B
�B
xB
yB
uB
zB
vB
tB
yB
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�G�O�B
$�B
,�B
2B
7B
=EB
B`B
G�B
K�B
P�B
T�B
X�B
]B
aB
e3B
jRB
mcB
q|B
t�B
z�B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.38 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544582016080715445820160807154458  AO  ARCAADJP                                                                    20150226221446    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221446  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221446  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154458  IP                  G�O�G�O�G�O�                