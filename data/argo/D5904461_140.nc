CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125828  20190408133246  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @�ə�,�p1   @�ɚ��K�@4�n��O��b�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�  D�@ D�vfD���D��D�S3D�� D��3D�	�D�@ D�� D���D�3D�9�Dڐ D��3D�3D�P D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�u�B�u�B��)B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx:�Cz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDtθDy��D�$)D�D)D�z�D���D��D�W\D��)D��\D��D�D)D��)D���D�\D�=�Dڔ)D��\D�\D�T)D�}�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA�ĜA�ƨA�ȴA���A���A���A��
A��
A��
A��
A��
A��A��A���A���A���A���A���A�ĜAΣ�A�x�A�XA�n�A�9XA�\)A�oA̼jẢ7A��A��;A�9XA��`A��TA���AĮA���A�bA��wA�^5A��A�%A�1A���A�ƨA��^A�A�A�A�ffA�dZA��-A�VA��A���A�v�A�+A�jA�A��A���A�bA�ĜA�
=A�r�A�z�A��uA�jA���A�O�A�O�A�hsA��A�=qA��A��7A�bA���A��PA��A��TA��`A��A�A�O�A��A��PA�oA�5?A��A��#A��+A�+A� �A�~�A��A�A��A�VA�A�hsA�K�A�XA��uA���A���A��A�/A��A�z�A��mA��A���A�`BA�
A~�A|bAy�Av-At�Ao�AnĜAk�Ai\)AgXAdM�Ab�A`VA]+AY��AW�AU`BARM�AQƨAP�RAN�`AM�AK;dAI\)AG�FAGhsAES�AC&�A@-A=��A<��A;t�A9�#A7%A5XA4�A4JA2A�A0��A.�jA.$�A-33A,A*^5A(�yA(bNA'�TA'O�A&ȴA&JA%�A$JA"=qA!/A E�A�A(�A/A��A��AG�Ar�A��A��A�Al�A��A(�A�A=qA��A��A�A�A�A�!A�mA�+AC�A
A�A	t�A��A�A�A~�A�hAȴA�A\)A33A �A �9A ffA bNA ��A j@��;@�@�%@��@�~�@���@��@��P@��H@���@�O�@�Ĝ@�@��`@�7@�+@�ȴ@�ƨ@�?}@��@땁@���@�&�@�A�@�w@�ȴ@���@���@�j@���@�l�@�R@�=q@�-@�@��/@��;@߅@�dZ@��H@��@�X@ܬ@���@��;@ش9@١�@�z�@��@ԓu@�I�@��@ӥ�@Ұ!@�?}@��@ϕ�@��@θR@�v�@͙�@�G�@�7L@���@�@�V@�1'@�I�@�@�O�@�bN@ź^@ă@�%@�S�@��@��`@�%@�/@�bN@��!@�n�@�M�@���@��h@���@��-@���@�x�@��u@�Z@�Q�@�b@��@��F@�|�@�l�@��@�ff@�V@��@���@�?}@���@�z�@�ƨ@���@��
@�t�@�|�@�C�@��@��!@�~�@��@���@��@�p�@�X@�G�@�/@��@���@��u@�Q�@��@��F@�dZ@�+@���@���@���@�n�@�$�@��@���@���@�hs@�V@��/@���@��@�j@�  @��w@���@�|�@�l�@�C�@�
=@���@�v�@�5?@���@���@�O�@���@���@��u@��@�bN@�(�@�1@��@��
@���@�\)@��@�5?@�hs@��`@���@���@��@���@���@��@���@�{@���@��@�V@�Ĝ@�Q�@���@��@�t�@�^5@��@�@�?}@��@�r�@� �@�t�@�-@��^@��7@�G�@���@�j@���@���@��9@��@�1@�S�@��@��@��+@��+@���@�^5@�-@�{@��^@�hs@�&�@���@�Z@�I�@�I�@�Z@��u@���@��u@�r�@�(�@��@���@��y@���@��\@�E�@���@��7@�hs@�G�@��@���@��j@��9@���@��D@�r�@�I�@��@��@���@���@�\)@�o@��y@��R@��+@�M�@�=q@�{@�J@���@��#@�@��h@�G�@��@���@��9@�j@��@���@�33@��@���@���@���@�n�@�5?@���@�x�@�Ĝ@�S�@|�@t��@j��@a��@Y��@SdZ@L�@Fȴ@?;d@8 �@17L@,z�@'K�@ ��@@�`@�@A�@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ĜA�ĜA�ƨA�ȴA���A���A���A��
A��
A��
A��
A��
A��A��A���A���A���A���A���A�ĜAΣ�A�x�A�XA�n�A�9XA�\)A�oA̼jẢ7A��A��;A�9XA��`A��TA���AĮA���A�bA��wA�^5A��A�%A�1A���A�ƨA��^A�A�A�A�ffA�dZA��-A�VA��A���A�v�A�+A�jA�A��A���A�bA�ĜA�
=A�r�A�z�A��uA�jA���A�O�A�O�A�hsA��A�=qA��A��7A�bA���A��PA��A��TA��`A��A�A�O�A��A��PA�oA�5?A��A��#A��+A�+A� �A�~�A��A�A��A�VA�A�hsA�K�A�XA��uA���A���A��A�/A��A�z�A��mA��A���A�`BA�
A~�A|bAy�Av-At�Ao�AnĜAk�Ai\)AgXAdM�Ab�A`VA]+AY��AW�AU`BARM�AQƨAP�RAN�`AM�AK;dAI\)AG�FAGhsAES�AC&�A@-A=��A<��A;t�A9�#A7%A5XA4�A4JA2A�A0��A.�jA.$�A-33A,A*^5A(�yA(bNA'�TA'O�A&ȴA&JA%�A$JA"=qA!/A E�A�A(�A/A��A��AG�Ar�A��A��A�Al�A��A(�A�A=qA��A��A�A�A�A�!A�mA�+AC�A
A�A	t�A��A�A�A~�A�hAȴA�A\)A33A �A �9A ffA bNA ��A j@��;@�@�%@��@�~�@���@��@��P@��H@���@�O�@�Ĝ@�@��`@�7@�+@�ȴ@�ƨ@�?}@��@땁@���@�&�@�A�@�w@�ȴ@���@���@�j@���@�l�@�R@�=q@�-@�@��/@��;@߅@�dZ@��H@��@�X@ܬ@���@��;@ش9@١�@�z�@��@ԓu@�I�@��@ӥ�@Ұ!@�?}@��@ϕ�@��@θR@�v�@͙�@�G�@�7L@���@�@�V@�1'@�I�@�@�O�@�bN@ź^@ă@�%@�S�@��@��`@�%@�/@�bN@��!@�n�@�M�@���@��h@���@��-@���@�x�@��u@�Z@�Q�@�b@��@��F@�|�@�l�@��@�ff@�V@��@���@�?}@���@�z�@�ƨ@���@��
@�t�@�|�@�C�@��@��!@�~�@��@���@��@�p�@�X@�G�@�/@��@���@��u@�Q�@��@��F@�dZ@�+@���@���@���@�n�@�$�@��@���@���@�hs@�V@��/@���@��@�j@�  @��w@���@�|�@�l�@�C�@�
=@���@�v�@�5?@���@���@�O�@���@���@��u@��@�bN@�(�@�1@��@��
@���@�\)@��@�5?@�hs@��`@���@���@��@���@���@��@���@�{@���@��@�V@�Ĝ@�Q�@���@��@�t�@�^5@��@�@�?}@��@�r�@� �@�t�@�-@��^@��7@�G�@���@�j@���@���@��9@��@�1@�S�@��@��@��+@��+@���@�^5@�-@�{@��^@�hs@�&�@���@�Z@�I�@�I�@�Z@��u@���@��u@�r�@�(�@��@���@��y@���@��\@�E�@���@��7@�hs@�G�@��@���@��j@��9@���@��D@�r�@�I�@��@��@���@���@�\)@�o@��y@��R@��+@�M�@�=q@�{@�J@���@��#@�@��h@�G�@��@���@��9@�j@��@���@�33@��@���@���@���@�n�@�5?@���G�O�@�Ĝ@�S�@|�@t��@j��@a��@Y��@SdZ@L�@Fȴ@?;d@8 �@17L@,z�@'K�@ ��@@�`@�@A�@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�B
�B
��B	7B2-B^5Be`Bm�Bq�B{�B� B�hB�3BƨB�ZB:^BYBhsBl�Br�Bw�Bw�Bv�Bz�B��BŢB��B��B�yB�B�B�yB�mB�mB�fB�TB�;B��B�XB�=BB�B$�B�B#�B-B9XB0!B!�B)�B0!B7LBgmB}�Bx�Bp�BiyBcTB+B�TB�B�#B��B��B��B��B�+B�B�7B� B~�Bz�Bt�BgmB`BBS�BG�B=qB0!BuB
��B
�B
��B
ÖB
�FB
�B
��B
}�B
m�B
]/B
W
B
J�B
49B
!�B
�B
DB	��B	�ZB	��B	�wB	��B	��B	�+B	t�B	dZB	P�B	>wB	1'B	�B	+B	B��B�B�sB�TB�B��BȴB��B�qB�^B�9B��B��B�uB�\B�7B� Bu�Bp�Br�Bt�Bo�Bm�Bm�BjBgmBe`BcTBdZBe`BjBk�Bk�BhsBffBgmBdZBaHB_;B\)BYB\)B^5BaHBq�Br�Bn�BbNBgmBm�BdZB^5BaHBZBZBVBS�BVBT�BP�BN�BR�BVBT�BR�BQ�BO�BO�BN�BK�BH�BG�BG�BH�BM�BR�BR�BT�BdZBw�Bz�Bz�B|�B{�B}�B�B�B�B�B�B�B� B|�Bv�Bt�Bu�B~�B�hB��B��B��B��B��B��B��B��B�B�3B�9B�?B�FB�RB�XB�jB�jB�wBBÖBÖBĜBƨBƨBƨBɺB��B��B��B��B��B��B��B�B�/B�5B�5B�/B�;B�BB�TB�sB�B�B�B�B�B�B�B��B��B��B��B	bB	\B	1B	+B	%B	%B		7B	JB	DB	1B	
=B	DB	hB	�B	�B	�B	�B	 �B	'�B	0!B	2-B	6FB	7LB	8RB	9XB	9XB	;dB	<jB	<jB	=qB	=qB	=qB	=qB	@�B	C�B	E�B	J�B	N�B	T�B	W
B	XB	\)B	_;B	aHB	dZB	e`B	ffB	gmB	gmB	hsB	k�B	k�B	m�B	o�B	p�B	q�B	s�B	u�B	v�B	x�B	y�B	z�B	{�B	}�B	~�B	� B	�B	�B	�%B	�%B	�+B	�7B	�JB	�PB	�VB	�\B	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�?B	�FB	�RB	�^B	�dB	�dB	�dB	�qB	�}B	��B	��B	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
%B

=B
uB
�B
'�B
/B
5?B
9XB
=qB
B�B
J�B
Q�B
VB
\)B
aHB
dZB
hsB
l�B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
�B
�B
�B
�B
��B	/B2!B^+BeVBm�Bq�B{�B�B�[B�*BƛB�QB:TBYBhgBl�Br�Bw�Bw�Bv�Bz�B��BŖB��B��B�qB�uB�yB�kB�cB�bB�YB�LB�/B��B�KB�1BB�B$�B�B#�B-B9KB0B!�B)�B0B7@BgdB}�Bx�Bp�BinBcGB*�B�FB�B�B��B��B��B�{B�B�B�,B�B~�Bz�Bt�Bg`B`6BS�BG�B=eB0BhB
��B
�B
��B
ÊB
�;B
� B
�sB
}�B
m�B
]"B
V�B
J�B
4.B
!�B
B
9B	��B	�LB	��B	�hB	��B	�B	�B	t�B	dMB	P�B	>iB	1B	�B	B	 �B��B�uB�dB�FB�B��BȥB�zB�bB�PB�,B��B��B�fB�MB�'B�Bu�Bp�Br�Bt�Bo�Bm�Bm�BjpBg]BeSBcFBdLBeSBjrBkvBkwBhdBfXBg^BdKBa8B_,B\BY	B\B^'Ba9Bq�Br�Bn�Bb=Bg\Bm�BdMB^%Ba:BZBZBU�BS�BU�BT�BP�BN�BR�BU�BT�BR�BQ�BO�BO�BN�BK�BH�BG�BG�BH�BM�BR�BR�BT�BdKBw�Bz�Bz�B|�B{�B}�B��B��B��B��B��B��B�B|�Bv�Bt�Bu�B~�B�[B��B��B��B��B��B��B��B��B�
B�#B�'B�.B�7B�AB�FB�YB�WB�fB�}BÅBÈBčBƙBƘBƙBɩB��B˷B��B��B��B��B��B�B�B�#B�$B�B�*B�/B�CB�dB�yB�xB�B�B�B�B�B��B��B��B��B	PB	MB	 B	B	B	B		&B	9B	2B	#B	
/B	2B	UB	�B	�B	�B	�B	 �B	'�B	0B	2B	67B	7=B	8CB	9FB	9FB	;TB	<[B	<ZB	=`B	=aB	=_B	=\B	@rB	C�B	E�B	J�B	N�B	T�B	V�B	XB	\B	_(B	a7B	dHB	eQB	fTB	g[B	g[B	hbB	ktB	kvB	m�B	o�B	p�B	q�B	s�B	u�B	v�B	x�B	y�B	z�B	{�B	}�B	~�B	�B	��B	�B	�B	�B	�B	�&B	�;B	�?B	�FB	�JB	�LB	�OB	�YB	�`B	�rB	�xB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�
B	�B	�B	�B	�-B	�.B	�7B	�AB	�NB	�SB	�RB	�RB	�bB	�kB	�oB	�wB	�B	ŒB	ʱB	˴B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�*B	�1B	�=B	�=B	�CB	�CB	�HB	�HB	�GB	�HB	�NB	�MB	�VB	�ZB	�[B	�dB	�kB	�mB	�uB	�yB	�B	�~B	�B	�B	�B	�B	�G�O�B	��B
B

,B
bB
�B
'�B
/	B
5/B
9FB
=^B
BB
J�B
Q�B
U�B
\B
a6B
dHB
h`B
l{B
p�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332462019040813324620190408133246  AO  ARCAADJP                                                                    20181121125828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125828  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133246  IP                  G�O�G�O�G�O�                