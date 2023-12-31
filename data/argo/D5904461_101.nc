CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-16T20:20:52Z AOML 3.0 creation; 2016-08-07T21:36:43Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160216202052  20160807143644  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               eA   AO  5286_8897_101                   2C  D   APEX                            6531                            072314                          846 @ז0��E1   @ז1^�@2��Q��cI���o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    eA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�S3D���D�ٚD���D�9�D���D���D�3D�FfD���D�ɚD�fD�C3D�i�D��fD�3D�@ D�y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx�Cz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP��DQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt��Dy�RD��D�W\D���D���D� �D�=�D���D���D�\D�J�D���D���D�
�D�G\D�m�D�ʏD�\D�D)D�}�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AƃAƋDAƋDAƇ+AƅAƇ+AƋDAƓuAƕ�AƗ�AƗ�Aƙ�Aƙ�Aƙ�Aƛ�Aƛ�Aƙ�Aƛ�AƝ�AƟ�AƟ�Aơ�Aơ�Aƥ�Aƥ�Aƣ�Aƣ�Aƣ�Aƣ�Aƣ�Aơ�AƧ�AƧ�AƧ�AƩ�AƩ�AƬA��mA�oA���A�|�A�A�&�A�5?A�A�9XA�9XA���A��TA���A�Q�A���A��9A�`BA���A�JA�C�A���A�-A��+A�  A��#A�M�A�&�A��A�-A�XA���A���A���A��\A�~�A�\)A�ĜA�33A���A�?}A��A��A~��A}hsA{�Ax5?Au��As�mArbAn��Ak&�AiƨAihsAg��Ab�A`Q�A^r�A]ƨA[`BAV�+AT=qAR{AO�PAM�AK��AJbAHACA?t�A>�9A;��A:$�A933A7�mA6�A5��A3p�A0�A/�A.JA-�^A,�!A*��A*bA)�A)%A(�A($�A&�jA%�#A$�uA#��A"��A"M�A"9XA!�;A!/A��A&�AoA�HA��A�hA&�A
=A
=A�A��A�yA1'A�PA
=A��AhsA/Ap�AAE�A��A��AO�AjA��A�wAhsA+A
=A�`An�A�PA
=A
{A	A	�A	33A��A~�A�A|�AG�A��A��At�A ��@�ƨ@��^@�n�@� �@��@�~�@���@�o@��
@��H@��T@�hs@�R@�$�@��-@�9@�w@��@���@��@��@��`@�w@� �@ݙ�@�C�@�^5@��@�G�@��@���@��/@أ�@�9X@�  @׮@֧�@�v�@�E�@պ^@���@ԃ@Ӯ@�o@��y@��@�5?@��#@�%@�^5@�$�@��#@�`B@�V@�z�@��@�|�@ʧ�@��@��@ȴ9@��@�^5@�p�@�Z@��@�p�@��j@��@�n�@���@�C�@��\@�hs@��/@�Z@���@�K�@��\@��@�G�@���@���@�r�@�Q�@�b@��F@��P@�S�@�@�~�@�E�@��@�G�@�Ĝ@��9@���@��F@��H@��R@���@�^5@���@���@�/@��D@��P@�K�@�33@�o@���@���@�p�@��@�A�@���@���@�K�@�ȴ@��\@�n�@�$�@���@���@��^@���@��@�hs@�`B@�X@�O�@�?}@�/@���@���@��u@�l�@�E�@��@�@�x�@�%@��`@��9@��@���@��@�l�@�;d@�E�@�x�@�&�@��@�x�@�7L@���@���@� �@���@�o@��!@��+@�J@��T@���@�`B@�&�@���@�Ĝ@�r�@� �@��w@��@�S�@�"�@���@�@���@���@�hs@���@�1@�t�@�K�@�o@���@�ff@��T@��^@�@���@��T@���@��/@���@��D@�bN@�1'@��m@��P@�l�@�S�@�"�@�@�@��y@���@��\@��+@��+@�v�@�ff@�~�@�M�@��@��T@���@�p�@�hs@�`B@�`B@�hs@��@���@��@���@��-@�X@��/@�bN@���@�ƨ@��F@��P@�K�@�C�@�+@�@���@�$�@�@���@�p�@�X@�V@�Ĝ@�Ĝ@���@�bN@�1'@���@�|�@�C�@��@��@��!@�~�@�$�@��@��@��@��#@��^@��7@���@��@���@�z�@�Z@��m@�K�@�
=@��@�ȴ@���@�M�@�$�@�@���@��-@���@��7@�G�@���@���@��@���@��D@�Z@;d@~�R@~$�@}�T@}@}�-@}��@}��@}p�@}V@|j@|I�@|1@{�
@{�@{"�@z��@z^5@vȴ@nff@iG�@_K�@T��@N�y@F��@>v�@9��@2��@,�@%��@!��@V@&�@�j@bN@O�@	�7@��@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AƃAƋDAƋDAƇ+AƅAƇ+AƋDAƓuAƕ�AƗ�AƗ�Aƙ�Aƙ�Aƙ�Aƛ�Aƛ�Aƙ�Aƛ�AƝ�AƟ�AƟ�Aơ�Aơ�Aƥ�Aƥ�Aƣ�Aƣ�Aƣ�Aƣ�Aƣ�Aơ�AƧ�AƧ�AƧ�AƩ�AƩ�AƬA��mA�oA���A�|�A�A�&�A�5?A�A�9XA�9XA���A��TA���A�Q�A���A��9A�`BA���A�JA�C�A���A�-A��+A�  A��#A�M�A�&�A��A�-A�XA���A���A���A��\A�~�A�\)A�ĜA�33A���A�?}A��A��A~��A}hsA{�Ax5?Au��As�mArbAn��Ak&�AiƨAihsAg��Ab�A`Q�A^r�A]ƨA[`BAV�+AT=qAR{AO�PAM�AK��AJbAHACA?t�A>�9A;��A:$�A933A7�mA6�A5��A3p�A0�A/�A.JA-�^A,�!A*��A*bA)�A)%A(�A($�A&�jA%�#A$�uA#��A"��A"M�A"9XA!�;A!/A��A&�AoA�HA��A�hA&�A
=A
=A�A��A�yA1'A�PA
=A��AhsA/Ap�AAE�A��A��AO�AjA��A�wAhsA+A
=A�`An�A�PA
=A
{A	A	�A	33A��A~�A�A|�AG�A��A��At�A ��@�ƨ@��^@�n�@� �@��@�~�@���@�o@��
@��H@��T@�hs@�R@�$�@��-@�9@�w@��@���@��@��@��`@�w@� �@ݙ�@�C�@�^5@��@�G�@��@���@��/@أ�@�9X@�  @׮@֧�@�v�@�E�@պ^@���@ԃ@Ӯ@�o@��y@��@�5?@��#@�%@�^5@�$�@��#@�`B@�V@�z�@��@�|�@ʧ�@��@��@ȴ9@��@�^5@�p�@�Z@��@�p�@��j@��@�n�@���@�C�@��\@�hs@��/@�Z@���@�K�@��\@��@�G�@���@���@�r�@�Q�@�b@��F@��P@�S�@�@�~�@�E�@��@�G�@�Ĝ@��9@���@��F@��H@��R@���@�^5@���@���@�/@��D@��P@�K�@�33@�o@���@���@�p�@��@�A�@���@���@�K�@�ȴ@��\@�n�@�$�@���@���@��^@���@��@�hs@�`B@�X@�O�@�?}@�/@���@���@��u@�l�@�E�@��@�@�x�@�%@��`@��9@��@���@��@�l�@�;d@�E�@�x�@�&�@��@�x�@�7L@���@���@� �@���@�o@��!@��+@�J@��T@���@�`B@�&�@���@�Ĝ@�r�@� �@��w@��@�S�@�"�@���@�@���@���@�hs@���@�1@�t�@�K�@�o@���@�ff@��T@��^@�@���@��T@���@��/@���@��D@�bN@�1'@��m@��P@�l�@�S�@�"�@�@�@��y@���@��\@��+@��+@�v�@�ff@�~�@�M�@��@��T@���@�p�@�hs@�`B@�`B@�hs@��@���@��@���@��-@�X@��/@�bN@���@�ƨ@��F@��P@�K�@�C�@�+@�@���@�$�@�@���@�p�@�X@�V@�Ĝ@�Ĝ@���@�bN@�1'@���@�|�@�C�@��@��@��!@�~�@�$�@��@��@��@��#@��^@��7@���@��@���@�z�@�Z@��m@�K�@�
=@��@�ȴ@���@�M�@�$�@�@���@��-@���@��7@�G�@���@���@��@���@��D@�Z@;d@~�R@~$�@}�T@}@}�-@}��@}��@}p�@}V@|j@|I�@|1@{�
@{�@{"�@z��G�O�@vȴ@nff@iG�@_K�@T��@N�y@F��@>v�@9��@2��@,�@%��@!��@V@&�@�j@bN@O�@	�7@��@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	8RB	8RB	7LB	8RB	8RB	8RB	8RB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	8RB	8RB	8RB	8RB	8RB	8RB	8RB	9XB	8RB	8RB	8RB	8RB	;dB	�B	�RB	��B	�HB	��B
B
JB
49B
A�B
iyB
��B
ĜB
��B
�B
hsB
VB
I�B
>wB
.B
�B
\B
JB
1B
B	�B	�B	�ZB	�B	�BB	��B	�B	�B	�)B	��B	��B	��B	��B	��B	��B	��B	ŢB	�-B	��B	��B	�PB	z�B	jB	_;B	R�B	B�B	33B	,B	'�B	�B	
=B	B	B	B��B�B�TB�
BɺB�wB�RB�'B��B�uB�=B�%B�B}�B{�Bv�Bs�Bp�BjBe`Be`Be`BffBe`BdZBe`BhsBo�Bz�B�B�oB�uB��B��B�uB�hB�bB�VB�=B�B�B�B�B�B|�Bm�Bl�Bn�Bo�Bl�BffBdZBr�Bv�Bu�By�Bn�Bl�Bl�Br�Bu�Bw�By�B�7B�VB�hB��B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�bB�\B�hB�JB�+B�B~�B{�Bz�B}�B~�B|�Bx�Bs�Bt�Bq�Br�B�B�B�B�B�B�B}�Bx�Bt�Bs�Bq�Bm�BiyBffBk�Bo�Bq�Bt�By�B|�B~�B�B�B� B�B�%B�7B�=B�JB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�9B�^B��BBĜBȴB��B��B��B��B��B�B�B�/B�HB�TB�`B�fB�fB�fB�mB�mB�B�B�B�B��B��B��B��B	B	B	B	%B	
=B	DB	\B	uB	�B	�B	�B	�B	!�B	%�B	'�B	,B	33B	7LB	8RB	;dB	?}B	@�B	A�B	E�B	K�B	L�B	M�B	N�B	P�B	Q�B	Q�B	Q�B	R�B	R�B	T�B	VB	W
B	W
B	^5B	cTB	dZB	gmB	k�B	n�B	o�B	p�B	t�B	v�B	v�B	v�B	w�B	x�B	z�B	|�B	|�B	�B	�B	�B	�+B	�=B	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�-B	�?B	�?B	�9B	�9B	�?B	�?B	�RB	�^B	�jB	�qB	�wB	��B	B	ĜB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�;B	�BB	�BB	�BB	�HB	�TB	�ZB	�mB	�yB	�sB	�sB	�fB	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
VB
VB
\B
bB
oB
�B
�B
'�B
0!B
6FB
=qB
E�B
I�B
O�B
VB
[#B
^5B
dZB
gmB
l�B
p�B
s�B
w�B
z�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	8ZB	8XB	7RB	8WB	8VB	8WB	8XB	7RB	7SB	7SB	7QB	7RB	7RB	7RB	7QB	7RB	7RB	7OB	7RB	7RB	7QB	7RB	7PB	7QB	8YB	8YB	8YB	8YB	8YB	8YB	8YB	9]B	8ZB	8VB	8XB	8XB	;kB	�B	�TB	��B	�IB	��B
 B
FB
47B
A�B
isB
��B
ĖB
��B
�B
hmB
U�B
I�B
>tB
.B
~B
WB
IB
.B
B	�B	�B	�ZB	�B	�<B	��B	�B	�{B	�'B	��B	��B	��B	��B	��B	��B	��B	şB	�-B	��B	��B	�OB	z�B	jB	_:B	R�B	B�B	32B	,	B	'�B	�B	
?B	!B	B		B��B�B�YB�BɿB�|B�UB�-B��B�B�CB�-B�B}�B{�Bv�Bs�Bp�Bj�BekBehBeiBfpBegBdaBekBh}Bo�Bz�B�B�uB�{B��B��B�zB�pB�iB�[B�DB�&B� B�B�B�B|�Bm�Bl�Bn�Bo�Bl�BflBdbBr�Bv�Bu�By�Bn�Bl�Bl�Br�Bu�Bw�By�B�=B�[B�oB��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B�hB�aB�qB�OB�2B�BB{�Bz�B}�BB|�Bx�Bs�Bt�Bq�Br�B�B�"B�#B�B�B�B}�Bx�Bt�Bs�Bq�Bm�Bi~BflBk�Bo�Bq�Bt�By�B|�BB�B�B�B�B�)B�=B�@B�MB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�B�/B�4B�9B�aB��BBĝBȳB��B��B��B��B��B�B�B�.B�GB�TB�^B�fB�eB�cB�nB�mB�~B�B�B�B��B��B��B��B	B	B	B	#B	
9B	@B	[B	sB	�B	�B	�B	�B	!�B	%�B	'�B	,B	3-B	7FB	8MB	;_B	?zB	@~B	A�B	E�B	K�B	L�B	M�B	N�B	P�B	Q�B	Q�B	Q�B	R�B	R�B	T�B	U�B	WB	WB	^0B	cQB	dTB	ghB	kB	n�B	o�B	p�B	t�B	v�B	v�B	v�B	w�B	x�B	z�B	|�B	|�B	�B	�
B	�B	�#B	�5B	�JB	�TB	�aB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�$B	�6B	�6B	�1B	�1B	�4B	�7B	�HB	�XB	�`B	�jB	�oB	�|B	B	ĒB	ǦB	ǥB	ǤB	ǦB	ǧB	ɲB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�1B	�8B	�8B	�7B	�=B	�HB	�PB	�dB	�oB	�iB	�jB	�[B	�\B	�fB	�bB	�jB	�lB	�uB	�|B	�|B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 B
B
 B
 B
 B
B
B
B
B
B
B
B
B
B
B
 B
 B
 B
!B
	,B
	,B

2B
8B
6B
8B
8B
7B
6B
?B
FB
FB
EB
GB
HB
JB
SG�O�B
fB
�B
�B
'�B
0B
68B
=eB
E�B
I�B
O�B
U�B
[B
^*B
dMB
gaB
l~B
p�B
s�B
w�B
z�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436442016080714364420160807143644  AO  ARCAADJP                                                                    20160216202052    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160216202052  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160216202052  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143644  IP                  G�O�G�O�G�O�                