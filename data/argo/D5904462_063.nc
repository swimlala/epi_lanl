CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-17T02:15:43Z AOML 3.0 creation; 2016-08-07T21:51:19Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150717021543  20160807145119  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ?A   AO  5287_9017_063                   2C  D   APEX                            6529                            072314                          846 @�_)�
@1   @�_*}'�@0��l�C��d�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ?A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D� D�` D�� D�ٚD� D�@ D�s3D���D��fD�VfD��3D��fD��D�L�Dڌ�D���D�	�D�&fD�VfD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C :�C!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp:�Cr!HCt!HCv!HCx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"�RD#RD#�RD$RD$�RD%RD%�RD&RD&�RD'�D'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@�RDARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE�RDFRDF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDORDO�RDPRDP�RDQRDQ�RDRRDR�RDSRDS�RDTRDT�RDURDU�RDVRDV�RDWRDW�RDXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\�RD]RD]�RD^RD^�RD_RD_�RD`RD`�RDaRDa�RDbRDb�RDcRDc�RDdRDd�RDeRDe�RDfRDf�RDgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDy{�D�)D�d)D��)D���D�)D�D)D�w\D���D��D�Z�D��\D�ڏD��D�P�Dڐ�D���D��D�*�D�Z�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AܓuAܛ�Aܝ�Aܟ�Aܟ�Aܣ�Aܥ�Aܰ!Aܴ9Aܴ9Aܴ9Aܴ9AܸRAܺ^Aܺ^Aܺ^Aܺ^Aܺ^AܾwAܾwA�ĜA�ĜA���A��
A��#A��HA��HA��HA���A�jAۇ+A�\)A�%AړuA�hsAكA�oA԰!Aԝ�A���A�r�AռjAԕ�A�v�A�%A�r�AϬA�ĜA�/A��A�$�A�=qA�p�A�1AʶFA�ffA���A�^5AǙ�A��AƃA�&�A���A�G�A��HA�r�A��hA�Q�A���A�33A�{A�G�A��A��-A��HA�7LA�  A���A��A�I�A��A� �A�ZA��A���A�{A�G�A�/A�bA��`A���A���A�=qA�?}A��RA��A���A��A�VA�1A���A�z�A���A��A��A���A��/A��DA�9XA���A��PA�/A��A~I�A}C�Ay33AxȴAy
=Aw\)ArZAl1'Ah�Ad�DAc��Aa��A]S�A\��A\(�A[�A[/AZ�AV��AQAO\)AMoAK\)AJz�AIXAHr�AF�`AD-AA��A>ĜA>1A=33A;�#A;�A:��A9�^A7�-A4�\A3��A3��A3t�A2Q�A0jA/33A-\)A+�-A)oA((�A&bA$��A%A%`BA$��A$VA"�A!XA��A��Ar�A�AVA�yA��AM�AA�TA�A�AC�AA�A�A�+A�
A��A7LA5?A�PA
��A��AhsA33A��A�A�-A��A�A��AK�A��A-At�A+A �yA ��@��@��#@��D@�  @�$�@��u@�A�@�  @��@��@�P@�^5@�@���@��@�X@��`@�K�@�(�@���@��@��@��@�@��@�@��@���@��H@��@��@웦@�j@�o@�7L@� �@�33@��m@�x�@߶F@��@��@݉7@�X@܃@۶F@ّh@��@؋D@�z�@�j@��@�dZ@�S�@�\)@�t�@�S�@�33@֏\@��@Լj@ӶF@�@�v�@���@�n�@�l�@�V@��@͑h@̼j@̴9@̛�@̋D@�|�@�M�@ɩ�@�`B@���@�%@ȣ�@��@ǝ�@�"�@�=q@�G�@ě�@�G�@��@�n�@ř�@���@Ý�@�|�@�l�@�\)@�dZ@�|�@��@ēu@�V@�Ĝ@Ĭ@��@Å@��^@���@���@�%@���@��u@��@��@�33@���@���@�G�@��@�z�@�(�@�l�@���@�^5@�$�@�{@���@��@�@��@�O�@�V@��9@�Z@���@��/@��
@�C�@���@�V@�E�@�@��^@��h@�7L@���@���@�A�@�1@�ƨ@��w@���@��@�C�@��@�V@���@���@��@���@�Ĝ@�Ĝ@�Ĝ@��j@��9@��D@�I�@���@��@�C�@��@�~�@�E�@��@�?}@�I�@���@�@��@�ȴ@�M�@��T@�x�@�p�@�O�@�/@��/@��m@�t�@�o@��R@�~�@�V@�$�@��@��#@���@�O�@��/@�Ĝ@��j@���@�Z@�1'@���@�dZ@�+@��!@�=q@�J@��7@�/@��/@��/@���@��j@��9@�ƨ@��@���@�~�@���@���@��@�v�@�@�p�@�X@�X@�O�@�X@�X@�&�@��@��u@�Z@�(�@�b@�b@�b@�ƨ@�t�@�l�@�o@���@�-@��@��h@�/@���@��9@��@� �@���@��F@���@��P@�K�@���@��\@��\@�ff@�=q@�@��#@�`B@���@��j@���@��D@�bN@�Q�@�I�@�A�@�(�@��;@�ƨ@�l�@�o@��H@���@�J@��w@�K�@�/@��@u`B@m��@e?}@]�-@T�@Jn�@C"�@;��@3C�@.ff@&v�@!hs@?}@%@��@�\@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AܓuAܛ�Aܝ�Aܟ�Aܟ�Aܣ�Aܥ�Aܰ!Aܴ9Aܴ9Aܴ9Aܴ9AܸRAܺ^Aܺ^Aܺ^Aܺ^Aܺ^AܾwAܾwA�ĜA�ĜA���A��
A��#A��HA��HA��HA���A�jAۇ+A�\)A�%AړuA�hsAكA�oA԰!Aԝ�A���A�r�AռjAԕ�A�v�A�%A�r�AϬA�ĜA�/A��A�$�A�=qA�p�A�1AʶFA�ffA���A�^5AǙ�A��AƃA�&�A���A�G�A��HA�r�A��hA�Q�A���A�33A�{A�G�A��A��-A��HA�7LA�  A���A��A�I�A��A� �A�ZA��A���A�{A�G�A�/A�bA��`A���A���A�=qA�?}A��RA��A���A��A�VA�1A���A�z�A���A��A��A���A��/A��DA�9XA���A��PA�/A��A~I�A}C�Ay33AxȴAy
=Aw\)ArZAl1'Ah�Ad�DAc��Aa��A]S�A\��A\(�A[�A[/AZ�AV��AQAO\)AMoAK\)AJz�AIXAHr�AF�`AD-AA��A>ĜA>1A=33A;�#A;�A:��A9�^A7�-A4�\A3��A3��A3t�A2Q�A0jA/33A-\)A+�-A)oA((�A&bA$��A%A%`BA$��A$VA"�A!XA��A��Ar�A�AVA�yA��AM�AA�TA�A�AC�AA�A�A�+A�
A��A7LA5?A�PA
��A��AhsA33A��A�A�-A��A�A��AK�A��A-At�A+A �yA ��@��@��#@��D@�  @�$�@��u@�A�@�  @��@��@�P@�^5@�@���@��@�X@��`@�K�@�(�@���@��@��@��@�@��@�@��@���@��H@��@��@웦@�j@�o@�7L@� �@�33@��m@�x�@߶F@��@��@݉7@�X@܃@۶F@ّh@��@؋D@�z�@�j@��@�dZ@�S�@�\)@�t�@�S�@�33@֏\@��@Լj@ӶF@�@�v�@���@�n�@�l�@�V@��@͑h@̼j@̴9@̛�@̋D@�|�@�M�@ɩ�@�`B@���@�%@ȣ�@��@ǝ�@�"�@�=q@�G�@ě�@�G�@��@�n�@ř�@���@Ý�@�|�@�l�@�\)@�dZ@�|�@��@ēu@�V@�Ĝ@Ĭ@��@Å@��^@���@���@�%@���@��u@��@��@�33@���@���@�G�@��@�z�@�(�@�l�@���@�^5@�$�@�{@���@��@�@��@�O�@�V@��9@�Z@���@��/@��
@�C�@���@�V@�E�@�@��^@��h@�7L@���@���@�A�@�1@�ƨ@��w@���@��@�C�@��@�V@���@���@��@���@�Ĝ@�Ĝ@�Ĝ@��j@��9@��D@�I�@���@��@�C�@��@�~�@�E�@��@�?}@�I�@���@�@��@�ȴ@�M�@��T@�x�@�p�@�O�@�/@��/@��m@�t�@�o@��R@�~�@�V@�$�@��@��#@���@�O�@��/@�Ĝ@��j@���@�Z@�1'@���@�dZ@�+@��!@�=q@�J@��7@�/@��/@��/@���@��j@��9@�ƨ@��@���@�~�@���@���@��@�v�@�@�p�@�X@�X@�O�@�X@�X@�&�@��@��u@�Z@�(�@�b@�b@�b@�ƨ@�t�@�l�@�o@���@�-@��@��h@�/@���@��9@��@� �@���@��F@���@��P@�K�@���@��\@��\@�ff@�=q@�@��#@�`B@���@��j@���@��D@�bN@�Q�@�I�@�A�@�(�@��;@�ƨ@�l�@�o@��H@���G�O�@��w@�K�@�/@��@u`B@m��@e?}@]�-@T�@Jn�@C"�@;��@3C�@.ff@&v�@!hs@?}@%@��@�\@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	~�B	}�B	}�B	}�B	}�B	}�B	~�B	~�B	~�B	}�B	}�B	}�B	~�B	~�B	~�B	}�B	}�B	}�B	~�B	~�B	�B	�B	�B	�+B	�1B	�=B	�JB	�JB	�PB	�VB	��B	��B	��B	ÖB	��B	�B
�B
�B
w�B
�BaHBs�Bx�BYB0!B�B)�BR�Bw�B��B��B�B�yBPB33B9XB>wBG�BT�B}�B�=B�1B�1B��B��B�FB�'B��B��B�RB�'B�B�?B�FBŢB��Bw�Bu�By�Bv�B� B� Bt�BC�B�B{B\B�B��B�3B�B�B�}B�Bv�BG�B>wB=qB=qBB�BG�BH�B?}B49B�B
�B
��B
~�B
ZB
:^B
.B
�B	��B	�B	�`B	�NB	��B	��B
B	�TB	�9B	��B	�%B	{�B	l�B	XB	VB	S�B	P�B	L�B	F�B	7LB	#�B	�B	PB	B	B��B��B�B�B�sB�fB�`B�NB�HB�HB�BB�5B�#B�#B�B�B�B�B�/B�/B�HB�mB�`B�B�)B�B�B	B	B��B�B��B	B�ZB�#B�;B�B	%B	1B	
=B	
=B	DB		7B	B��B��B��B�}B�qB�B�fB�HB�B��BB�qB�qB�dB�RB�^B�^B�jB�qB�jB�jB�jB��BÖBɺB��B��B�
B�/B�/B�HB�fB�yB�B�B��B�B�B�B�B�B��B��B��B	B	JB	hB	�B	$�B	-B	-B	0!B	0!B	/B	2-B	.B	-B	2-B	49B	49B	2-B	0!B	/B	,B	+B	,B	,B	,B	-B	1'B	2-B	6FB	8RB	8RB	7LB	7LB	7LB	7LB	8RB	?}B	B�B	G�B	K�B	M�B	O�B	N�B	R�B	^5B	_;B	cTB	jB	gmB	^5B	^5B	]/B	]/B	bNB	ffB	e`B	e`B	bNB	_;B	^5B	_;B	_;B	bNB	cTB	e`B	e`B	e`B	e`B	iyB	p�B	z�B	� B	�+B	�7B	�%B	�%B	�%B	�%B	�%B	�+B	�1B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�RB	�dB	�qB	�qB	�qB	�}B	��B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�/B	�)B	�)B	�B	�B	�/B	�5B	�/B	�5B	�;B	�;B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�`B	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
%B
+B
1B
DB
{B
�B
(�B
/B
49B
8RB
=qB
C�B
J�B
O�B
T�B
]/B
`BB
gmB
k�B
m�B
q�B
t�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	 B	}�B	}�B	}�B	}�B	}�B	 B	~�B	~�B	}�B	}�B	}�B	~�B	~�B	~�B	}�B	}�B	}�B	~�B	~�B	�	B	�	B	�B	�-B	�5B	�BB	�MB	�NB	�TB	�ZB	��B	��B	��B	×B	��B	�B
�B
�B
w�B
�Ba>Bs�Bx�BYB0B�B)�BR�Bw�B��B��B�	B�nBEB3&B9OB>mBG�BT�B}�B�3B�%B�'B�{B��B�<B�B��B�}B�IB�B�B�4B�<BŖB��Bw�Bu�By�Bv�B�B�Bt�BC�B�BpBPB�B��B�(B��B��B�pB�Bv�BG�B>nB=iB=gBB�BG�BH�B?qB4+B�B
�B
��B
~�B
ZB
:YB
.B
~B	��B	�B	�\B	�HB	��B	��B
B	�PB	�6B	��B	�"B	{�B	l�B	XB	VB	S�B	P�B	L�B	F�B	7NB	#�B	�B	TB	!B	B��B��B�B�B�wB�jB�cB�RB�JB�JB�EB�;B�%B�'B�"B�"B�B�B�1B�2B�KB�rB�dB�B�+B�B�B	B	B��B�B��B	B�]B�'B�>B�B	$B	3B	
>B	
?B	DB		6B	B��B��B��B��B�rB�B�iB�KB�B��BB�sB�rB�iB�UB�aB�aB�lB�rB�nB�mB�mB��BÙBɻB��B��B�	B�2B�1B�JB�eB�{B�B�B��B�B�B�B�B�B��B��B��B	B	IB	hB	�B	$�B	-B	-B	0 B	0 B	/B	2+B	.B	-B	2+B	47B	48B	2*B	0 B	/B	,B	*�B	,B	,B	,B	-B	1$B	2+B	6BB	8LB	8MB	7HB	7HB	7GB	7HB	8NB	?yB	B�B	G�B	K�B	M�B	O�B	N�B	R�B	^2B	_6B	cPB	jzB	ghB	^,B	^1B	]*B	]*B	bHB	faB	e[B	eYB	bGB	_7B	^1B	_6B	_6B	bHB	cPB	e\B	e[B	eYB	eYB	irB	p�B	z�B	�B	�#B	�0B	�B	�B	�B	�B	�B	�$B	�-B	�DB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�&B	�-B	�7B	�KB	�_B	�iB	�iB	�iB	�uB	�B	B	ēB	ŘB	ǧB	ȬB	ʹB	˾B	˼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	�&B	�&B	�&B	�'B	�(B	�2B	�2B	�3B	�2B	�1B	�2B	�+B	�&B	�B	� B	�B	�B	�&B	�+B	�'B	�*B	�0B	�2B	�,B	�+B	�-B	�.B	�2B	�:B	�>B	�CB	�BB	�JB	�KB	�IB	�YB	�ZB	�iB	�mB	�nB	�wB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
	B
B
B
B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
"B
B
 G�O�B
:B
nB
�B
(�B
/B
4/B
8FB
=eB
C�B
J�B
O�B
T�B
]$B
`5B
gaB
kwB
m�B
q�B
t�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.13 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451192016080714511920160807145119  AO  ARCAADJP                                                                    20150717021543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150717021543  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150717021543  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145119  IP                  G�O�G�O�G�O�                