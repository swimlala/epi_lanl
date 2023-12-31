CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:22Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140822  20181024140822  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               cA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d� n1   @��eww�0@3�vȴ9X�cΟ�vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      cA   A   B   @9��@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(fD(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DBy�DC  DC� DD  DD� DD��DE� DFfDF�fDGfDG� DH  DH� DI  DIy�DJ  DJ� DKfDK� DL  DL� DM  DMy�DM��DNy�DO  DOy�DP  DP� DQ  DQ� DR  DR� DR��DSy�DT  DT� DU  DU� DV  DV�fDWfDW� DX  DX� DX��DY� DZ  DZ� D[fD[� D\  Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dx  DxS3Dy��D�HR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@A�@�(�@�(�A{A"{AB{Ac�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�=pB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�\B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC�C!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6:�C8:�C:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd�Cf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv�Cx!HCz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC�qC��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD�RDRD�RDRD�RDRD��D�D��DRD�RDRD�RD	RD	�RD
RD
�RDRD�RDRD�RDRD�RDRD��D�D�RDRD�RDRD�RD�D�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD RD �RD!RD!�RD"RD"��D#RD#�RD$RD$�RD%RD%�RD&RD&�RD'RD'��D(�D(��D)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.��D/�D/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8�D8�RD9RD9�RD:RD:�RD;RD;�RD<RD<�RD=RD=�RD>RD>�RD?RD?�RD@�D@�RDARDA�RDBRDB��DCRDC�RDDRDD�RDE�DE�RDF�DF��DG�DG�RDHRDH�RDIRDI��DJRDJ�RDK�DK�RDLRDL�RDMRDM��DN�DN��DORDO��DPRDP�RDQRDQ�RDRRDR�RDS�DS��DTRDT�RDURDU�RDVRDV��DW�DW�RDXRDX�RDY�DY�RDZRDZ�RD[�D[�RD\RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDlRDl�RDmRDm�RDnRDn�RDoRDo��DpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDu�Du�RDvRDv�RDwRDw�RDxRDx[�Dy�)D�L{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A߾wA߾wA߾wA���A���A߸RA߬Aߙ�A�r�A�C�A�A��
A���Aް!Aޡ�Aޡ�Aޟ�Aޛ�AޓuA�ffA�I�A��A�r�A��yA׏\A�=qA�p�A�
=AԃA�  A΋DA��A̛�Aˡ�A��A���A��Aħ�A�ƨA�v�A��RA��7A�`BA��HA�  A�\)A�\)A�1'A��A��wA�C�A���A�7LA��;A�I�A�S�A�M�A�A�+A�%A�A�A��A���A��
A�{A��A�A�`BA��#A��A���A�A��9A���A��A�bNA��HA���A�
=A�~�A�(�A�M�A�VA��;A���A�n�A���A���A��RA���A��!A���A�{A� �A�r�A���A��RA��A���A�bNA�bA���A�`BA��;A�(�A���A~�DA|{A{p�A{�Az��AzbAw��As�-An��Am��Al�\Aj�DAiO�Ah=qAf~�Ab��A_�7A\��A[XAZAYdZAX�\AWO�AV�\AT��AO�AK`BAF�RAF-AE�AE"�AD=qAC�hAB�AB-AA%A?|�A>ffA;�hA:v�A9��A9�A8��A8(�A7|�A6��A6�A4��A2VA0�`A0�A/A-��A+��A)�A&bNA#��A"VA!�PA ��A�Ax�AZAXA%A�+A{AƨA33AVAbNAXA��A�AS�A��An�A�TAXA�`A^5A\)A
��A
I�A	�FA	?}A��AM�A?}A�jA�+Ar�AA�AbA�#AA��A ��A Q�A  �@��m@�"�@�J@�?}@���@��9@���@�Z@�v�@��
@���@�K�@陚@�bN@�K�@�ȴ@�v�@��@�Z@ާ�@��@ݑh@�Q�@�p�@�Q�@���@�dZ@�33@�&�@���@ͺ^@�|�@�M�@�j@�n�@ģ�@�S�@�M�@�J@���@��-@�ȴ@�Q�@��@�C�@�t�@ǝ�@�\)@�33@���@ũ�@�7L@��`@ēu@�A�@���@�\)@��y@�=q@���@�p�@�z�@�1@���@��@���@�$�@���@��T@�@���@���@���@��m@���@�33@�t�@��H@�@�7L@���@�I�@��F@�33@�~�@�{@��@���@��@�bN@�(�@� �@�A�@�ƨ@�
=@�v�@�v�@�ff@�M�@��@���@��#@��T@�p�@�z�@�1'@�1@��m@���@�1@��@��@���@���@�+@��@���@�$�@��^@���@�1@�ƨ@��F@���@�|�@�S�@�
=@��@��@�{@�x�@�x�@�G�@��/@��D@� �@�ƨ@���@�
=@��H@���@���@���@�v�@�$�@��h@�x�@�x�@�p�@�hs@�`B@�?}@���@�1'@���@�\)@��@�o@���@��!@���@���@���@�v�@�5?@��@��T@�&�@�Ĝ@��u@�z�@�1'@�ƨ@�dZ@��R@��+@�-@�@��h@��7@��@���@��/@��D@�9X@��;@��P@�l�@�o@�M�@��@��@�Ĝ@��@�Q�@�1'@� �@�  @��m@���@�
=@��!@���@�n�@�5?@��T@���@�?}@�%@��/@�bN@� �@��P@��@���@�5?@��@��-@��@�`B@�7L@�&�@�V@��9@�bN@�Z@�Q�@�(�@�b@��@��
@��@���@�|�@��@���@��@�dZ@�@�n�@�@��7@�X@��@�%@���@�j@�I�@�I�@�I�@�I�@�1'@��@�l�@�S�@�33@�o@��y@���@�~�@�-@��^@��7@�hs@�X@�?}@�7L@��@���@q�N11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A߾wA߾wA߾wA���A���A߸RA߬Aߙ�A�r�A�C�A�A��
A���Aް!Aޡ�Aޡ�Aޟ�Aޛ�AޓuA�ffA�I�A��A�r�A��yA׏\A�=qA�p�A�
=AԃA�  A΋DA��A̛�Aˡ�A��A���A��Aħ�A�ƨA�v�A��RA��7A�`BA��HA�  A�\)A�\)A�1'A��A��wA�C�A���A�7LA��;A�I�A�S�A�M�A�A�+A�%A�A�A��A���A��
A�{A��A�A�`BA��#A��A���A�A��9A���A��A�bNA��HA���A�
=A�~�A�(�A�M�A�VA��;A���A�n�A���A���A��RA���A��!A���A�{A� �A�r�A���A��RA��A���A�bNA�bA���A�`BA��;A�(�A���A~�DA|{A{p�A{�Az��AzbAw��As�-An��Am��Al�\Aj�DAiO�Ah=qAf~�Ab��A_�7A\��A[XAZAYdZAX�\AWO�AV�\AT��AO�AK`BAF�RAF-AE�AE"�AD=qAC�hAB�AB-AA%A?|�A>ffA;�hA:v�A9��A9�A8��A8(�A7|�A6��A6�A4��A2VA0�`A0�A/A-��A+��A)�A&bNA#��A"VA!�PA ��A�Ax�AZAXA%A�+A{AƨA33AVAbNAXA��A�AS�A��An�A�TAXA�`A^5A\)A
��A
I�A	�FA	?}A��AM�A?}A�jA�+Ar�AA�AbA�#AA��A ��A Q�A  �@��m@�"�@�J@�?}@���@��9@���@�Z@�v�@��
@���@�K�@陚@�bN@�K�@�ȴ@�v�@��@�Z@ާ�@��@ݑh@�Q�@�p�@�Q�@���@�dZ@�33@�&�@���@ͺ^@�|�@�M�@�j@�n�@ģ�@�S�@�M�@�J@���@��-@�ȴ@�Q�@��@�C�@�t�@ǝ�@�\)@�33@���@ũ�@�7L@��`@ēu@�A�@���@�\)@��y@�=q@���@�p�@�z�@�1@���@��@���@�$�@���@��T@�@���@���@���@��m@���@�33@�t�@��H@�@�7L@���@�I�@��F@�33@�~�@�{@��@���@��@�bN@�(�@� �@�A�@�ƨ@�
=@�v�@�v�@�ff@�M�@��@���@��#@��T@�p�@�z�@�1'@�1@��m@���@�1@��@��@���@���@�+@��@���@�$�@��^@���@�1@�ƨ@��F@���@�|�@�S�@�
=@��@��@�{@�x�@�x�@�G�@��/@��D@� �@�ƨ@���@�
=@��H@���@���@���@�v�@�$�@��h@�x�@�x�@�p�@�hs@�`B@�?}@���@�1'@���@�\)@��@�o@���@��!@���@���@���@�v�@�5?@��@��T@�&�@�Ĝ@��u@�z�@�1'@�ƨ@�dZ@��R@��+@�-@�@��h@��7@��@���@��/@��D@�9X@��;@��P@�l�@�o@�M�@��@��@�Ĝ@��@�Q�@�1'@� �@�  @��m@���@�
=@��!@���@�n�@�5?@��T@���@�?}@�%@��/@�bN@� �@��P@��@���@�5?@��@��-@��@�`B@�7L@�&�@�V@��9@�bN@�Z@�Q�@�(�@�b@��@��
@��@���@�|�@��@���@��@�dZ@�@�n�@�@��7@�X@��@�%@���@�j@�I�@�I�@�I�@�I�@�1'@��@�l�@�S�@�33@�o@��y@���@�~�@�-@��^@��7@�hs@�X@�?}@�7L@��@���@q�N11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB�BB�BC�BC�BD�BH�BJ�BM�BP�BT�BYBZBZBZBYBYBYBYBXBXBXBZBYB`BBhsBiyBiyBl�Bn�BjBv�B~�B�%B�bB��B��B�HB�B�B��BB�B%�B33BI�By�B��B��B��B�B�3B�B��B�hB�PB�%B|�Bu�B�{B�\B�\B�+B�=B�PB�By�Bz�B�B�B}�Bu�B|�B�B�B�Bu�BffBT�BN�BE�B>wB0!B-B+B%�B"�B�B	7B��B�/BɺB�^B�hBt�B_;BG�B�B
��B
��B
�wB
�^B
�FB
�B
��B
�B
cTB
O�B
@�B
;dB
8RB
33B
,B
�B
B	�yB	�BB	�
B	��B	ÖB	�dB	�B	��B	�B	t�B	l�B	gmB	bNB	]/B	T�B	N�B	@�B	$�B	VB��B��B��B�B�B�B�yB�`B�HB�#B�B��BɺBǮBĜBÖB��B�wB�jB�XB�?B�B��B��B��B��B��B�hB�DB�1B�%B�B�B�B�B�1B�PB�VB�\B�\B�\B�JB�B|�B�B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�?B�B��B��B�B��B��B��B��B��B��B�B�FB��B��B�B�mB�B��B��B��B��B��B	  B	B	B	B	B	B	B		7B	PB	\B	�B	�B	�B	"�B	)�B	-B	/B	0!B	1'B	33B	33B	2-B	33B	5?B	:^B	?}B	?}B	<jB	<jB	?}B	B�B	D�B	E�B	I�B	L�B	L�B	M�B	Q�B	W
B	\)B	`BB	bNB	ffB	k�B	n�B	s�B	u�B	w�B	x�B	y�B	y�B	|�B	�+B	�7B	�DB	�JB	�JB	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�?B	�FB	�LB	�qB	�wB	�}B	��B	��B	��B	ÖB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�
B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�mB	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��A4~�B
B
B
B
B
B
%B
1B
1B
	7B
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
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
uB
B
(
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111BB�BB�BC�BC�BD�BH�BJ�BM�BP�BT�BYBZBZBZBYBYBYBYBXBXBXBZBYB`BBhsBiyBiyBl�Bn�BjBv�B~�B�%B�bB��B��B�HB�B�B��BB�B%�B33BI�By�B��B��B��B�B�3B�B��B�hB�PB�%B|�Bu�B�{B�\B�\B�+B�=B�PB�By�Bz�B�B�B}�Bu�B|�B�B�B�Bu�BffBT�BN�BE�B>wB0!B-B+B%�B"�B�B	7B��B�/BɺB�^B�hBt�B_;BG�B�B
��B
��B
�wB
�^B
�FB
�B
��B
�B
cTB
O�B
@�B
;dB
8RB
33B
,B
�B
B	�yB	�BB	�
B	��B	ÖB	�dB	�B	��B	�B	t�B	l�B	gmB	bNB	]/B	T�B	N�B	@�B	$�B	VB��B��B��B�B�B�B�yB�`B�HB�#B�B��BɺBǮBĜBÖB��B�wB�jB�XB�?B�B��B��B��B��B��B�hB�DB�1B�%B�B�B�B�B�1B�PB�VB�\B�\B�\B�JB�B|�B�B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�?B�B��B��B�B��B��B��B��B��B��B�B�FB��B��B�B�mB�B��B��B��B��B��B	  B	B	B	B	B	B	B		7B	PB	\B	�B	�B	�B	"�B	)�B	-B	/B	0!B	1'B	33B	33B	2-B	33B	5?B	:^B	?}B	?}B	<jB	<jB	?}B	B�B	D�B	E�B	I�B	L�B	L�B	M�B	Q�B	W
B	\)B	`BB	bNB	ffB	k�B	n�B	s�B	u�B	w�B	x�B	y�B	y�B	|�B	�+B	�7B	�DB	�JB	�JB	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�?B	�FB	�LB	�qB	�wB	�}B	��B	��B	��B	ÖB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�
B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�mB	�`B	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��A4~�B
B
B
B
B
B
%B
1B
1B
	7B
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
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
uB
B
(
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140822                              AO  ARCAADJP                                                                    20181024140822    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140822  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140822  QCF$                G�O�G�O�G�O�0               