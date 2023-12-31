CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:09Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190509  20181005190509  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ש�no_1   @ש�`�V@2��E���c�� ě�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B��B'��B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCy�fC{�fC}�fC�  C�  C�  C��C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C��3C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C��C��C�  C��3C��3C��C�  C�  C��C�  C�  C��C�  C��3D � D  D� D  Dy�D  D� D  D� D  D� D  D� DfD� D  D� D��D	� D
  D
y�D
��D� DfD� DfD�fDfD�fDfD� D  D� D  Dy�D  D� D��D� D  D�fD  Dy�D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fDfD�fD   D �fD!fD!�fD"fD"�fD#  D#y�D$  D$� D%fD%� D%��D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9�fD:fD:� D:��D;� D<fD<� D=  D=� D>fD>�fD?  D?� D@fD@� DA  DA�fDB  DB� DCfDC�fDDfDD� DD��DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP�fDQ  DQ� DQ��DR� DSfDS� DS��DTy�DT��DU� DVfDV� DV��DWy�DW��DXy�DX��DY� DZ  DZ� D[fD[�fD\fD\� D]  D]� D^  D^� D_fD_�fD`  D`� Da  Day�Db  Db� Db��Dc� DdfDd�fDe  De� Df  Df�fDg  Dgy�Dg��Dh� Di  Di�fDj  Djy�Dj��Dk� Dl  Dly�Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dyt{D�>�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�(�A{A"{AB{A`z�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC:�C!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`�Cb!HCd:�Cf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv�Cx�Cz�C|�C~�C��C��C��C�qC�qC�qC��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C�qC��C��C�qC�qC��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC�qC��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C�qC�qC�qC��C��C��C�qC��C��C�qC��C��C�qC��D �D �RDRD�RDRD��DRD�RDRD�RDRD�RDRD�RD�D�RDRD�RD	�D	�RD
RD
��D�D�RD�D�RD�D��D�D��D�D�RDRD�RDRD��DRD�RD�D�RDRD��DRD��DRD�RDRD�RDRD�RDRD�RD�D�RDRD�RDRD�RDRD�RDRD��D�D��D RD ��D!�D!��D"�D"��D#RD#��D$RD$�RD%�D%�RD&�D&��D'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,�RD-RD-�RD.RD.�RD/RD/��D0RD0��D1RD1�RD2RD2�RD3RD3�RD4RD4�RD5RD5�RD6RD6�RD7RD7�RD8RD8��D9RD9��D:�D:�RD;�D;�RD<�D<�RD=RD=�RD>�D>��D?RD?�RD@�D@�RDARDA��DBRDB�RDC�DC��DD�DD�RDE�DE�RDFRDF�RDG�DG�RDHRDH�RDIRDI�RDJRDJ�RDKRDK�RDLRDL�RDMRDM�RDNRDN�RDO�DO�RDPRDP��DQRDQ�RDR�DR�RDS�DS�RDT�DT��DU�DU�RDV�DV�RDW�DW��DX�DX��DY�DY�RDZRDZ�RD[�D[��D\�D\�RD]RD]�RD^RD^�RD_�D_��D`RD`�RDaRDa��DbRDb�RDc�Dc�RDd�Dd��DeRDe�RDfRDf��DgRDg��Dh�Dh�RDiRDi��DjRDj��Dk�Dk�RDlRDl��DmRDm�RDnRDn��DoRDo�RDpRDp�RDqRDq��DrRDr�RDsRDs�RDtRDt�RDuRDu�RDvRDv�RDwRDwuDy|�D�B�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A΋DA΋DA΋DA΍PAΏ\AΏ\A΍PA΍PAΏ\AΏ\AΑhAΓuA΍PA΃A�ZA��A�v�A���A˩�A�`BA�JA���AʸRAʬAʅA�;dA�+A�&�A� �A�
=AɋDA�"�A�%A��A��#A�K�A�VA���A�l�A�S�A�oA��A���AƅA���A��A�K�AìA�jA�A�ȴA�S�A�(�A��wA�9XA�ĜA�
=A�&�A�;dA���A��A���A���A�ZA�1A��-A��yA�VA�;dA��A��^A�O�A��A��+A��A���A���A�ƨA��PA��wA�\)A�/A�`BA�S�A���A�G�A�JA��PA�K�A�-A�z�A��-A��;A��A���A��`A�r�A�JA��+A�33A�$�A�A�1A��PA�t�A��#A�+A��DA�VA��yA���A��A��A��A���A�A|��Ay33Av(�AsS�Aq��Aqp�Am��Ah�Af^5Ad��Ab  A_O�A^bA\�\A[��A[hsAZI�AY7LAX�RAX9XAS�hAO�AMl�AK��AJbNAH^5AD��ABI�A@ZA?�-A>�uA<��A:�HA9�#A8(�A6  A4n�A2E�A1p�A0�A/7LA-��A,�A, �A+�mA+�-A*(�A*��A*�+A*�DA*��A*-A)�#A(�9A'�A&��A&(�A%/A$Q�A"�A"bA!t�A ��A��A
=AJA�FA��AbNAQ�AC�A��A�A�hA�AAK�A�7A�;A33A�HA �A�7A�A�7Ar�A��A��A
�9A
�A	G�A�9AAG�A�AffA1A�mA��A�-A�A$�A��A�A ��@��`@���@��w@���@�Q�@���@��@�@�\)@�@�S�@��@�b@���@� �@� �@�
=@��#@��@���@�o@�n�@��@�j@ڏ\@�p�@�1@�~�@�M�@�@ա�@�p�@���@�
=@��@�7L@���@���@�Ĝ@ЋD@�j@��@ϝ�@�+@�ff@��T@͡�@�`B@��`@�Q�@��
@�+@���@�^5@��@�X@ȃ@ǅ@�ff@ź^@�`B@�bN@öF@Ý�@Õ�@ÍP@�\)@�@�ff@��@�j@�  @���@��@��\@�$�@��/@��9@��@�j@�I�@��m@�;d@�n�@���@�V@�9X@���@�dZ@�+@�v�@���@��@��@��@�ff@�-@��u@���@��\@�x�@��#@��+@���@�-@��h@�z�@�E�@���@�33@��9@�/@�I�@�33@�~�@�M�@��@�J@��#@��#@��^@��#@�$�@�ȴ@���@���@�z�@���@��w@��@�j@��u@� �@��
@�|�@��D@�X@�Ĝ@�Q�@���@��@���@�ƨ@�M�@��#@��T@��@�ff@���@���@�\)@��y@��+@�5?@�@�@�`B@��j@��D@�33@���@��#@�%@�ƨ@��H@���@�p�@�hs@�p�@�p�@��@�x�@���@���@��@�5?@�~�@��\@�l�@��@�A�@� �@��@�
=@���@���@���@�C�@��P@���@���@�l�@�l�@�S�@�o@��H@��@��@���@�A�@��H@��@���@�^5@�ff@�E�@�5?@�-@�p�@�p�@���@�&�@��9@��9@��@��@���@��D@�1'@��;@��P@�;d@��@�E�@�-@�@�p�@�V@��9@��u@��u@�r�@�A�@��F@��@�ȴ@�V@�ff@��@���@�V@���@���@���@�bN@��
@���@�dZ@���@��@�O�@�7L@�/@��@�9X@��;@���@��@�\)@�C�@���@��H@���@��\@�~�@�-@��@���@�x�@�p�@�p�@�hs@�hs@�� @v�b@h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A΋DA΋DA΋DA΍PAΏ\AΏ\A΍PA΍PAΏ\AΏ\AΑhAΓuA΍PA΃A�ZA��A�v�A���A˩�A�`BA�JA���AʸRAʬAʅA�;dA�+A�&�A� �A�
=AɋDA�"�A�%A��A��#A�K�A�VA���A�l�A�S�A�oA��A���AƅA���A��A�K�AìA�jA�A�ȴA�S�A�(�A��wA�9XA�ĜA�
=A�&�A�;dA���A��A���A���A�ZA�1A��-A��yA�VA�;dA��A��^A�O�A��A��+A��A���A���A�ƨA��PA��wA�\)A�/A�`BA�S�A���A�G�A�JA��PA�K�A�-A�z�A��-A��;A��A���A��`A�r�A�JA��+A�33A�$�A�A�1A��PA�t�A��#A�+A��DA�VA��yA���A��A��A��A���A�A|��Ay33Av(�AsS�Aq��Aqp�Am��Ah�Af^5Ad��Ab  A_O�A^bA\�\A[��A[hsAZI�AY7LAX�RAX9XAS�hAO�AMl�AK��AJbNAH^5AD��ABI�A@ZA?�-A>�uA<��A:�HA9�#A8(�A6  A4n�A2E�A1p�A0�A/7LA-��A,�A, �A+�mA+�-A*(�A*��A*�+A*�DA*��A*-A)�#A(�9A'�A&��A&(�A%/A$Q�A"�A"bA!t�A ��A��A
=AJA�FA��AbNAQ�AC�A��A�A�hA�AAK�A�7A�;A33A�HA �A�7A�A�7Ar�A��A��A
�9A
�A	G�A�9AAG�A�AffA1A�mA��A�-A�A$�A��A�A ��@��`@���@��w@���@�Q�@���@��@�@�\)@�@�S�@��@�b@���@� �@� �@�
=@��#@��@���@�o@�n�@��@�j@ڏ\@�p�@�1@�~�@�M�@�@ա�@�p�@���@�
=@��@�7L@���@���@�Ĝ@ЋD@�j@��@ϝ�@�+@�ff@��T@͡�@�`B@��`@�Q�@��
@�+@���@�^5@��@�X@ȃ@ǅ@�ff@ź^@�`B@�bN@öF@Ý�@Õ�@ÍP@�\)@�@�ff@��@�j@�  @���@��@��\@�$�@��/@��9@��@�j@�I�@��m@�;d@�n�@���@�V@�9X@���@�dZ@�+@�v�@���@��@��@��@�ff@�-@��u@���@��\@�x�@��#@��+@���@�-@��h@�z�@�E�@���@�33@��9@�/@�I�@�33@�~�@�M�@��@�J@��#@��#@��^@��#@�$�@�ȴ@���@���@�z�@���@��w@��@�j@��u@� �@��
@�|�@��D@�X@�Ĝ@�Q�@���@��@���@�ƨ@�M�@��#@��T@��@�ff@���@���@�\)@��y@��+@�5?@�@�@�`B@��j@��D@�33@���@��#@�%@�ƨ@��H@���@�p�@�hs@�p�@�p�@��@�x�@���@���@��@�5?@�~�@��\@�l�@��@�A�@� �@��@�
=@���@���@���@�C�@��P@���@���@�l�@�l�@�S�@�o@��H@��@��@���@�A�@��H@��@���@�^5@�ff@�E�@�5?@�-@�p�@�p�@���@�&�@��9@��9@��@��@���@��D@�1'@��;@��P@�;d@��@�E�@�-@�@�p�@�V@��9@��u@��u@�r�@�A�@��F@��@�ȴ@�V@�ff@��@���@�V@���@���@���@�bN@��
@���@�dZ@���@��@�O�@�7L@�/@��@�9X@��;@���@��@�\)@�C�@���@��H@���@��\@�~�@�-@��@���@�x�@�p�@�p�@�hs@�hs@�� @v�b@h�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�BB1B	7BPBoB{B�B�B�B�B�B�B�B(�B2-B49B5?B8RBO�BXBR�BW
B`BBhsBo�Bp�Bv�B�VB��B�3B�wBÖBȴB��B�B�)B�`B�B��BBbB�B!�B'�B)�B/BQ�BVBXBYBW
BS�BL�BD�BD�BG�BE�BD�BE�BF�BC�B?}B;dB5?B(�B#�B�B�B1B��B��B��B��B��B�B�ZB�BÖB�FB�!B�B��B��B�PBz�Bp�BgmBI�B33B�BB
�B
�B
�XB
�B
�\B
bNB
6FB
JB	��B	�/B	ɺB	�FB	�B	��B	�{B	z�B	k�B	bNB	T�B	H�B	A�B	:^B	5?B	33B	,B	%�B	!�B	�B	B�B�yB�ZB�TB�#B��B��B��BɺBĜB�}B�^B�LB�?B�3B�!B�B�B�B��B��B��B��B�B�!B�BŢB��B�B�#B�TB�TB�HB�BB�NB�NB�HB�NB�ZB�ZB�ZB�`B�yB�B�B�B�yB�yB�mB�ZB�)B�B�B�
B�
B�#B�NB�/B�B�/B�B�)B�BB�#B�B�5B�mB�B�B�B�B�B�yB�mB�fB�TB�TB�NB�HB�5B�/B�
B��BƨB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�-B�9B�FB�XB�XB��BÖBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�/B�;B�BB�NB�ZB�mB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	%B	+B	DB	oB	�B	"�B	#�B	$�B	/B	1'B	1'B	/B	,B	/B	33B	7LB	8RB	:^B	@�B	?}B	F�B	L�B	J�B	;dB	2-B	6FB	F�B	\)B	[#B	[#B	_;B	dZB	jB	p�B	s�B	p�B	gmB	W
B	VB	aHB	gmB	gmB	gmB	gmB	gmB	gmB	hsB	jB	m�B	n�B	p�B	r�B	v�B	x�B	x�B	u�B	u�B	y�B	}�B	�B	�B	�B	�B	�%B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�jB	�wB	�wB	�wB	�qB	�qB	�qB	�}B	B	ŢB	ŢB	ĜB	ÖB	ĜB	B	�qB	�dB	�XB	�RB	�^B	�jB	��B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�5B	�)B	�#B	�#B	�/B	�5B	�HB	�ZB	�ZB	�ZB	�mB	�mB	�fB	�`B	�ZB	�HB	�BB	�NB	�`B	�NB	�BB	�HB	�`B	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
	7B

=B

=B
DB
VB
vB
!B
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
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
�BB1B	7BPBoB{B�B�B�B�B�B�B�B(�B2-B49B5?B8RBO�BXBR�BW
B`BBhsBo�Bp�Bv�B�VB��B�3B�wBÖBȴB��B�B�)B�`B�B��BBbB�B!�B'�B)�B/BQ�BVBXBYBW
BS�BL�BD�BD�BG�BE�BD�BE�BF�BC�B?}B;dB5?B(�B#�B�B�B1B��B��B��B��B��B�B�ZB�BÖB�FB�!B�B��B��B�PBz�Bp�BgmBI�B33B�BB
�B
�B
�XB
�B
�\B
bNB
6FB
JB	��B	�/B	ɺB	�FB	�B	��B	�{B	z�B	k�B	bNB	T�B	H�B	A�B	:^B	5?B	33B	,B	%�B	!�B	�B	B�B�yB�ZB�TB�#B��B��B��BɺBĜB�}B�^B�LB�?B�3B�!B�B�B�B��B��B��B��B�B�!B�BŢB��B�B�#B�TB�TB�HB�BB�NB�NB�HB�NB�ZB�ZB�ZB�`B�yB�B�B�B�yB�yB�mB�ZB�)B�B�B�
B�
B�#B�NB�/B�B�/B�B�)B�BB�#B�B�5B�mB�B�B�B�B�B�yB�mB�fB�TB�TB�NB�HB�5B�/B�
B��BƨB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�-B�9B�FB�XB�XB��BÖBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�/B�;B�BB�NB�ZB�mB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	%B	+B	DB	oB	�B	"�B	#�B	$�B	/B	1'B	1'B	/B	,B	/B	33B	7LB	8RB	:^B	@�B	?}B	F�B	L�B	J�B	;dB	2-B	6FB	F�B	\)B	[#B	[#B	_;B	dZB	jB	p�B	s�B	p�B	gmB	W
B	VB	aHB	gmB	gmB	gmB	gmB	gmB	gmB	hsB	jB	m�B	n�B	p�B	r�B	v�B	x�B	x�B	u�B	u�B	y�B	}�B	�B	�B	�B	�B	�%B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�jB	�wB	�wB	�wB	�qB	�qB	�qB	�}B	B	ŢB	ŢB	ĜB	ÖB	ĜB	B	�qB	�dB	�XB	�RB	�^B	�jB	��B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�5B	�)B	�#B	�#B	�/B	�5B	�HB	�ZB	�ZB	�ZB	�mB	�mB	�fB	�`B	�ZB	�HB	�BB	�NB	�`B	�NB	�BB	�HB	�`B	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
	7B

=B

=B
DB
VB
vB
!B
/�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190509                              AO  ARCAADJP                                                                    20181005190509    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190509  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190509  QCF$                G�O�G�O�G�O�8000            