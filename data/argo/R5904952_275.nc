CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:08Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190608  20181005190608  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�(�m1   @��*q�1p@2/��-V�c��\(��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @9��@�  @�  A   AffA@  Aa��A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B0  B8ffB@  BG��BP  BX  B`  Bh  Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C�C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D	  D	� D	��D
� D  D� D  D� D  D� DfD� D��D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D��Dy�D   D � D!  D!� D!��D"y�D"��D#y�D#��D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6fD6� D7  D7� D8  D8� D8��D9� D:  D:� D:��D;y�D;��D<y�D<��D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DCfDC�fDD  DD� DEfDE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DJ��DK� DL  DLy�DM  DM�fDN  DN� DO  DO� DPfDP� DQ  DQ�fDRfDR�fDS  DSy�DT  DT�fDU  DU� DV  DV�fDWfDW� DW��DX� DY  DY� DY��DZy�DZ��D[� D\  D\y�D]  D]�fD^  D^� D_fD_� D_��D`� Da  Day�Db  Db� Dc  Dc� Dc��Dd� DefDe�fDf  Df� DgfDg� Dg��Dh� Di  Di� Dj  Dj�fDkfDk� Dl  Dl� Dm  Dm�fDnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dw�3Dy�3D�K3D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@�(�@�(�A{A z�AB{Ac�A�
=A�=pA�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC!HC!HC!HC!HC
!HC�C!HC!HC!HC!HC!HC:�C!HC!HC!HC :�C"!HC$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@:�CB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj�Cl!HCn!HCp!HCr!HCt!HCv!HCx:�Cz!HC|!HC~!HC��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C�qC��C��C�qC��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C�qC��C��C��D RD �RD�D�RDRD�RDRD�RDRD�RDRD�RD�D��D�D�RDRD�RD	RD	�RD
�D
�RDRD�RDRD�RDRD�RD�D�RD�D�RD�D�RDRD�RDRD�RDRD�RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD��D�D�RDRD�RD�D��D RD �RD!RD!�RD"�D"��D#�D#��D$�D$�RD%�D%�RD&RD&�RD'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+�RD,RD,��D-RD-�RD.RD.�RD/RD/�RD0RD0�RD1RD1�RD2RD2�RD3RD3�RD4�D4�RD5RD5�RD6�D6�RD7RD7�RD8RD8�RD9�D9�RD:RD:�RD;�D;��D<�D<��D=�D=�RD>RD>��D?RD?�RD@RD@�RDARDA�RDBRDB�RDC�DC��DDRDD�RDE�DE�RDF�DF�RDGRDG�RDHRDH�RDIRDI�RDJRDJ��DK�DK�RDLRDL��DMRDM��DNRDN�RDORDO�RDP�DP�RDQRDQ��DR�DR��DSRDS��DTRDT��DURDU�RDVRDV��DW�DW�RDX�DX�RDYRDY�RDZ�DZ��D[�D[�RD\RD\��D]RD]��D^RD^�RD_�D_�RD`�D`�RDaRDa��DbRDb�RDcRDc�RDd�Dd�RDe�De��DfRDf�RDg�Dg�RDh�Dh�RDiRDi�RDjRDj��Dk�Dk�RDlRDl�RDmRDm��Dn�Dn�RDoRDo�RDpRDp�RDqRDq�RDrRDr��DsRDs�RDtRDt�RDuRDu��DvRDv�RDwRDw�RDw��Dy��D�O\D��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��#A��#A��/A���A�VA�z�A�C�A�bNA�A�1A�ȴA�ZA���A�jA��#A�ZA¬A�x�A�-A���A��7A�-A��/A���A���A��hA��A�t�A�I�A�1A��#A��A�jA�^5A�;dA��A��A��A���A�l�A��#A��hA��A��7A���A��`A���A��^A���A���A�9XA��A��RA�  A��wA�p�A�=qA�oA��yA���A��A��#A�=qA�(�A�S�A���A� �A��A��9A���A�E�A�
=A���A�G�A�1A�jA��TA�^5A�;dA�/A���A��A���A�M�A��A�7LA�`BA��mA���A��A�r�A�I�A��-A��A�-A~�jAw�TAt�yAr��Ap  Ajn�Ae+Abr�A_��A\�AX^5AV(�AO��AJ=qAGt�AE�AEG�AE%AB�!AA��A@�!A?7LA>�+A=/A=�A<��A;dZA9�-A7�A7dZA5"�A2�A01'A-�^A, �A+hsA*�A*�A)C�A((�A'7LA&��A$�A"��A"1A JA�#A"�A��A�A^5A�AJA��AI�At�A�AjAĜA1A��A�7A|�A33A�An�AA|�AhsA|�A7LAAz�A�A&�A
�A
v�A
Q�A
�DA
��A
z�A
^5A
  A
{A	�A	"�A1A�A\)A��AI�A�PA�RAA�hA
=A �+A bNA M�A V@�ȴ@�@�A�@��;@�33@�-@�?}@��@��T@��/@�%@�/@���@��@�{@��/@�u@�1@��@��@��@�\@��@�@��/@�z�@��@�V@�p�@�7L@��@�%@��`@�/@��@��/@�Z@�dZ@�-@�`B@��;@�+@�$�@��@�^@�G�@�V@���@�(�@��m@߾w@ߍP@��H@ݲ-@��@�ƨ@�K�@�C�@�K�@��@��@�n�@���@ّh@ّh@�p�@�x�@�p�@�7L@��@�1'@ׅ@��@���@�O�@���@��@ҸR@��#@�?}@�?}@�O�@�G�@���@Ь@Ь@Гu@�b@��m@���@Ͼw@ϕ�@�K�@��y@θR@�M�@�@ͩ�@͉7@�x�@��@��/@̼j@�r�@�1'@˶F@�S�@�"�@ʰ!@�@��@ȃ@��@���@�dZ@��@�-@š�@�?}@�G�@���@ě�@��@�+@�
=@���@�o@��@�~�@�@�7L@�z�@�b@�K�@���@�M�@��@�j@��@�b@��@���@�-@�hs@���@��u@�(�@�t�@���@�V@��#@��-@�`B@��j@��D@�9X@��@���@�@�ȴ@��\@��@��-@��7@���@��u@�bN@�9X@��w@��H@�~�@��@�@�p�@�X@�/@�&�@��@���@��@�1@�ƨ@���@�;d@��@���@�V@�x�@�V@��9@�z�@�r�@�j@�Z@�Q�@�A�@�9X@�9X@�9X@�9X@���@�@��R@��+@�5?@��-@�p�@�O�@�?}@��@�z�@�ƨ@��@�|�@�S�@�o@��+@�=q@�@���@��7@�`B@�/@��@�A�@�ƨ@�+@��y@���@��R@��\@���@��@��-@��7@�%@� �@��@�|�@��@��H@��R@�J@��7@�V@��9@�A�@��m@���@��@�"�@���@�=q@��@�J@���@���@�`B@�G�@�/@�/@�&�@���@���@�z�@�z�@�j@�9X@��@���@�\)@���@���@�E�@�-@�-@�$�@��T@���@�O�@���@�C�@��}@x�E1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��#A��#A��/A���A�VA�z�A�C�A�bNA�A�1A�ȴA�ZA���A�jA��#A�ZA¬A�x�A�-A���A��7A�-A��/A���A���A��hA��A�t�A�I�A�1A��#A��A�jA�^5A�;dA��A��A��A���A�l�A��#A��hA��A��7A���A��`A���A��^A���A���A�9XA��A��RA�  A��wA�p�A�=qA�oA��yA���A��A��#A�=qA�(�A�S�A���A� �A��A��9A���A�E�A�
=A���A�G�A�1A�jA��TA�^5A�;dA�/A���A��A���A�M�A��A�7LA�`BA��mA���A��A�r�A�I�A��-A��A�-A~�jAw�TAt�yAr��Ap  Ajn�Ae+Abr�A_��A\�AX^5AV(�AO��AJ=qAGt�AE�AEG�AE%AB�!AA��A@�!A?7LA>�+A=/A=�A<��A;dZA9�-A7�A7dZA5"�A2�A01'A-�^A, �A+hsA*�A*�A)C�A((�A'7LA&��A$�A"��A"1A JA�#A"�A��A�A^5A�AJA��AI�At�A�AjAĜA1A��A�7A|�A33A�An�AA|�AhsA|�A7LAAz�A�A&�A
�A
v�A
Q�A
�DA
��A
z�A
^5A
  A
{A	�A	"�A1A�A\)A��AI�A�PA�RAA�hA
=A �+A bNA M�A V@�ȴ@�@�A�@��;@�33@�-@�?}@��@��T@��/@�%@�/@���@��@�{@��/@�u@�1@��@��@��@�\@��@�@��/@�z�@��@�V@�p�@�7L@��@�%@��`@�/@��@��/@�Z@�dZ@�-@�`B@��;@�+@�$�@��@�^@�G�@�V@���@�(�@��m@߾w@ߍP@��H@ݲ-@��@�ƨ@�K�@�C�@�K�@��@��@�n�@���@ّh@ّh@�p�@�x�@�p�@�7L@��@�1'@ׅ@��@���@�O�@���@��@ҸR@��#@�?}@�?}@�O�@�G�@���@Ь@Ь@Гu@�b@��m@���@Ͼw@ϕ�@�K�@��y@θR@�M�@�@ͩ�@͉7@�x�@��@��/@̼j@�r�@�1'@˶F@�S�@�"�@ʰ!@�@��@ȃ@��@���@�dZ@��@�-@š�@�?}@�G�@���@ě�@��@�+@�
=@���@�o@��@�~�@�@�7L@�z�@�b@�K�@���@�M�@��@�j@��@�b@��@���@�-@�hs@���@��u@�(�@�t�@���@�V@��#@��-@�`B@��j@��D@�9X@��@���@�@�ȴ@��\@��@��-@��7@���@��u@�bN@�9X@��w@��H@�~�@��@�@�p�@�X@�/@�&�@��@���@��@�1@�ƨ@���@�;d@��@���@�V@�x�@�V@��9@�z�@�r�@�j@�Z@�Q�@�A�@�9X@�9X@�9X@�9X@���@�@��R@��+@�5?@��-@�p�@�O�@�?}@��@�z�@�ƨ@��@�|�@�S�@�o@��+@�=q@�@���@��7@�`B@�/@��@�A�@�ƨ@�+@��y@���@��R@��\@���@��@��-@��7@�%@� �@��@�|�@��@��H@��R@�J@��7@�V@��9@�A�@��m@���@��@�"�@���@�=q@��@�J@���@���@�`B@�G�@�/@�/@�&�@���@���@�z�@�z�@�j@�9X@��@���@�\)@���@���@�E�@�-@�-@�$�@��T@���@�O�@���@�C�@��}@x�E1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�sB�sB�mB�sB�sB�yB�yB�yB�yB�yB�B�B�yB�yB�sB�sB�B�B�B�B�B�B�B	B	t�B
>wB
gmB
��B
�mB%BVBoB�B�B�B�B"�B$�B$�B#�B+B1'B49B5?B5?B49B49B49B5?B8RB=qBA�BK�BYB_;BgmBy�B�hB��B��B�B�mBDB�BB$�B(�B(�BD�B�B�7B�bB�oB��B��B��B��B��B�uB�bB�7Bz�BgmB_;BYBP�B>wB#�B\BB��B�HB��B�RB��B��B��B�{B�oB�\B�1Bt�BbNB^5BL�B>wB/B"�B
�B
�jB
�hB
jB
K�B
�B	�B	�DB	N�B	5?B	$�B	{B	B��B��B�B�sB�ZB�HB��B��BɺBŢBÖBBȴBɺBɺBǮBǮBɺBȴBƨBŢBƨB��B��B�
B�
B�#B��B��B�
B�B�
B��B��B��BɺB��B��B��B��BȴB��B�B��BȴBɺB��B��B�B�B�HB�`B�HB�B�B�B�B�B��B��B	B	%B	�B	(�B	-B	!�B	�B	�B	hB	�B	#�B	+B	1'B	8RB	9XB	;dB	=qB	O�B	O�B	L�B	E�B	@�B	9XB	7LB	9XB	:^B	<jB	:^B	6FB	33B	6FB	8RB	9XB	@�B	=qB	;dB	8RB	;dB	:^B	6FB	49B	2-B	2-B	7LB	<jB	?}B	E�B	I�B	O�B	R�B	W
B	e`B	l�B	m�B	k�B	jB	jB	l�B	l�B	jB	iyB	e`B	cTB	dZB	hsB	n�B	q�B	y�B	�+B	�7B	�=B	�=B	�PB	�\B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�9B	�qB	�wB	�wB	�wB	�wB	��B	��B	�}B	�}B	B	B	��B	��B	��B	B	B	B	ÖB	ÖB	B	ÖB	ÖB	ĜB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�BB	�HB	�ZB	�sB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
  B
B
B
  B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
1B
+B
1B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
VB
bB
bB
bB
hB
hB
hB
bB
\B
VB
VB
VB
VB
VB
VB
VB
VB
\B
�B
hB
'�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�sB�sB�mB�sB�sB�yB�yB�yB�yB�yB�B�B�yB�yB�sB�sB�B�B�B�B�B�B�B	B	t�B
>wB
gmB
��B
�mB%BVBoB�B�B�B�B"�B$�B$�B#�B+B1'B49B5?B5?B49B49B49B5?B8RB=qBA�BK�BYB_;BgmBy�B�hB��B��B�B�mBDB�BB$�B(�B(�BD�B�B�7B�bB�oB��B��B��B��B��B�uB�bB�7Bz�BgmB_;BYBP�B>wB#�B\BB��B�HB��B�RB��B��B��B�{B�oB�\B�1Bt�BbNB^5BL�B>wB/B"�B
�B
�jB
�hB
jB
K�B
�B	�B	�DB	N�B	5?B	$�B	{B	B��B��B�B�sB�ZB�HB��B��BɺBŢBÖBBȴBɺBɺBǮBǮBɺBȴBƨBŢBƨB��B��B�
B�
B�#B��B��B�
B�B�
B��B��B��BɺB��B��B��B��BȴB��B�B��BȴBɺB��B��B�B�B�HB�`B�HB�B�B�B�B�B��B��B	B	%B	�B	(�B	-B	!�B	�B	�B	hB	�B	#�B	+B	1'B	8RB	9XB	;dB	=qB	O�B	O�B	L�B	E�B	@�B	9XB	7LB	9XB	:^B	<jB	:^B	6FB	33B	6FB	8RB	9XB	@�B	=qB	;dB	8RB	;dB	:^B	6FB	49B	2-B	2-B	7LB	<jB	?}B	E�B	I�B	O�B	R�B	W
B	e`B	l�B	m�B	k�B	jB	jB	l�B	l�B	jB	iyB	e`B	cTB	dZB	hsB	n�B	q�B	y�B	�+B	�7B	�=B	�=B	�PB	�\B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�9B	�qB	�wB	�wB	�wB	�wB	��B	��B	�}B	�}B	B	B	��B	��B	��B	B	B	B	ÖB	ÖB	B	ÖB	ÖB	ĜB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�BB	�HB	�ZB	�sB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
  B
B
B
  B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B
	7B
1B
+B
+B
+B
1B
+B
1B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
VB
bB
bB
bB
hB
hB
hB
bB
\B
VB
VB
VB
VB
VB
VB
VB
VB
\B
�B
hB
'�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190608                              AO  ARCAADJP                                                                    20181005190608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190608  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190608  QCF$                G�O�G�O�G�O�8000            