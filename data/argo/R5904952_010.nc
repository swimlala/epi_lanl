CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:07Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190507  20181005190507  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               
A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ף�3�`H1   @ף컻��@3@     �c��G�{1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      
A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��C��C�  C�  C�  C��C��C��C��C��C��C��C��C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  Dy�D  D� D��D� DfD� D  D� D  D� D  D� D	  D	� D
fD
�fD  D� D  D� D��Dy�D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D��D� D  D� D��Dy�D  D� DfD� D  D� D  D� DfD� D   D y�D!  D!�fD"fD"�fD#fD#�fD$fD$� D$��D%� D&fD&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.fD.�fD/fD/�fD0  D0y�D1fD1� D1��D2y�D3  D3�fD4  D4� D5  D5�fD6  D6� D7  D7y�D7��D8y�D8��D9y�D:  D:� D:��D;y�D<  D<� D=  D=�fD>fD>�fD?  D?� D@  D@� DA  DAy�DA��DB� DCfDC�fDDfDD� DD��DE� DFfDF� DF��DG� DH  DHy�DH��DI� DJ  DJy�DK  DK� DL  DL�fDM  DMy�DN  DN� DN��DO� DPfDP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DT��DU� DVfDV�fDV��DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]fD]� D]��D^� D_  D_� D`  D`� D`��Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� DifDi� Di��Dj� Dk  Dky�Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw�fDw��Dy�RD�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @HQ�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC�C!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC !HC"!HC$�C&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP:�CR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz:�C|!HC~!HC��C�qC�qC��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC�qC�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C�qC�qC��C��C��C�qC�qC�qC�qC�qC�qC�qC�qC��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��D RD �RDRD�RDRD��DRD�RD�D�RD�D�RDRD�RDRD�RDRD�RD	RD	�RD
�D
��DRD�RDRD�RD�D��DRD�RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD��DRD�RD�D�RDRD�RD�D��DRD�RD�D�RDRD�RDRD�RD�D�RD RD ��D!RD!��D"�D"��D#�D#��D$�D$�RD%�D%�RD&�D&��D'RD'�RD(RD(�RD)RD)�RD*RD*�RD+RD+��D,RD,�RD-RD-�RD.�D.��D/�D/��D0RD0��D1�D1�RD2�D2��D3RD3��D4RD4�RD5RD5��D6RD6�RD7RD7��D8�D8��D9�D9��D:RD:�RD;�D;��D<RD<�RD=RD=��D>�D>��D?RD?�RD@RD@�RDARDA��DB�DB�RDC�DC��DD�DD�RDE�DE�RDF�DF�RDG�DG�RDHRDH��DI�DI�RDJRDJ��DKRDK�RDLRDL��DMRDM��DNRDN�RDO�DO�RDP�DP�RDQRDQ�RDRRDR��DSRDS�RDTRDT�RDU�DU�RDV�DV��DW�DW��DXRDX�RDYRDY�RDZRDZ�RD[RD[�RD\RD\��D]�D]�RD^�D^�RD_RD_�RD`RD`�RDa�Da�RDb�Db�RDcRDc�RDdRDd�RDeRDe�RDfRDf��DgRDg�RDhRDh�RDi�Di�RDj�Dj�RDkRDk��DlRDl�RDmRDm�RDn�Dn�RDoRDo�RDpRDp�RDqRDq�RDrRDr�RDsRDs�RDtRDt�RDu�Du�RDvRDv�RDwRDw��Dx�Dy��D�=�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�bNA�dZA�bNA�bNA�hsA�ffA�ffA�hsA�hsA�hsA�ZA�K�A�C�A��A��#A���A�ĜAͺ^Aͣ�A͓uA��A��A˼jA˰!A˝�A�`BA���A���A��`A�1A�  A���A���AʼjA�Aʰ!Aʉ7A�r�A�hsA�^5A�E�A�7LA�JA���A���AɸRA�t�A��AȾwAȇ+A���A���A���A���Aǰ!A�O�A�9XA�(�A�A��A��A��A�`BA�9XA�
=Aŏ\A��#A�M�AîA��A��A��yA��A��!A�|�A�^5A�M�A�;dA� �A�  A���A��jA�l�A��uA��wA��!A��#A��A��A�  A�(�A�{A��A�oA��A���A��/A��wA��hA�ffA�hsA�ZA�XA���A�M�A��A��A�1'A�t�A��yA�7LA��yA�G�A�`BA��
A��;A��!A�ĜA��yA|��Ax��At��ApĜAiG�Ac��A`��A^A[��AZI�AY��AXM�AT�AR1'AK��AHQ�AFjAD�\A@��A>�!A;p�A:I�A9p�A8ȴA7��A5�
A4��A3�A2=qA2bA1��A1`BA/�#A.r�A,z�A+C�A)"�A'�TA'�A&�+A&�A%�7A%XA$A�A"�+A!��A ��A bA��A/A�yA�hAn�A�AXAp�A+A�AM�A�AG�A�!A��AZA��A��Ap�A��AI�A  A
=qA	�-A	dZAȴAVAv�AA�PA�yA�TAv�@�K�@�@ܬ@�S�@�J@�C�@�$�@��@�J@��@�l�@��T@��@���@�"�@��@Χ�@�E�@��@��#@�x�@�z�@��;@˝�@˅@˅@��
@���@� �@�(�@�(�@�b@˶F@�33@���@ʇ+@�@ȼj@��m@�Q�@�@��@š�@Ĵ9@Ĵ9@�I�@�l�@��@�n�@��@�?}@��/@��@��y@���@�/@��j@�bN@�+@��@��@���@��-@���@�Ĝ@�1'@�ƨ@�~�@���@�?}@�V@���@�Ĝ@���@��D@��@��@��@�z�@�r�@�bN@�Z@�9X@�1@�|�@��@�Ĝ@��D@�r�@�z�@���@�/@��u@���@���@�;d@�@��@��+@�^5@�x�@��@���@���@�/@�`B@�O�@��@�I�@� �@�(�@���@���@��@��+@��@�J@��@��#@�7L@���@�Z@�I�@�I�@�j@��@��u@���@�Ĝ@�Q�@� �@� �@��w@�\)@�;d@��@��m@��@���@���@�M�@�E�@��@��@���@�-@��!@���@��@��#@���@��7@�G�@�X@��@�%@���@�bN@�1@��@��;@��@�ȴ@�^5@�5?@�J@���@�hs@�O�@�/@���@��j@���@��D@�j@� �@���@�dZ@�C�@�;d@�+@���@�ff@�-@���@�hs@�%@���@��D@��@�Q�@�1@��w@�S�@�o@�ȴ@�E�@�?}@��@���@���@�S�@�S�@�dZ@�S�@�dZ@�S�@��y@��+@��+@�J@�hs@���@�Ĝ@�Z@��w@�K�@�ff@��@���@�7L@�/@�&�@��`@��u@��w@�
=@��+@�^5@�E�@�-@��@�p�@�/@���@��j@���@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�Z@�A�@� �@�1@��;@�;d@��@���@���@�E�@�$�@�$�@���@��7@�7L@���@�Ĝ@��@�Q�@�9X@�9X@�9X@��@��;@�ƨ@��F@��P@�>�@v&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�bNA�dZA�bNA�bNA�hsA�ffA�ffA�hsA�hsA�hsA�ZA�K�A�C�A��A��#A���A�ĜAͺ^Aͣ�A͓uA��A��A˼jA˰!A˝�A�`BA���A���A��`A�1A�  A���A���AʼjA�Aʰ!Aʉ7A�r�A�hsA�^5A�E�A�7LA�JA���A���AɸRA�t�A��AȾwAȇ+A���A���A���A���Aǰ!A�O�A�9XA�(�A�A��A��A��A�`BA�9XA�
=Aŏ\A��#A�M�AîA��A��A��yA��A��!A�|�A�^5A�M�A�;dA� �A�  A���A��jA�l�A��uA��wA��!A��#A��A��A�  A�(�A�{A��A�oA��A���A��/A��wA��hA�ffA�hsA�ZA�XA���A�M�A��A��A�1'A�t�A��yA�7LA��yA�G�A�`BA��
A��;A��!A�ĜA��yA|��Ax��At��ApĜAiG�Ac��A`��A^A[��AZI�AY��AXM�AT�AR1'AK��AHQ�AFjAD�\A@��A>�!A;p�A:I�A9p�A8ȴA7��A5�
A4��A3�A2=qA2bA1��A1`BA/�#A.r�A,z�A+C�A)"�A'�TA'�A&�+A&�A%�7A%XA$A�A"�+A!��A ��A bA��A/A�yA�hAn�A�AXAp�A+A�AM�A�AG�A�!A��AZA��A��Ap�A��AI�A  A
=qA	�-A	dZAȴAVAv�AA�PA�yA�TAv�@�K�@�@ܬ@�S�@�J@�C�@�$�@��@�J@��@�l�@��T@��@���@�"�@��@Χ�@�E�@��@��#@�x�@�z�@��;@˝�@˅@˅@��
@���@� �@�(�@�(�@�b@˶F@�33@���@ʇ+@�@ȼj@��m@�Q�@�@��@š�@Ĵ9@Ĵ9@�I�@�l�@��@�n�@��@�?}@��/@��@��y@���@�/@��j@�bN@�+@��@��@���@��-@���@�Ĝ@�1'@�ƨ@�~�@���@�?}@�V@���@�Ĝ@���@��D@��@��@��@�z�@�r�@�bN@�Z@�9X@�1@�|�@��@�Ĝ@��D@�r�@�z�@���@�/@��u@���@���@�;d@�@��@��+@�^5@�x�@��@���@���@�/@�`B@�O�@��@�I�@� �@�(�@���@���@��@��+@��@�J@��@��#@�7L@���@�Z@�I�@�I�@�j@��@��u@���@�Ĝ@�Q�@� �@� �@��w@�\)@�;d@��@��m@��@���@���@�M�@�E�@��@��@���@�-@��!@���@��@��#@���@��7@�G�@�X@��@�%@���@�bN@�1@��@��;@��@�ȴ@�^5@�5?@�J@���@�hs@�O�@�/@���@��j@���@��D@�j@� �@���@�dZ@�C�@�;d@�+@���@�ff@�-@���@�hs@�%@���@��D@��@�Q�@�1@��w@�S�@�o@�ȴ@�E�@�?}@��@���@���@�S�@�S�@�dZ@�S�@�dZ@�S�@��y@��+@��+@�J@�hs@���@�Ĝ@�Z@��w@�K�@�ff@��@���@�7L@�/@�&�@��`@��u@��w@�
=@��+@�^5@�E�@�-@��@�p�@�/@���@��j@���@�z�@�z�@�z�@�r�@�r�@�r�@�r�@�j@�Z@�A�@� �@�1@��;@�;d@��@���@���@�E�@�$�@�$�@���@��7@�7L@���@�Ĝ@��@�Q�@�9X@�9X@�9X@��@��;@�ƨ@��F@��P@�>�@v&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
VB
VB
VB
VB
\B
VB
VB
VB
VB
VB
PB
JB
DB

=B
+B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B
1B
49B
H�B
hsB
m�B
iyB
hsB
jB
|�B
}�B
�B
�DB
�VB
�uB
��B
�3B
�3B
�wB
��B
�`B
�fB
�BB
�B
��B)�BK�BM�BN�BS�B]/BgmB�PB��B��B�B�LB��B�
B�HB�NB�#B�TB�sB�NB�fBoB1'B5?B7LB8RB8RB8RB8RB7LB5?BYBaHBjBt�Bu�Bo�BjBgmB]/BT�BH�B+B�B�B�BJB��B��B��B�)B�wB�Bl�BE�B�BhBDB  B
�#B
�?B
�B
��B
��B
�+B
�B
r�B
cTB
J�B
)�B

=B	�B	��B	��B	x�B	iyB	W
B	E�B	<jB	6FB	+B	�B	%B�B�/B�B��B�}B�FB�B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�hB��B��B�hB�oB��B�uB��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�?B�XBƨB��B��B�B�;B�NB�TB�ZB�`B�`B�fB�fB�fB�fB�mB�B��B��B��B��B��B��B��B��B��B��B	B	JB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	(�B	)�B	+B	,B	.B	0!B	49B	5?B	5?B	6FB	6FB	7LB	8RB	8RB	:^B	<jB	>wB	<jB	8RB	8RB	8RB	8RB	<jB	E�B	D�B	E�B	J�B	L�B	M�B	M�B	Q�B	R�B	ZB	]/B	_;B	aHB	dZB	ffB	iyB	p�B	t�B	u�B	y�B	}�B	�B	�B	�B	�+B	�7B	�=B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�FB	�RB	�RB	�?B	�?B	�LB	�LB	�jB	�}B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�5B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�sB	�sB	�mB	�sB	�sB	�sB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
PB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
aB
%�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
VB
VB
VB
VB
\B
VB
VB
VB
VB
VB
PB
JB
DB

=B
+B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B
1B
49B
H�B
hsB
m�B
iyB
hsB
jB
|�B
}�B
�B
�DB
�VB
�uB
��B
�3B
�3B
�wB
��B
�`B
�fB
�BB
�B
��B)�BK�BM�BN�BS�B]/BgmB�PB��B��B�B�LB��B�
B�HB�NB�#B�TB�sB�NB�fBoB1'B5?B7LB8RB8RB8RB8RB7LB5?BYBaHBjBt�Bu�Bo�BjBgmB]/BT�BH�B+B�B�B�BJB��B��B��B�)B�wB�Bl�BE�B�BhBDB  B
�#B
�?B
�B
��B
��B
�+B
�B
r�B
cTB
J�B
)�B

=B	�B	��B	��B	x�B	iyB	W
B	E�B	<jB	6FB	+B	�B	%B�B�/B�B��B�}B�FB�B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�hB��B��B�hB�oB��B�uB��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�3B�?B�XBƨB��B��B�B�;B�NB�TB�ZB�`B�`B�fB�fB�fB�fB�mB�B��B��B��B��B��B��B��B��B��B��B	B	JB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	(�B	)�B	+B	,B	.B	0!B	49B	5?B	5?B	6FB	6FB	7LB	8RB	8RB	:^B	<jB	>wB	<jB	8RB	8RB	8RB	8RB	<jB	E�B	D�B	E�B	J�B	L�B	M�B	M�B	Q�B	R�B	ZB	]/B	_;B	aHB	dZB	ffB	iyB	p�B	t�B	u�B	y�B	}�B	�B	�B	�B	�+B	�7B	�=B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�FB	�RB	�RB	�?B	�?B	�LB	�LB	�jB	�}B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�5B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�sB	�sB	�mB	�sB	�sB	�sB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
PB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
aB
%�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190507                              AO  ARCAADJP                                                                    20181005190507    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190507  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190507  QCF$                G�O�G�O�G�O�8000            