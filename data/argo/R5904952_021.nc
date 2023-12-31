CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:10Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190510  20181005190510  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׮���k�1   @׮���@2�
=p���c�G�z�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C��3C�  C��3C��3C�  C��3C�  C�  C��C��C��C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C��3D   D �fD  D� D  D� D  D� D  Dy�D��D� DfD�fD  D� DfD� D	  D	� D
  D
� DfD�fD  D� DfD�fD  Dy�D��Dy�D��Dy�D��Dy�D  D� D��D� D  D�fDfD� D  D� D  D� D��D� D  D�fDfD�fDfD�fDfD� D��Dy�D��D� D  D�fD   D y�D ��D!� D"  D"� D#  D#� D$fD$�fD%fD%�fD&  D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1fD1� D1��D2y�D3  D3�fD4  D4�fD5  D5� D6  D6�fD7  D7y�D8  D8� D9  D9� D:  D:� D;fD;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DF��DGy�DH  DH�fDI  DIy�DI��DJy�DJ��DKy�DL  DL� DM  DM� DM��DN� DO  DOy�DO��DPy�DP��DQy�DR  DR� DR��DS� DTfDT� DT��DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\fD\� D]  D]�fD^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDq  Dq� Dq��Dry�Dr��Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw` Dy\)D�;�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @;�@�(�@�(�A{A"{AB{Ab{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�u�C !HC!HC!HC!HC!HC
!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC!HC �C"!HC$!HC&:�C(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC:!HC<!HC>!HC@!HCB!HCD!HCF!HCH!HCJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj!HCl!HCn!HCp!HCr!HCt!HCv!HCx!HCz�C|!HC~!HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C�qC�qC�qC��C��C��C��C�qC�qC�qC�qC��C��C��C��C�qC�qC��C��C�qC��C��C��C��C�qC��C��C��C��C��D RD ��DRD�RDRD�RDRD�RDRD��D�D�RD�D��DRD�RD�D�RD	RD	�RD
RD
�RD�D��DRD�RD�D��DRD��D�D��D�D��D�D��DRD�RD�D�RDRD��D�D�RDRD�RDRD�RD�D�RDRD��D�D��D�D��D�D�RD�D��D�D�RDRD��D RD ��D!�D!�RD"RD"�RD#RD#�RD$�D$��D%�D%��D&RD&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,RD,�RD-RD-�RD.RD.�RD/RD/��D0RD0�RD1�D1�RD2�D2��D3RD3��D4RD4��D5RD5�RD6RD6��D7RD7��D8RD8�RD9RD9�RD:RD:�RD;�D;��D<RD<�RD=RD=�RD>RD>�RD?RD?�RD@RD@��DARDA�RDBRDB�RDCRDC�RDDRDD�RDERDE��DFRDF�RDG�DG��DHRDH��DIRDI��DJ�DJ��DK�DK��DLRDL�RDMRDM�RDN�DN�RDORDO��DP�DP��DQ�DQ��DRRDR�RDS�DS�RDT�DT�RDU�DU�RDVRDV�RDWRDW�RDXRDX�RDY�DY�RDZRDZ�RD[RD[�RD\�D\�RD]RD]��D^RD^��D_RD_�RD`RD`�RDaRDa�RDbRDb�RDc�Dc�RDdRDd�RDeRDe�RDfRDf��DgRDg�RDhRDh�RDiRDi�RDjRDj�RDkRDk�RDl�Dl��DmRDm�RDnRDn�RDoRDo�RDp�Dp��DqRDq�RDr�Dr��Ds�Ds�RDtRDt�RDuRDu�RDvRDv�RDwRDwhRDyd{D�?�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�r�A�v�A�z�A�z�A�p�A�n�A�x�A�v�A�v�A�t�A�"�A��A�ĜAя\A�n�A�C�A��A�{A��A��A�I�A��A��#A�p�A�=qA�C�A�jAϬA�ȴA��A���A���Aϲ-AϋDA�;dA�l�A�r�A̴9A̕�A�t�A���A˟�A�G�A�A�A�K�A�5?A�  A���Aʛ�A�A�AɸRAɗ�AɅA�I�A��Aȏ\A���AǾwA�S�A�{A��yAƼjAƕ�A�^5A�"�A���A�+A�x�A��A���A�A��wA�^5A�5?A���A�/A�VA�;dA�G�A���A��jA��A��/A��/A��A�bA�  A�&�A��+A��^A��A�A�ĜA��\A�JA���A��A��A�=qA��A��9A���A���A��-A��DA�|�A�VA��HA�G�A��/A���A�=qA�hsA{�AzjAy��Aw��ArZAoK�AlE�Afz�AbQ�A`�A\bNAZ�AVVAQ?}AO��AOp�AN�`AN�AM��AL�RAJȴAD��A@�9A@I�A?�TA?hsA=C�A;�#A;hsA:�A9�^A8��A7��A6-A4��A4v�A4=qA3��A3��A333A1�;A0��A0M�A/?}A-�A+�A+dZA*�`A*bA)33A(��A'�A&A$��A$bA"�A"bA A�A�FA&�A��An�AƨA��AQ�AĜA/A9XA��A
=AJA-Al�AĜA�7A"�A~�AA
=AE�A�A�#A
JA�AVA��A^5A��A��AVAffAQ�A �D@��@��!@�X@�bN@��;@���@�"�@�G�@��@�v�@�V@�@�h@���@���@��@��@��@�\@�E�@�7L@��;@�K�@�-@��@�|�@�
=@�!@�r�@�~�@�7L@��@� �@���@�`B@�"�@�%@� �@�\)@֗�@�n�@�5?@��@ղ-@Չ7@�`B@���@���@�j@ӍP@��@�n�@���@�x�@��/@ЋD@��;@·+@͡�@�V@̴9@̋D@��;@�;d@ʸR@ɡ�@ȣ�@� �@�K�@��y@ƸR@�E�@�@ź^@�O�@��@ģ�@��m@öF@�S�@�@��#@��-@���@���@��h@�/@�9X@��w@��y@��y@�v�@��R@�S�@�t�@��@�"�@�X@��9@��@�ƨ@���@���@��P@�-@��h@���@�M�@�9X@�9X@��@�1@���@�z�@�Ĝ@��@�(�@���@�-@�7L@�Q�@�  @��;@�|�@�"�@���@�v�@��^@�bN@�A�@�(�@�9X@��m@��m@�ƨ@�dZ@��F@��@��!@�n�@�@�G�@�&�@��9@�  @��F@��@��@��R@��!@���@�n�@�x�@�%@���@��@�9X@��m@���@�l�@�"�@��H@��R@�E�@�X@���@�z�@�Q�@�(�@��@�dZ@�C�@�33@��H@���@�5?@�$�@�J@��T@���@���@�@��h@��`@���@�9X@��
@��P@�S�@���@�ff@�E�@�$�@�{@���@��7@��u@�I�@�I�@�A�@���@��w@�K�@���@���@�@�%@���@��D@�Z@�(�@� �@�9X@�9X@�b@�b@�1@�  @���@�  @�  @�1@�1@�b@�1@���@��@��P@��H@��@��7@�hs@�G�@�7L@�&�@�V@���@�Z@�1@��m@�ƨ@�33@���@�V@�5?@�{@��@�@���@�X@���@���@�z�@�9X@�A�@�9X@�(�@�b@��;@��F@��P@�t�@�@�^5@�$�@�J@�@��T@�@���@��7@�p�@�`B@�hs@�hs@�`B@�/@���@��@�Z@�I�@�I�@�bN@�A�@���@��@��P@�;d@���@|��@l�K111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�r�A�v�A�z�A�z�A�p�A�n�A�x�A�v�A�v�A�t�A�"�A��A�ĜAя\A�n�A�C�A��A�{A��A��A�I�A��A��#A�p�A�=qA�C�A�jAϬA�ȴA��A���A���Aϲ-AϋDA�;dA�l�A�r�A̴9A̕�A�t�A���A˟�A�G�A�A�A�K�A�5?A�  A���Aʛ�A�A�AɸRAɗ�AɅA�I�A��Aȏ\A���AǾwA�S�A�{A��yAƼjAƕ�A�^5A�"�A���A�+A�x�A��A���A�A��wA�^5A�5?A���A�/A�VA�;dA�G�A���A��jA��A��/A��/A��A�bA�  A�&�A��+A��^A��A�A�ĜA��\A�JA���A��A��A�=qA��A��9A���A���A��-A��DA�|�A�VA��HA�G�A��/A���A�=qA�hsA{�AzjAy��Aw��ArZAoK�AlE�Afz�AbQ�A`�A\bNAZ�AVVAQ?}AO��AOp�AN�`AN�AM��AL�RAJȴAD��A@�9A@I�A?�TA?hsA=C�A;�#A;hsA:�A9�^A8��A7��A6-A4��A4v�A4=qA3��A3��A333A1�;A0��A0M�A/?}A-�A+�A+dZA*�`A*bA)33A(��A'�A&A$��A$bA"�A"bA A�A�FA&�A��An�AƨA��AQ�AĜA/A9XA��A
=AJA-Al�AĜA�7A"�A~�AA
=AE�A�A�#A
JA�AVA��A^5A��A��AVAffAQ�A �D@��@��!@�X@�bN@��;@���@�"�@�G�@��@�v�@�V@�@�h@���@���@��@��@��@�\@�E�@�7L@��;@�K�@�-@��@�|�@�
=@�!@�r�@�~�@�7L@��@� �@���@�`B@�"�@�%@� �@�\)@֗�@�n�@�5?@��@ղ-@Չ7@�`B@���@���@�j@ӍP@��@�n�@���@�x�@��/@ЋD@��;@·+@͡�@�V@̴9@̋D@��;@�;d@ʸR@ɡ�@ȣ�@� �@�K�@��y@ƸR@�E�@�@ź^@�O�@��@ģ�@��m@öF@�S�@�@��#@��-@���@���@��h@�/@�9X@��w@��y@��y@�v�@��R@�S�@�t�@��@�"�@�X@��9@��@�ƨ@���@���@��P@�-@��h@���@�M�@�9X@�9X@��@�1@���@�z�@�Ĝ@��@�(�@���@�-@�7L@�Q�@�  @��;@�|�@�"�@���@�v�@��^@�bN@�A�@�(�@�9X@��m@��m@�ƨ@�dZ@��F@��@��!@�n�@�@�G�@�&�@��9@�  @��F@��@��@��R@��!@���@�n�@�x�@�%@���@��@�9X@��m@���@�l�@�"�@��H@��R@�E�@�X@���@�z�@�Q�@�(�@��@�dZ@�C�@�33@��H@���@�5?@�$�@�J@��T@���@���@�@��h@��`@���@�9X@��
@��P@�S�@���@�ff@�E�@�$�@�{@���@��7@��u@�I�@�I�@�A�@���@��w@�K�@���@���@�@�%@���@��D@�Z@�(�@� �@�9X@�9X@�b@�b@�1@�  @���@�  @�  @�1@�1@�b@�1@���@��@��P@��H@��@��7@�hs@�G�@�7L@�&�@�V@���@�Z@�1@��m@�ƨ@�33@���@�V@�5?@�{@��@�@���@�X@���@���@�z�@�9X@�A�@�9X@�(�@�b@��;@��F@��P@�t�@�@�^5@�$�@�J@�@��T@�@���@��7@�p�@�`B@�hs@�hs@�`B@�/@���@��@�Z@�I�@�I�@�bN@�A�@���@��@��P@�;d@���@|��@l�K111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�)B	�)B	�)B	�)B	�)B	�#B	�#B	�#B	�#B	�#B	�#B	�B	��B	��B	��B	��B	��B	ƨB	��B	�B	�B
uB
�B
&�B
$�B
#�B
'�B
9XB
W
B
aHB
ffB
hsB
jB
jB
jB
cTB
L�B
N�B
�DB
�B
ƨB
ȴB
ǮB
�
B
�BPBuBuB{BoBoB{B{B�B�B$�B2-BJ�BXBl�Bt�By�B}�B~�B�B� B�B��B�!BɺB��B��B�B  B�B5?B9XB@�BP�BO�BN�BM�BL�BG�BD�B@�B<jB=qB?}B@�B:^B2-B%�B�BhBB�sBB��BS�BoB
�ZB
ŢB
��B
�}B
�dB
�RB
��B
hsB
]/B
ZB
K�B
{B	�)B	��B	��B	��B	�oB	r�B	[#B	\)B	7LB	 �B	�B		7B	B��B�B�sB�mB�fB�`B�TB�;B�BƨB�}B�}B�wB�jB�qB�qB�qB�qB�jB�qB�qBBǮB��B��B��B��B��B�5B�fB�B�B�sB�ZB�NB�NB�;B�5B�/B�B�
B�B�B�B�;B�TB�TB�mB�B�B�B�TB�/B�B�sB�fB�B�B�B�B�B�B�B�B�B�B�B�B�B�fB�B��B�
B�5B�B��B��BɺBǮB�qB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�B�B�!B�'B�9B�LB�RB�^B�jB�jB�jB�jB�jB�jB�jB�jB�jB�qB�wB�}B��B��B��BBÖBĜB��B��B��B��B��B��B��B��B�
B�B�)B�5B�BB�HB�`B�fB�sB�B�B�B�B�B��B��B��B��B	  B	B	B	B	DB	
=B	1B	\B	uB	�B	$�B	'�B	(�B	)�B	(�B	%�B	%�B	&�B	+B	,B	.B	.B	1'B	7LB	?}B	?}B	E�B	J�B	L�B	N�B	S�B	W
B	W
B	XB	ZB	ZB	[#B	[#B	[#B	ZB	\)B	]/B	^5B	^5B	aHB	cTB	cTB	dZB	k�B	p�B	r�B	u�B	t�B	w�B	z�B	z�B	y�B	y�B	z�B	|�B	�B	�B	�B	�%B	�DB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�9B	�?B	�FB	�RB	�dB	�jB	�qB	�}B	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
%B
1B
	7B

=B
DB
JB
JB
VB
bB
WB
*e222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�)B	�)B	�)B	�)B	�)B	�#B	�#B	�#B	�#B	�#B	�#B	�B	��B	��B	��B	��B	��B	ƨB	��B	�B	�B
uB
�B
&�B
$�B
#�B
'�B
9XB
W
B
aHB
ffB
hsB
jB
jB
jB
cTB
L�B
N�B
�DB
�B
ƨB
ȴB
ǮB
�
B
�BPBuBuB{BoBoB{B{B�B�B$�B2-BJ�BXBl�Bt�By�B}�B~�B�B� B�B��B�!BɺB��B��B�B  B�B5?B9XB@�BP�BO�BN�BM�BL�BG�BD�B@�B<jB=qB?}B@�B:^B2-B%�B�BhBB�sBB��BS�BoB
�ZB
ŢB
��B
�}B
�dB
�RB
��B
hsB
]/B
ZB
K�B
{B	�)B	��B	��B	��B	�oB	r�B	[#B	\)B	7LB	 �B	�B		7B	B��B�B�sB�mB�fB�`B�TB�;B�BƨB�}B�}B�wB�jB�qB�qB�qB�qB�jB�qB�qBBǮB��B��B��B��B��B�5B�fB�B�B�sB�ZB�NB�NB�;B�5B�/B�B�
B�B�B�B�;B�TB�TB�mB�B�B�B�TB�/B�B�sB�fB�B�B�B�B�B�B�B�B�B�B�B�B�B�fB�B��B�
B�5B�B��B��BɺBǮB�qB�3B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�B�B�!B�'B�9B�LB�RB�^B�jB�jB�jB�jB�jB�jB�jB�jB�jB�qB�wB�}B��B��B��BBÖBĜB��B��B��B��B��B��B��B��B�
B�B�)B�5B�BB�HB�`B�fB�sB�B�B�B�B�B��B��B��B��B	  B	B	B	B	DB	
=B	1B	\B	uB	�B	$�B	'�B	(�B	)�B	(�B	%�B	%�B	&�B	+B	,B	.B	.B	1'B	7LB	?}B	?}B	E�B	J�B	L�B	N�B	S�B	W
B	W
B	XB	ZB	ZB	[#B	[#B	[#B	ZB	\)B	]/B	^5B	^5B	aHB	cTB	cTB	dZB	k�B	p�B	r�B	u�B	t�B	w�B	z�B	z�B	y�B	y�B	z�B	|�B	�B	�B	�B	�%B	�DB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�9B	�?B	�FB	�RB	�dB	�jB	�qB	�}B	��B	ÖB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
%B
1B
	7B

=B
DB
JB
JB
VB
bB
WB
*e222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190510                              AO  ARCAADJP                                                                    20181005190510    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190510  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190510  QCF$                G�O�G�O�G�O�8000            