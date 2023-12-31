CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:51Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005191651  20181005191651  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ש�䱐�1   @ש�q�/&@4&�x����c�-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C�C�C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@�CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC{�fC~  C�  C��C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C�  C��3C�  C��3C��C��C��3C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D
��Dy�DfD� D��D�fD  D� D  D� D��Dy�D  D�fD  D�fD��D� D  Dy�D  D� D  D� D��Ds3D  D�fD��D�fD  D�fD  D� D  D� D  D� D  D�fD  Dy�D fD y�D!fD!� D"fD"� D#  D#y�D#��D$� D%fD%��D&fD&�fD'  D'y�D(fD(� D)  D)� D*fD*� D*�3D+� D,  D,y�D-  D-� D.fD.��D/fD/� D0  D0� D0��D1� D2fD2�fD3fD3�fD4  D4� D5fD5�fD6  D6�fD7  D7y�D7��D8� D9fD9� D9��D:y�D;  D;y�D<  D<y�D<��D=� D>fD>� D?  D?� D?��D@y�DA  DAy�DA��DBy�DB��DCy�DC��DD� DE  DE� DFfDF� DF��DG� DH  DHy�DI  DI� DJ  DJ� DK  DK�fDL  DL�fDL��DMy�DM��DNy�DN�3DO� DPfDP� DQ  DQy�DR  DR� DS�DS� DT  DT�fDT��DU� DV  DV� DW  DWy�DW��DX� DY  DY� DZfDZ� DZ��D[y�D\fD\� D]  D]� D^fD^� D^��D_�fD`  D`y�Da  Da�fDb  Db� DcfDc� DdfDd� Dd��De� Df  Dfy�Dg  Dg� Dg��Dhy�Di  Diy�Dj  Dj�fDj��Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Do�3Dpy�Dq  Dq� Dr  Dr� DsfDs�fDtfDt� Du  Du� Du��Dv� DwfDwy�Dw� Dy�3D�7
D�v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@���A{A"{AB{Ab{A�
=A�
=A�
=A�
=A��
A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC�C!HC!HC!HC
!HC!HC!HC!HC!HC:�C:�C!HC!HC!HC!HC !HC"�C$!HC&!HC(!HC*!HC,!HC.!HC0!HC2!HC4!HC6!HC8!HC::�C<:�C>!HC@:�CB!HCD!HCF:�CH:�CJ!HCL!HCN!HCP!HCR!HCT!HCV!HCX!HCZ!HC\!HC^!HC`!HCb!HCd!HCf!HCh!HCj:�Cl!HCn!HCp!HCr!HCt!HCv!HCx!HCz�C|�C~!HC��C�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C�qC�qC�qC��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C�qC��C��C��C��C��C�qC�qC��C��C��C��C��C��C��C��C�qC�qC�qC�qC�qC��C��C��C��C��C��C�qC��C��C��C��C�qC�qC��C��C�qC�qC��C��C��C��C��C��C�qC��C��C��C��C��C�qC�qC��C��C��C�qC��C��C��C�qC��C��C��C��C��C��C��D RD �RDRD��DRD�RDRD�RDRD�RDRD�RDRD�RDRD�RDRD�RD	RD	��D
RD
�RD�D��D�D�RD�D��DRD�RDRD�RD�D��DRD��DRD��D�D�RDRD��DRD�RDRD�RD�D{�DRD��D�D��DRD��DRD�RDRD�RDRD�RDRD��DRD��D �D ��D!�D!�RD"�D"�RD#RD#��D$�D$�RD%�D%�D&�D&��D'RD'��D(�D(�RD)RD)�RD*�D*�RD*��D+�RD,RD,��D-RD-�RD.�D.�D/�D/�RD0RD0�RD1�D1�RD2�D2��D3�D3��D4RD4�RD5�D5��D6RD6��D7RD7��D8�D8�RD9�D9�RD:�D:��D;RD;��D<RD<��D=�D=�RD>�D>�RD?RD?�RD@�D@��DARDA��DB�DB��DC�DC��DD�DD�RDERDE�RDF�DF�RDG�DG�RDHRDH��DIRDI�RDJRDJ�RDKRDK��DLRDL��DM�DM��DN�DN��DN��DO�RDP�DP�RDQRDQ��DRRDR�RDSDS�RDTRDT��DU�DU�RDVRDV�RDWRDW��DX�DX�RDYRDY�RDZ�DZ�RD[�D[��D\�D\�RD]RD]�RD^�D^�RD_�D_��D`RD`��DaRDa��DbRDb�RDc�Dc�RDd�Dd�RDe�De�RDfRDf��DgRDg�RDh�Dh��DiRDi��DjRDj��Dk�Dk�RDlRDl��DmRDm�RDnRDn�RDoRDo�RDo��Dp��DqRDq�RDrRDr�RDs�Ds��Dt�Dt�RDuRDu�RDv�Dv�RDw�Dw��Dw�RDy��D�;3D�z=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�dZA�bNA�^5A�`BA�`BA�XA�&�A�A�
=A�  A�^5AάA��`A�E�A�n�AΝ�A�dZA�\)A�I�A�?}A�A�A��A���A�ĜA͟�AͶFA͡�A͇+A�\)A�VA̮A�-A˅A�-A���Aʙ�A�x�A�;dA��AɋDAɁA�C�A�oAȕ�A�S�A���A�v�A�9XA��A���AƲ-Aƣ�AƑhAƃA�ffA�33A�JA�-A�$�A�  A�hsA��/A�VA�{A�XA���A�S�A���A���A��A��!A�ĜA���A�E�A�t�A�A��TA��#A�S�A��HA��RA��A�I�A�(�A�=qA��;A���A�oA�{A��A��A��A�(�A��hA��A�-A��wA��A�hsA��A���A�7LA�v�A�(�A�M�A�?}A�n�A�  A���A��`A�bA�K�A��yA��\A���A���A��;A~�A~=qA}hsA|ȴA|JAz��AydZAwVAvbAu?}As�;Ar�ArAn�/Al��Ak+Ah��Ad��Ab��A`��A^v�A[x�AYdZAXr�AWp�AUƨAT��AS�AS&�AO�wAM�AL~�AJȴAGG�ADz�AB^5AA|�AAK�AA7LA>�yA9�-A8bA7��A7hsA6�!A6�A5/A3�wA2  A0E�A,��A*ĜA(~�A&5?A%�A%��A%�A%33A$��A!�;A ��AƨA �A/AA�A��A��A?}A�mAr�At�AbNAbNA`BA��A�yAhsAĜAZA(�A �A{AZA�PAK�AbA^5A��A��AȴA	A&�A�A$�A�#AƨA�wA��A�PA�A��AVA��A�Ap�AG�A��A �D@�r�@���@�=q@�V@�Q�@�S�@�C�@��@��@�n�@�G�@�@�1'@��@�/@��
@��@���@�@�O�@�Z@ާ�@��@���@١�@ؼj@�1'@��m@��y@�$�@պ^@ՙ�@�G�@�r�@���@���@�@��@�@�(�@̣�@̛�@��;@�l�@�n�@���@��`@�bN@��@ǶF@Ə\@��@őh@�%@�1'@î@�1@�\)@\@�7L@��@�G�@��@��
@���@�t�@�S�@�dZ@�|�@�l�@�K�@��@��F@�r�@�  @���@�l�@�o@��\@�n�@�M�@�^5@��@��@��H@���@���@�{@��`@�j@�9X@��w@�\)@�@�{@�X@���@�Z@� �@��m@���@�33@��@��R@��!@���@��\@�{@��-@�G�@��j@��D@��@�r�@�j@�Z@�Q�@�1'@���@�33@��@�z�@�(�@��;@�S�@���@�-@�@���@�`B@�G�@��@���@��D@�9X@�I�@�Q�@�(�@��m@�ȴ@�{@��^@��7@�?}@��/@��@�l�@�33@��@���@��!@�^5@��@��7@�hs@�X@�?}@�/@��@�1@�|�@�"�@��@���@��+@�E�@�@��T@���@��7@�`B@�X@�O�@�/@�V@�Ĝ@�j@�I�@� �@��m@��
@���@��@�;d@��@���@�{@��-@��7@�x�@�?}@��@��j@���@�z�@�Q�@�1'@��@�1@��m@��;@��
@��@���@�K�@��!@�M�@�-@��@���@��7@��@��j@��@��D@�Z@�A�@�(�@��F@�S�@�K�@�;d@��H@�v�@�X@��/@�z�@�j@�Q�@�9X@�b@��P@�{@�-@�~�@��+@�~�@�=q@��7@���@���@�hs@�O�@�/@��`@�z�@�j@� �@���@���@�dZ@�K�@��@���@��R@���@�V@�=q@��@���@���@��h@�x�@�p�@�X@���@��9@��u@�9X@��@}�.@m�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bNA�dZA�bNA�^5A�`BA�`BA�XA�&�A�A�
=A�  A�^5AάA��`A�E�A�n�AΝ�A�dZA�\)A�I�A�?}A�A�A��A���A�ĜA͟�AͶFA͡�A͇+A�\)A�VA̮A�-A˅A�-A���Aʙ�A�x�A�;dA��AɋDAɁA�C�A�oAȕ�A�S�A���A�v�A�9XA��A���AƲ-Aƣ�AƑhAƃA�ffA�33A�JA�-A�$�A�  A�hsA��/A�VA�{A�XA���A�S�A���A���A��A��!A�ĜA���A�E�A�t�A�A��TA��#A�S�A��HA��RA��A�I�A�(�A�=qA��;A���A�oA�{A��A��A��A�(�A��hA��A�-A��wA��A�hsA��A���A�7LA�v�A�(�A�M�A�?}A�n�A�  A���A��`A�bA�K�A��yA��\A���A���A��;A~�A~=qA}hsA|ȴA|JAz��AydZAwVAvbAu?}As�;Ar�ArAn�/Al��Ak+Ah��Ad��Ab��A`��A^v�A[x�AYdZAXr�AWp�AUƨAT��AS�AS&�AO�wAM�AL~�AJȴAGG�ADz�AB^5AA|�AAK�AA7LA>�yA9�-A8bA7��A7hsA6�!A6�A5/A3�wA2  A0E�A,��A*ĜA(~�A&5?A%�A%��A%�A%33A$��A!�;A ��AƨA �A/AA�A��A��A?}A�mAr�At�AbNAbNA`BA��A�yAhsAĜAZA(�A �A{AZA�PAK�AbA^5A��A��AȴA	A&�A�A$�A�#AƨA�wA��A�PA�A��AVA��A�Ap�AG�A��A �D@�r�@���@�=q@�V@�Q�@�S�@�C�@��@��@�n�@�G�@�@�1'@��@�/@��
@��@���@�@�O�@�Z@ާ�@��@���@١�@ؼj@�1'@��m@��y@�$�@պ^@ՙ�@�G�@�r�@���@���@�@��@�@�(�@̣�@̛�@��;@�l�@�n�@���@��`@�bN@��@ǶF@Ə\@��@őh@�%@�1'@î@�1@�\)@\@�7L@��@�G�@��@��
@���@�t�@�S�@�dZ@�|�@�l�@�K�@��@��F@�r�@�  @���@�l�@�o@��\@�n�@�M�@�^5@��@��@��H@���@���@�{@��`@�j@�9X@��w@�\)@�@�{@�X@���@�Z@� �@��m@���@�33@��@��R@��!@���@��\@�{@��-@�G�@��j@��D@��@�r�@�j@�Z@�Q�@�1'@���@�33@��@�z�@�(�@��;@�S�@���@�-@�@���@�`B@�G�@��@���@��D@�9X@�I�@�Q�@�(�@��m@�ȴ@�{@��^@��7@�?}@��/@��@�l�@�33@��@���@��!@�^5@��@��7@�hs@�X@�?}@�/@��@�1@�|�@�"�@��@���@��+@�E�@�@��T@���@��7@�`B@�X@�O�@�/@�V@�Ĝ@�j@�I�@� �@��m@��
@���@��@�;d@��@���@�{@��-@��7@�x�@�?}@��@��j@���@�z�@�Q�@�1'@��@�1@��m@��;@��
@��@���@�K�@��!@�M�@�-@��@���@��7@��@��j@��@��D@�Z@�A�@�(�@��F@�S�@�K�@�;d@��H@�v�@�X@��/@�z�@�j@�Q�@�9X@�b@��P@�{@�-@�~�@��+@�~�@�=q@��7@���@���@�hs@�O�@�/@��`@�z�@�j@� �@���@���@�dZ@�K�@��@���@��R@���@�V@�=q@��@���@���@��h@�x�@�p�@�X@���@��9@��u@�9X@��@}�.@m�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�BB	�BB	�;B	�5B	�5B	�5B	�5B	�5B	�BB	�`B	�fB	��B
\B
�B
7LB
D�B
]/B
k�B
o�B
r�B
�B
�\B
�oB
�bB
��B
�9B
ŢB
ɺB
��B
ŢB
�qB
�?B
�B
��B
��B
��B
�B
�-B
��B
�/B
�B+BhB�B)�B1'B<jBC�BG�BQ�BVB]/B`BBcTBe`BjBt�B|�B��B�B��B��B��B)�BA�BF�BG�BM�BN�BO�BL�BN�Bt�By�Bv�Bu�BiyBH�BC�BbNB<jB49B0!B@�BE�BC�BC�BA�B6FB+B"�B�BhBB�BBƨB�XB��B�VB�oB�hB�DBm�BZBF�B:^B/B �B{B
��B
��B
�FB
��B
�B
n�B
bNB
]/B
S�B
>wB
9XB
33B
-B
$�B
�B
VB	��B	��B	�B	�fB	�/B	�B	��B	�-B	��B	�{B	}�B	n�B	cTB	W
B	H�B	<jB	8RB	2-B	+B	%�B	 �B	�B	JB	B��B�B�5B��BɺBǮBƨBŢBÖBB�jB�XB�LB�?B�9B�9B�?B�3B�!B��B��B��B��B�!B�^B��B�}B�^B�LB�!B��B��B�B�3BŢB	\B	JB	B��B��B�B�HB�;B��B��B��B��B��B��B	  B	B	JB	&�B	+B	"�B	�B	VB��B�yB��BǮBĜBB��B��B��B��B�}B�jB�^B�LB�?B�?B�LB�XB�RB�3B�!B�B�B�B�B��B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�dB�}BƨB��B��B��B�B�B�5B�;B�BB�HB�TB�TB�fB�B�B�B�B�B�B�B��B�B��B��B��B��B��B��B	B	DB	bB	�B	�B	!�B	 �B	$�B	)�B	-B	1'B	49B	@�B	D�B	F�B	G�B	G�B	F�B	E�B	E�B	E�B	E�B	D�B	E�B	K�B	K�B	L�B	L�B	P�B	T�B	YB	ZB	\)B	]/B	^5B	_;B	_;B	bNB	cTB	e`B	gmB	hsB	iyB	jB	jB	jB	jB	jB	iyB	hsB	n�B	q�B	s�B	u�B	v�B	y�B	|�B	}�B	~�B	� B	� B	� B	�B	�B	�B	�B	�+B	�1B	�7B	�JB	�\B	�bB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
mB
'R222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	�BB	�BB	�;B	�5B	�5B	�5B	�5B	�5B	�BB	�`B	�fB	��B
\B
�B
7LB
D�B
]/B
k�B
o�B
r�B
�B
�\B
�oB
�bB
��B
�9B
ŢB
ɺB
��B
ŢB
�qB
�?B
�B
��B
��B
��B
�B
�-B
��B
�/B
�B+BhB�B)�B1'B<jBC�BG�BQ�BVB]/B`BBcTBe`BjBt�B|�B��B�B��B��B��B)�BA�BF�BG�BM�BN�BO�BL�BN�Bt�By�Bv�Bu�BiyBH�BC�BbNB<jB49B0!B@�BE�BC�BC�BA�B6FB+B"�B�BhBB�BBƨB�XB��B�VB�oB�hB�DBm�BZBF�B:^B/B �B{B
��B
��B
�FB
��B
�B
n�B
bNB
]/B
S�B
>wB
9XB
33B
-B
$�B
�B
VB	��B	��B	�B	�fB	�/B	�B	��B	�-B	��B	�{B	}�B	n�B	cTB	W
B	H�B	<jB	8RB	2-B	+B	%�B	 �B	�B	JB	B��B�B�5B��BɺBǮBƨBŢBÖBB�jB�XB�LB�?B�9B�9B�?B�3B�!B��B��B��B��B�!B�^B��B�}B�^B�LB�!B��B��B�B�3BŢB	\B	JB	B��B��B�B�HB�;B��B��B��B��B��B��B	  B	B	JB	&�B	+B	"�B	�B	VB��B�yB��BǮBĜBB��B��B��B��B�}B�jB�^B�LB�?B�?B�LB�XB�RB�3B�!B�B�B�B�B��B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB�dB�}BƨB��B��B��B�B�B�5B�;B�BB�HB�TB�TB�fB�B�B�B�B�B�B�B��B�B��B��B��B��B��B��B	B	DB	bB	�B	�B	!�B	 �B	$�B	)�B	-B	1'B	49B	@�B	D�B	F�B	G�B	G�B	F�B	E�B	E�B	E�B	E�B	D�B	E�B	K�B	K�B	L�B	L�B	P�B	T�B	YB	ZB	\)B	]/B	^5B	_;B	_;B	bNB	cTB	e`B	gmB	hsB	iyB	jB	jB	jB	jB	jB	iyB	hsB	n�B	q�B	s�B	u�B	v�B	y�B	|�B	}�B	~�B	� B	� B	� B	�B	�B	�B	�B	�+B	�1B	�7B	�JB	�\B	�bB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�BB	�HB	�NB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
mB
'R222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191651                              AO  ARCAADJP                                                                    20181005191651    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191651  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191651  QCF$                G�O�G�O�G�O�8000            