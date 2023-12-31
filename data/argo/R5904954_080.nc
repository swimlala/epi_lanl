CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:07Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191707  20181005191707  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               PA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d���z1   @��eK�%\@5��$��d,�C��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      PA   A   A   @9��@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   BffB��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C�C�C  C  C  C  C�fC  C  C �C"�C$  C&  C(  C)�fC+�fC-�fC0  C2  C4  C6  C7�fC9�fC;�fC>  C@  CB  CD  CE�fCH  CJ  CK�fCN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cc�fCf  Ch�Cj  Cl  Cn�Cp  Cq�fCt  Cv  Cw�fCz  C|�C~  C�fC��3C�  C�  C��C�  C�  C�  C��3C��3C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C��3C��C��C��C��3C�  C�  C��3C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C��C��3C�  C��3C�  C��C��3C�  C��C��3C�  C�  C��C�  C��C��3C�  C�  C��C�  C��C��C�  C�  C�  C��fC��fC�  C��C�  C��C��C�  C��3C��C��C��C��C�  C��3C�  C��C�  C�  C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C�  D   D y�D ��D� D  D� D  D�fD  Dy�D  D�fDfDy�D��Ds3D��Dy�D�3D	� D	��D
�fDfD� D�3Ds3D  D� D��Dy�D  D� D  D�fD  D� D  D� DfD� D��D� D��Ds3D  D� D  D�fD  D� D��Dy�D  D�fD  Dy�D  D�fD  D� DfD�fD  D� D fD � D ��D!y�D!��D"s3D#  D#� D$  D$�fD%  D%� D&  D&� D'fD'�fD(  D(y�D(��D)� D*fD*� D+  D+�fD,fD,� D-  D-� D.fD.�fD/  D/� D0fD0�fD1fD1�fD2  D2y�D3  D3�fD4  D4� D5  D5y�D6  D6�fD7fD7�fD8fD8� D8�3D9y�D9��D:� D;  D;�fD<fD<�fD=  D=� D>  D>� D?fD?�fD?��D@y�D@��DAy�DB  DB�fDCfDC� DD  DD�fDE  DE�fDF  DF� DGfDG� DHfDH� DH��DIy�DJ  DJ� DJ��DKy�DK�3DLy�DL��DM� DN  DNy�DN��DO� DP  DPy�DQ  DQ� DQ��DR�fDSfDS� DT  DT� DUfDU� DU��DVy�DV��DWy�DX  DX� DY  DY� DZfDZ� DZ�3D[� D\  D\� D]fD]�fD^fD^� D_  D_� D_��D`�fDa  Da� DbfDby�Db��Dcy�Dc��Dd� DefDe� De��Dfy�DgfDg�fDh  Dh� Dh��Diy�Dj  Dj� Dk  Dk�fDl  Dl� DmfDm� Dm��Dny�DofDo��DpfDp� DqfDq� Dq��Dr� Dr��Ds�fDtfDt�fDufDu�fDvfDv�fDwfDw�fDw� Dy�RD�ND�pR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@�(�@�(�A{A"{AB{Ac�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C !HC�C!HC!HC!HC
!HC!HC:�C:�C!HC!HC!HC!HC�C!HC!HC :�C":�C$!HC&!HC(!HC*�C,�C.�C0!HC2!HC4!HC6!HC8�C:�C<�C>!HC@!HCB!HCD!HCF�CH!HCJ!HCL�CN!HCP!HCR!HCT!HCV:�CX:�CZ!HC\!HC^!HC`!HCb!HCd�Cf!HCh:�Cj!HCl!HCn:�Cp!HCr�Ct!HCv!HCx�Cz!HC|:�C~!HC��C��C��C��C�qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C��C��C��C�qC��C��C��C��C�qC�qC�qC��C��C��C��C�qC��C��C��C��C��C�qC��C��C�qC��C��C�qC��C��C��C��C�qC��C��C�qC��C��C��C�qC��C�qC��C��C��C�qC��C�qC�qC��C��C��C��
C��
C��C�qC��C�*>C�qC��C��C�qC�qC�qC�qC��C��C��C�qC��C��C�qC�qC�qC��C��C�qC��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C��C��C�qC��C��C��C��C��D RD ��D�D�RDRD�RDRD��DRD��DRD��D�D��D�D{�D�D��D��D	�RD
�D
��D�D�RD��D{�DRD�RD�D��DRD�RDRD��DRD�RDRD�RD�D�RD�D�RD�D{�DRD�RDRD��DRD�RD�D��DRD��DRD��DRD��DRD�RD�D��DRD�RD �D �RD!�D!��D"�D"{�D#RD#�RD$RD$��D%RD%�RD&RD&�RD'�D'��D(RD(��D)�D)�RD*�D*�RD+RD+��D,�D,�RD-RD-�RD.�D.��D/RD/�RD0�D0��D1�D1��D2RD2��D3RD3��D4RD4�RD5RD5��D6RD6��D7�D7��D8�D8�RD8��D9��D:�D:�RD;RD;��D<�D<��D=RD=�RD>RD>�RD?�D?��D@�D@��DA�DA��DBRDB��DC�DC�RDDRDD��DERDE��DFRDF�RDG�DG�RDH�DH�RDI�DI��DJRDJ�RDK�DK��DK��DL��DM�DM�RDNRDN��DO�DO�RDPRDP��DQRDQ�RDR�DR��DS�DS�RDTRDT�RDU�DU�RDV�DV��DW�DW��DXRDX�RDYRDY�RDZ�DZ�RDZ��D[�RD\RD\�RD]�D]��D^�D^�RD_RD_�RD`�D`��DaRDa�RDb�Db��Dc�Dc��Dd�Dd�RDe�De�RDf�Df��Dg�Dg��DhRDh�RDi�Di��DjRDj�RDkRDk��DlRDl�RDm�Dm�RDn�Dn��Do�Do�Dp�Dp�RDq�Dq�RDr�Dr�RDs�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�RDy��D�R=D�t{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A۴9A۲-A۰!AۮA۲-A۶FA۬AۮA۴9A۶FAۣ�A�ZA�$�A��A���Aڕ�A�-A��A��
Aҧ�A���A�+A���A�XA�{A�|�A�z�A��A�ffA�C�A�hsAŧ�A�ƨA�ZA��TA�ZA§�A���A��RA�~�A�VA���A��A�v�A�
=A���A��A�$�A��wA���A�9XA���A�VA�VA�%A�ȴA�1A��
A��A�/A�ffA��^A���A��^A�oA���A���A�`BA��#A�A�A�-A���A��HA�I�A���A�&�A�/A���A�1'A���A��jA�l�A���A���A�A�A�t�A���A�33A�~�A�ƨA�r�A�n�A~�HA}33Ay��Ax-AvA�At�As7LAqG�Ap�An�AjbNAh^5Ag�FAf�DAeAeXAd�AcƨAb9XAa
=A`E�A\��A[G�AZQ�AY�AY�-AX�AWt�AV�DAUhsASAQ�^AOAO/AM��AJ�9AI�AG��AEG�AD1'AC��ACp�AA�A@VA?��A=VA;`BA:�HA:�+A9|�A9�A7+A4��A2�A1&�A.�yA-�A,9XA*�uA'
=A%�A%ƨA%G�A$1'A!A �A?}A��A5?A�A+A�At�A�AXA�DAhsA�DAbA��AdZA/AȴAE�A��A�`AK�A�\A �AƨAp�AXAG�A;dA�A��A��A�uA$�A��A�TA��A��A
�yA	�#AffA�^A�yA5?A��A��A�^A�\A-A1A��A�TA��A`BA%@��@��^@�V@�+@��R@�b@��/@�j@��T@��`@蛦@��@�@���@�1@�v�@�V@��@��`@�9X@ޏ\@�S�@�J@أ�@׾w@�
=@�^5@�7L@�C�@�n�@���@��`@�dZ@Ώ\@�hs@�|�@�E�@ȃ@�ƨ@�K�@�~�@�@���@�b@�@�hs@� �@��w@�n�@��7@�%@�b@�^5@�V@��!@��;@��@��w@��+@��#@���@�
=@��h@��@��@�9X@��
@�ƨ@�o@��T@��@��@�ȴ@���@���@�ff@��@�v�@�^5@��@�V@���@��/@�K�@��j@��@���@�(�@��w@�dZ@�+@��H@���@��-@�G�@�{@�^5@�V@��@�&�@���@�I�@�  @�"�@�%@��^@�5?@�=q@�x�@�&�@�&�@�1'@��@���@���@��@��w@��\@��@�(�@�j@��@�l�@�o@���@���@�@�x�@��u@�1'@��w@�\)@�o@�@��H@���@���@��@�33@�l�@�l�@��@�l�@�C�@�S�@�S�@�
=@��R@�-@�=q@�{@�x�@�Ĝ@��@��u@��@��@�o@��+@�M�@�5?@��@��^@��7@�G�@��j@�bN@��@�\)@�"�@�
=@��@���@���@�~�@�=q@���@���@���@���@���@��^@�@���@�?}@�%@���@��D@��m@���@�l�@�"�@���@�~�@�M�@�E�@�E�@�J@��h@�p�@�`B@�O�@�/@���@���@�j@�I�@�9X@��@��;@���@��@�+@���@���@��T@��-@�p�@�7L@���@�bN@�  @��;@�1@�9X@� �@�1@��@���@��@���@�J@���@���@�`B@�?}@�%@���@�z�@�1@�ƨ@���@�t�@�K�@��@���@���@��!@�^5@��@�%@��j@��@�bN@��m@��@�33@�@��H@�ȴ@��!@���@�n�@�J@��@�X@�O�@�O�@��@��@��/@��/@���@�Z@�b@���@��
@��w@���@�C�@�@��R@��+@�ff@�:*@t�@a�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A۴9A۲-A۰!AۮA۲-A۶FA۬AۮA۴9A۶FAۣ�A�ZA�$�A��A���Aڕ�A�-A��A��
Aҧ�A���A�+A���A�XA�{A�|�A�z�A��A�ffA�C�A�hsAŧ�A�ƨA�ZA��TA�ZA§�A���A��RA�~�A�VA���A��A�v�A�
=A���A��A�$�A��wA���A�9XA���A�VA�VA�%A�ȴA�1A��
A��A�/A�ffA��^A���A��^A�oA���A���A�`BA��#A�A�A�-A���A��HA�I�A���A�&�A�/A���A�1'A���A��jA�l�A���A���A�A�A�t�A���A�33A�~�A�ƨA�r�A�n�A~�HA}33Ay��Ax-AvA�At�As7LAqG�Ap�An�AjbNAh^5Ag�FAf�DAeAeXAd�AcƨAb9XAa
=A`E�A\��A[G�AZQ�AY�AY�-AX�AWt�AV�DAUhsASAQ�^AOAO/AM��AJ�9AI�AG��AEG�AD1'AC��ACp�AA�A@VA?��A=VA;`BA:�HA:�+A9|�A9�A7+A4��A2�A1&�A.�yA-�A,9XA*�uA'
=A%�A%ƨA%G�A$1'A!A �A?}A��A5?A�A+A�At�A�AXA�DAhsA�DAbA��AdZA/AȴAE�A��A�`AK�A�\A �AƨAp�AXAG�A;dA�A��A��A�uA$�A��A�TA��A��A
�yA	�#AffA�^A�yA5?A��A��A�^A�\A-A1A��A�TA��A`BA%@��@��^@�V@�+@��R@�b@��/@�j@��T@��`@蛦@��@�@���@�1@�v�@�V@��@��`@�9X@ޏ\@�S�@�J@أ�@׾w@�
=@�^5@�7L@�C�@�n�@���@��`@�dZ@Ώ\@�hs@�|�@�E�@ȃ@�ƨ@�K�@�~�@�@���@�b@�@�hs@� �@��w@�n�@��7@�%@�b@�^5@�V@��!@��;@��@��w@��+@��#@���@�
=@��h@��@��@�9X@��
@�ƨ@�o@��T@��@��@�ȴ@���@���@�ff@��@�v�@�^5@��@�V@���@��/@�K�@��j@��@���@�(�@��w@�dZ@�+@��H@���@��-@�G�@�{@�^5@�V@��@�&�@���@�I�@�  @�"�@�%@��^@�5?@�=q@�x�@�&�@�&�@�1'@��@���@���@��@��w@��\@��@�(�@�j@��@�l�@�o@���@���@�@�x�@��u@�1'@��w@�\)@�o@�@��H@���@���@��@�33@�l�@�l�@��@�l�@�C�@�S�@�S�@�
=@��R@�-@�=q@�{@�x�@�Ĝ@��@��u@��@��@�o@��+@�M�@�5?@��@��^@��7@�G�@��j@�bN@��@�\)@�"�@�
=@��@���@���@�~�@�=q@���@���@���@���@���@��^@�@���@�?}@�%@���@��D@��m@���@�l�@�"�@���@�~�@�M�@�E�@�E�@�J@��h@�p�@�`B@�O�@�/@���@���@�j@�I�@�9X@��@��;@���@��@�+@���@���@��T@��-@�p�@�7L@���@�bN@�  @��;@�1@�9X@� �@�1@��@���@��@���@�J@���@���@�`B@�?}@�%@���@�z�@�1@�ƨ@���@�t�@�K�@��@���@���@��!@�^5@��@�%@��j@��@�bN@��m@��@�33@�@��H@�ȴ@��!@���@�n�@�J@��@�X@�O�@�O�@��@��@��/@��/@���@�Z@�b@���@��
@��w@���@�C�@�@��R@��+@�ff@�:*@t�@a�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�BuB\BDB1BB��B�mB�5B�#B�B�)B�sB��B��BBDBhB{B"�B.B8RBC�BG�BK�BO�BXBcTBhsBm�B|�B�B�B�B�+B�1B�B�VB�{B��B��B��B��B��B�oB�DB�By�Bm�BR�B6FB�BJB��B�fB�B��B��BĜB�RB��B�Bw�BdZBF�B<jB.B%�B�B�BB
�B
�B
�RB
�'B
��B
��B
�DB
r�B
YB
E�B
33B
'�B
�B
JB
B	��B	��B	�B	�`B	�BB	�
B	�qB	�!B	�B	��B	��B	��B	��B	��B	��B	�\B	�JB	y�B	gmB	aHB	^5B	\)B	XB	O�B	H�B	D�B	6FB	1'B	+B	&�B	�B	VB		7B	B��B�B�B�B�mB�NB�5B��B��B��BȴBB�qB�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�bB�\B�1B�+B�B�Bs�BjBgmBcTBbNBgmBu�Bw�Bp�Bn�Bk�BhsBhsBhsBr�Bv�Bu�Bo�Bt�B{�Bz�By�Bx�Bw�Bu�Bt�Bt�Bs�Br�Bq�Bp�Bn�Bo�Bp�Bo�Bo�Bo�Bo�Bn�Bo�Bq�Bs�Bv�Bw�By�Bx�Bx�Bz�B|�B� B�B�PB�VB�VB�bB�\B�\B�JB�=B�PB�hB�uB��B��B��B��B��B��B��B��B�B�3B�LB�wB�}B��BŢB��B��B��B�5B�B�B��B��B��B��B��B��B��B��B	B	%B	%B	B	B	B	+B	\B	\B	"�B	)�B	.B	1'B	0!B	0!B	/B	/B	1'B	;dB	;dB	A�B	J�B	G�B	O�B	YB	[#B	`BB	ffB	hsB	jB	jB	p�B	r�B	q�B	q�B	o�B	o�B	o�B	o�B	s�B	w�B	{�B	� B	�B	�B	�=B	�DB	�JB	�JB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�?B	�LB	�RB	�RB	�RB	�RB	�dB	�jB	�jB	�qB	�qB	��B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
PB
PB
JB
PB
PB
PB
VB
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
TB
OB
/O2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�B�B�B�B�B�B�B�B�BuB\BDB1BB��B�mB�5B�#B�B�)B�sB��B��BBDBhB{B"�B.B8RBC�BG�BK�BO�BXBcTBhsBm�B|�B�B�B�B�+B�1B�B�VB�{B��B��B��B��B��B�oB�DB�By�Bm�BR�B6FB�BJB��B�fB�B��B��BĜB�RB��B�Bw�BdZBF�B<jB.B%�B�B�BB
�B
�B
�RB
�'B
��B
��B
�DB
r�B
YB
E�B
33B
'�B
�B
JB
B	��B	��B	�B	�`B	�BB	�
B	�qB	�!B	�B	��B	��B	��B	��B	��B	��B	�\B	�JB	y�B	gmB	aHB	^5B	\)B	XB	O�B	H�B	D�B	6FB	1'B	+B	&�B	�B	VB		7B	B��B�B�B�B�mB�NB�5B��B��B��BȴBB�qB�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�bB�\B�1B�+B�B�Bs�BjBgmBcTBbNBgmBu�Bw�Bp�Bn�Bk�BhsBhsBhsBr�Bv�Bu�Bo�Bt�B{�Bz�By�Bx�Bw�Bu�Bt�Bt�Bs�Br�Bq�Bp�Bn�Bo�Bp�Bo�Bo�Bo�Bo�Bn�Bo�Bq�Bs�Bv�Bw�By�Bx�Bx�Bz�B|�B� B�B�PB�VB�VB�bB�\B�\B�JB�=B�PB�hB�uB��B��B��B��B��B��B��B��B�B�3B�LB�wB�}B��BŢB��B��B��B�5B�B�B��B��B��B��B��B��B��B��B	B	%B	%B	B	B	B	+B	\B	\B	"�B	)�B	.B	1'B	0!B	0!B	/B	/B	1'B	;dB	;dB	A�B	J�B	G�B	O�B	YB	[#B	`BB	ffB	hsB	jB	jB	p�B	r�B	q�B	q�B	o�B	o�B	o�B	o�B	s�B	w�B	{�B	� B	�B	�B	�=B	�DB	�JB	�JB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�?B	�LB	�RB	�RB	�RB	�RB	�dB	�jB	�jB	�qB	�qB	��B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
PB
PB
JB
PB
PB
PB
VB
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
TB
OB
/O2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191707                              AO  ARCAADJP                                                                    20181005191707    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191707  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191707  QCF$                G�O�G�O�G�O�8000            