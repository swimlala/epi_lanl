CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:15Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190615  20181005190615  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @כ����1   @כ�$�"@3�
=p���c�9XbN1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @���@�33@���A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C!�fC#�fC&  C(�C*�C,  C-�fC0  C2�C4  C5�fC8  C:  C<  C>  C@  CB  CC�fCF  CH  CI�fCK�fCM�fCP  CR�CS�fCV  CX�CZ�C[�fC]�fC`  Cb  Cd�Cf  Cg�fCi��Ck��Cm�fCo�fCq��Cs�fCv33Cw�fCz  C{�fC~  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C��C��3C�  C�  C��C��3C��C��3C��C��3C�  C��C��C�  C��fC��3C��C��C�  C��fC��fC��3C�  C��C��C��3C��fC��3C��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC�  C��C��C��3C��3C��fC�  C��C��C��3C�  C��C�  C��fC�  C�  C�  C��C�  C��3C��3C��3C�  C��C��C��C�  C��3C�  C��C�  C��3C��C�  C��3C�  C��C�  C��3C��C��C��C�  C��fC��fC��fC��3C��fC��fC��fC��fC��3C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C��fC�  C��C��C�  C��fC�  D fD � D  D�fD  Ds3DfD��D  Ds3D  D�fD��D� D�D� D�3Dy�D��D	y�D
  D
� D  D�fD  Dy�D  D� D  Dy�D  D� DfD� D��Dy�D  D� D��D�fD  D� DfD�fDfD� D  D� D  D� D  D�fD  D�fDfDy�D��D� D��D� D��Dy�D��D� D fD y�D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&fD&�fD&��D'y�D(  D(� D)  D)y�D*  D*y�D*��D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0fD0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D9��D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D?�3D@s3D@��DA� DBfDB�fDC  DCy�DD  DD�fDE  DEy�DFfDF��DG  DGy�DHfDH�fDI  DIy�DI��DJ� DKfDK��DLfDLy�DL��DM�fDNfDNy�DN��DO� DP  DP� DQ  DQ� DRfDR�fDSfDS�fDT  DT�fDUfDU� DU��DVy�DW  DW� DX  DX�fDYfDY� DZ  DZ� DZ��D[y�D\  D\�fD]  D]y�D^  D^�fD_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�De��Df� DgfDg�fDh  Dh� Dh��Di� Di��Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dw� Dy�\D�?�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�@��A�A;�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B�p�B���B���B���B��
B��
Bˣ�B�p�B�p�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�RC��C!�RC#�RC%��C'�C)�C+��C-�RC/��C1�C3��C5�RC7��C9��C;��C=��C?��CA��CC�RCE��CG��CI�RCK�RCM�RCO��CQ�CS�RCU��CW�CY�C[�RC]�RC_��Ca��Cc�Ce��Cg�RCi��Ck��Cm�RCo�RCq��Cs�RCvCw�RCy��C{�RC}��C��C���C���C���C��)C��)C���C���C���C���C��)C���C���C���C���C���C��)C���C���C���C���C��)C���C���C���C��)C��C��)C���C��)C���C���C���C���C��\C��)C��C��C���C��\C��\C��)C���C��C���C��)C��\C��)C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C���C���C���C��)C��)C��\C���C��C���C��)C���C���C���C��\C���C���C���C���C���C��)C��)C��)C���C���C��C��C���C��)C���C���C���C��)C���C���C��)C���C��C���C��)C���C���C��C���C��\C��\C��\C��)C��\C��\C��\C��\C��)C��)C��)C���C���C���C���C���C���C���C��C���C��\C���C���C��C���C��\C���C���D t{D �{Dz�D�{Dg�D��D�HD�{Dg�D�{Dz�D�Dt{DHDt{D�DnD�D	nD	�{D
t{D
�{Dz�D�{DnD�{Dt{D�{DnD�{Dt{D��Dt{D�DnD�{Dt{D�Dz�D�{Dt{D��Dz�D��Dt{D�{Dt{D�{Dt{D�{Dz�D�{Dz�D��DnD�Dt{D�Dt{D�DnD�Dt{D��D nD �{D!t{D!�{D"t{D"�{D#t{D#��D$t{D$�{D%t{D%��D&z�D&�D'nD'�{D(t{D(�{D)nD)�{D*nD*�D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/nD/��D0t{D0��D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8��D9t{D9�D:t{D:��D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�D@g�D@�DAt{DA��DBz�DB�{DCnDC�{DDz�DD�{DEnDE��DF�HDF�{DGnDG��DHz�DH�{DInDI�DJt{DJ��DK�HDK��DLnDL�DMz�DM��DNnDN�DOt{DO�{DPt{DP�{DQt{DQ��DRz�DR��DSz�DS�{DTz�DT��DUt{DU�DVnDV�{DWt{DW�{DXz�DX��DYt{DY�{DZt{DZ�D[nD[�{D\z�D\�{D]nD]�{D^z�D^�{D_nD_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{DenDe�Dft{Df��Dgz�Dg�{Dht{Dh�Dit{Di�DjnDj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv��Dwt{Dw�{Dy��D�9�D�ʏ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A�  A�A�1A�1A�  A���A���A���A���A�A�  A���A���A�  A���A���A��yA���AǑhA�^5A�ĜA�1'A��A�A��#AŸRAŕ�A�1'Aě�Aĕ�Aĝ�AĬA���A�A�(�Aĺ^AēuAĲ-Aě�A�hsA��/A�ZA��A��`A¼jA©�A��A���A��9A�x�A��A��7A�G�A�  A�z�A��A��A��TA���A�dZA�O�A��RA���A��9A�`BA�"�A��
A���A���A��A�oA�JA��FA��A�$�A�1'A�I�A��A�-A��A��/A��A���A��yA���A�E�A���A��DA���A�(�A��A���A�dZA�O�A�p�A��uA�x�A�l�A�x�A�ȴA�dZA��#A��A�A�VA~r�A}��A}?}A{��AzJAy�wAyC�Au�Arn�Aq|�ApE�Am�Aj�AhĜAdffA`M�A_33A[AR^5AQ%ANv�AM;dAIG�AF�RAD�AC��AC|�AC&�ABȴAA��AAXAAO�A@r�A>�uA>jA<��A:5?A933A8{A6Q�A5;dA1ƨA0jA/�;A/�A/��A.�jA+��A*v�A)XA'��A%��A#\)A!�TA!G�A �\A�mAt�A�AK�A�A��A�wA�AJA1A{A��A�A�+A5?A$�Al�AS�A-A  AM�A�A�#A
�jA	��AȴA  A&�A~�A�A��Az�AXA�wAAAoA @�`B@��^@�@���@�K�@��u@�-@��^@��\@��@�%@�%@�v�@��@��@�5?@�G�@�9X@���@�r�@�M�@�/@��u@��;@߅@޸R@�p�@��@�Z@�ƨ@�S�@֏\@�=q@�E�@�E�@�5?@Ӿw@���@�bN@�  @�S�@��H@�ȴ@�^5@�7L@̓u@�bN@��m@���@�^5@�E�@���@�%@�Ĝ@�l�@��T@ŉ7@ũ�@ŉ7@�?}@���@�G�@ēu@��@��7@��9@�Ĝ@�z�@���@�ƨ@�v�@�hs@���@�t�@�C�@�"�@���@��T@�V@�Ĝ@���@��@��@��D@�z�@���@�bN@�r�@�1'@�1'@��@��@���@��@�33@�M�@��@��@��@��R@�M�@�p�@��/@��u@���@�;d@�M�@�5?@�v�@��\@�v�@�{@���@��-@�x�@��9@�1'@�b@���@���@���@�V@�{@���@�hs@�`B@�hs@�O�@�hs@��h@��^@��@��@� �@�;d@�^5@���@�V@���@�I�@��@�+@��\@�V@���@�hs@�I�@�|�@��y@���@��\@�~�@�$�@���@��-@��7@��@��9@�Q�@� �@�1@��m@��@�l�@�"�@�@�V@�@��@��T@���@��^@���@���@��h@��7@��@�x�@�p�@�hs@�`B@�G�@��@��@�Ĝ@�A�@�b@�ƨ@���@�|�@�dZ@�C�@�@��R@�~�@�V@�@���@�V@���@�&�@�V@���@�z�@��@���@��w@�|�@�o@��+@�V@�J@��@��@��T@��T@��T@��#@�@�@��7@�7L@�/@��@�V@��j@��@�ƨ@���@���@��@�;d@���@�V@�-@�J@�X@���@���@��@��m@�@�ȴ@���@�E�@�@���@��h@�X@�7L@��`@��u@�I�@�  @��
@��w@���@�\)@�"�@��@���@��+@�{@��T@��^@��@���@�z�@�A�@���@���@�ƨ@���@�\)@�K�@�
=@���@��\@�v�@�ff@�E�@�{@��T@��^@���@��@��@���@��@��@��@�bN@�A�@iD@o�}@_"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A���A�  A�A�1A�1A�  A���A���A���A���A�A�  A���A���A�  A���A���A��yA���AǑhA�^5A�ĜA�1'A��A�A��#AŸRAŕ�A�1'Aě�Aĕ�Aĝ�AĬA���A�A�(�Aĺ^AēuAĲ-Aě�A�hsA��/A�ZA��A��`A¼jA©�A��A���A��9A�x�A��A��7A�G�A�  A�z�A��A��A��TA���A�dZA�O�A��RA���A��9A�`BA�"�A��
A���A���A��A�oA�JA��FA��A�$�A�1'A�I�A��A�-A��A��/A��A���A��yA���A�E�A���A��DA���A�(�A��A���A�dZA�O�A�p�A��uA�x�A�l�A�x�A�ȴA�dZA��#A��A�A�VA~r�A}��A}?}A{��AzJAy�wAyC�Au�Arn�Aq|�ApE�Am�Aj�AhĜAdffA`M�A_33A[AR^5AQ%ANv�AM;dAIG�AF�RAD�AC��AC|�AC&�ABȴAA��AAXAAO�A@r�A>�uA>jA<��A:5?A933A8{A6Q�A5;dA1ƨA0jA/�;A/�A/��A.�jA+��A*v�A)XA'��A%��A#\)A!�TA!G�A �\A�mAt�A�AK�A�A��A�wA�AJA1A{A��A�A�+A5?A$�Al�AS�A-A  AM�A�A�#A
�jA	��AȴA  A&�A~�A�A��Az�AXA�wAAAoA @�`B@��^@�@���@�K�@��u@�-@��^@��\@��@�%@�%@�v�@��@��@�5?@�G�@�9X@���@�r�@�M�@�/@��u@��;@߅@޸R@�p�@��@�Z@�ƨ@�S�@֏\@�=q@�E�@�E�@�5?@Ӿw@���@�bN@�  @�S�@��H@�ȴ@�^5@�7L@̓u@�bN@��m@���@�^5@�E�@���@�%@�Ĝ@�l�@��T@ŉ7@ũ�@ŉ7@�?}@���@�G�@ēu@��@��7@��9@�Ĝ@�z�@���@�ƨ@�v�@�hs@���@�t�@�C�@�"�@���@��T@�V@�Ĝ@���@��@��@��D@�z�@���@�bN@�r�@�1'@�1'@��@��@���@��@�33@�M�@��@��@��@��R@�M�@�p�@��/@��u@���@�;d@�M�@�5?@�v�@��\@�v�@�{@���@��-@�x�@��9@�1'@�b@���@���@���@�V@�{@���@�hs@�`B@�hs@�O�@�hs@��h@��^@��@��@� �@�;d@�^5@���@�V@���@�I�@��@�+@��\@�V@���@�hs@�I�@�|�@��y@���@��\@�~�@�$�@���@��-@��7@��@��9@�Q�@� �@�1@��m@��@�l�@�"�@�@�V@�@��@��T@���@��^@���@���@��h@��7@��@�x�@�p�@�hs@�`B@�G�@��@��@�Ĝ@�A�@�b@�ƨ@���@�|�@�dZ@�C�@�@��R@�~�@�V@�@���@�V@���@�&�@�V@���@�z�@��@���@��w@�|�@�o@��+@�V@�J@��@��@��T@��T@��T@��#@�@�@��7@�7L@�/@��@�V@��j@��@�ƨ@���@���@��@�;d@���@�V@�-@�J@�X@���@���@��@��m@�@�ȴ@���@�E�@�@���@��h@�X@�7L@��`@��u@�I�@�  @��
@��w@���@�\)@�"�@��@���@��+@�{@��T@��^@��@���@�z�@�A�@���@���@�ƨ@���@�\)@�K�@�
=@���@��\@�v�@�ff@�E�@�{@��T@��^@���@��@��@���@��@��@��@�bN@�A�@iD@o�}@_"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�NB�NB�NB�HB�HB�HB�HB�HB�HB�NB�ZB�ZB�TB�ZB�ZB�ZB�ZB�`B�`B�`B�`B�mB�sB�fB�;B�)B�B�
B��B��B��B��B��B��B�5B�yB  B�B�B"�B33B6FB1'B#�B�B!�B/B8RBF�BN�BT�Bv�B�VB��B��B��B�B�?B�FB�^B�XBÖB�
B�B��B��B��BɺBǮBÖB�3B��B�B��B�=Bw�BffB:^BVB�B�BȴBŢBĜB�wB��Bw�BZBL�B?}B{BDB
��B
�B
�5B
��B
ĜB
��B
�{B
�oB
�hB
�B
l�B
ffB
_;B
S�B
G�B
:^B
1'B
-B
'�B
�B
oB
\B
1B	�B	�
B	��B	B	��B	��B	�7B	jB	K�B	A�B	+B��B�B�ZB�BƨB�LB�!B�B�B�B��B��B�B��B�B��B�3B�dB��B��B��B�+Bz�Bm�BgmBe`Bm�B|�B��B��B��B��B��B��B��B��B��B��B��B��B��B�jBȴB��BǮBƨB��B��B�)B�yB�B��B��B��B��B�B�`B�BɺBB�B��B��B��B�hB�VB�oB��B��B�oB�PB��B��B��B��B�{B��B��B��B��B�B�dBȴB��B�B�TB�ZB�B��B��B�B��B��B��B�{B�VB�bB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�9B�^B�dB�dB�dB�}B��B��B��B��BBBÖBǮBȴB��B��B��B�
B�#B�)B�/B�BB�TB�ZB�fB�sB�B��B��B��B��B��B��B��B��B	B	B	
=B	uB	�B	�B	!�B	$�B	&�B	-B	1'B	1'B	7LB	;dB	?}B	D�B	J�B	O�B	P�B	O�B	Q�B	M�B	L�B	L�B	Q�B	VB	T�B	T�B	VB	YB	^5B	`BB	cTB	e`B	ffB	ffB	ffB	e`B	gmB	gmB	gmB	gmB	iyB	iyB	hsB	hsB	jB	l�B	o�B	p�B	t�B	u�B	x�B	z�B	{�B	}�B	� B	� B	}�B	~�B	}�B	� B	�B	�B	�B	�DB	�VB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�9B	�?B	�FB	�RB	�XB	�wB	��B	��B	��B	B	B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�BB	�HB	�HB	�NB	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
VB
�B
B
/�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�NB�NB�NB�HB�HB�HB�HB�HB�HB�NB�ZB�ZB�TB�ZB�ZB�ZB�ZB�`B�`B�`B�`B�mB�sB�fB�;B�)B�B�
B��B��B��B��B��B��B�5B�yB  B�B�B"�B33B6FB1'B#�B�B!�B/B8RBF�BN�BT�Bv�B�VB��B��B��B�B�?B�FB�^B�XBÖB�
B�B��B��B��BɺBǮBÖB�3B��B�B��B�=Bw�BffB:^BVB�B�BȴBŢBĜB�wB��Bw�BZBL�B?}B{BDB
��B
�B
�5B
��B
ĜB
��B
�{B
�oB
�hB
�B
l�B
ffB
_;B
S�B
G�B
:^B
1'B
-B
'�B
�B
oB
\B
1B	�B	�
B	��B	B	��B	��B	�7B	jB	K�B	A�B	+B��B�B�ZB�BƨB�LB�!B�B�B�B��B��B�B��B�B��B�3B�dB��B��B��B�+Bz�Bm�BgmBe`Bm�B|�B��B��B��B��B��B��B��B��B��B��B��B��B��B�jBȴB��BǮBƨB��B��B�)B�yB�B��B��B��B��B�B�`B�BɺBB�B��B��B��B�hB�VB�oB��B��B�oB�PB��B��B��B��B�{B��B��B��B��B�B�dBȴB��B�B�TB�ZB�B��B��B�B��B��B��B�{B�VB�bB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�9B�^B�dB�dB�dB�}B��B��B��B��BBBÖBǮBȴB��B��B��B�
B�#B�)B�/B�BB�TB�ZB�fB�sB�B��B��B��B��B��B��B��B��B	B	B	
=B	uB	�B	�B	!�B	$�B	&�B	-B	1'B	1'B	7LB	;dB	?}B	D�B	J�B	O�B	P�B	O�B	Q�B	M�B	L�B	L�B	Q�B	VB	T�B	T�B	VB	YB	^5B	`BB	cTB	e`B	ffB	ffB	ffB	e`B	gmB	gmB	gmB	gmB	iyB	iyB	hsB	hsB	jB	l�B	o�B	p�B	t�B	u�B	x�B	z�B	{�B	}�B	� B	� B	}�B	~�B	}�B	� B	�B	�B	�B	�DB	�VB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�9B	�?B	�FB	�RB	�XB	�wB	��B	��B	��B	B	B	B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�BB	�HB	�HB	�NB	�NB	�ZB	�ZB	�`B	�`B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
VB
�B
B
/�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190615                              AO  ARCAADJP                                                                    20181005190615    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190615  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190615  QCF$                G�O�G�O�G�O�8000            