CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:28Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191728  20181005191728  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$Q��1   @��$��@6/\(��d~��vȴ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C!�fC$  C&  C(  C*  C,�C.  C0  C2  C4�C6  C8  C:�C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C_�fCa�fCc�fCf  Ch  Cj  Cl  Cn  Cp�Cr�Ct�Cv�Cx�Cz�C{�fC}�fC�  C��3C�  C��C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��C�  C��C�  C�  C��C��C�  C�  C��C�  C�  C�  C��C��C��C�  C��3C��3C��3C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C��C��C��3C��3C�  C�  C�  C�  C��3C�  C�  C��3C��fC��3C�  C��C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  D   D � D  D� D��D� D  D� D  D� D��Dy�D  D� D��Dy�D  D� D	  D	� D
fD
� D  Dy�D��Dy�D  D� D  D� D  Dy�D��Dy�D��D� D  Dy�D  D�fD  D� D  D�fD  D�fD  D� D  D� D  D� DfD� D��D� D  D� D  D� D  D� D  D� D��D � D!  D!�fD"  D"� D"��D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(�fD)fD)�fD*  D*� D*��D+y�D,  D,� D-fD-� D.  D.y�D/  D/� D0  D0�fD1fD1�fD2fD2�fD3  D3� D4fD4� D5  D5y�D6  D6� D7  D7y�D7��D8� D9fD9� D:  D:� D;fD;� D<  D<� D=  D=� D=��D>y�D?  D?�fD@  D@� DA  DA� DB  DB�fDCfDC� DD  DD� DE  DE� DFfDF� DGfDG� DH  DH� DH��DIs3DI��DJ�fDKfDK� DLfDL� DM  DM�fDN  DNy�DN��DO� DPfDP� DP��DQy�DR  DRy�DSfDS�fDS��DT� DUfDUy�DV  DV�fDWfDWy�DW��DX� DY  DYy�DZ  DZ� D[  D[�fD\  D\� D\��D]y�D^  D^�fD_fD_� D_��D`� DafDa�fDa��Db� Dc  Dcy�Dc��Dd� De  Dey�De��Df� DgfDg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dk��Dl� Dm  Dm� DnfDn�fDo  Do� DpfDp� Dq  Dq�fDr  Dry�Dr��Ds� DtfDt� DufDu� Dv  Dvy�Dw  DwY�Dy��D�?�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�p�A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BO�BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�
=B�
=B��
B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B��
B�
=B�
=B�
=B�
=B�
=B�
=C CC�CCC
CCCCCCCCCC�C C!�C$C&C(C*C,�C.C0C2C4�C6C8C:�C<C>C@CBCDCFCH�CJCLCNCPCRCTCVCXCY�C\C^C_�Ca�Cc�CfChCjClCnCp�Cr�Ct�Cv�Cx�Cz�C{�C}�C��C���C��C�\C��C�\C�\C��C���C��C��C��C��C��C�\C��C�\C��C��C�\C�\C��C��C�\C��C��C��C�\C�\C�\C��C���C���C���C��C���C��C��C��C��C��C���C���C���C�\C�\C���C���C��C��C��C��C���C��C��C���C���C���C��C�\C��C��C�\C��C��C��C��C���C��C��C���C��C�\C�\C�)C��C��C��C��C���C���C��C�\C��C��C��C��C��C���C��C�\C��C��C�\C�\C��C��C��C���C��C��C��C��C�\C��C��C��C��C��C���C��C��C���C���C��C��C��C��C��C��C��C�\C�\C��C�\C��C��C��D HD �HDHD�HD��D�HDHD�HDHD�HD��Dz�DHD�HD��Dz�DHD�HD	HD	�HD
�D
�HDHDz�D��Dz�DHD�HDHD�HDHDz�D��Dz�D��D�HDHDz�DHD��DHD�HDHD��DHD��DHD�HDHD�HDHD�HD�D�HD��D�HDHD�HDHD�HDHD�HDHD�HD��D �HD!HD!��D"HD"�HD"��D#�HD$HD$�HD%HD%�HD&HD&��D'HD'�HD(HD(��D)�D)��D*HD*�HD*��D+z�D,HD,�HD-�D-�HD.HD.z�D/HD/�HD0HD0��D1�D1��D2�D2��D3HD3�HD4�D4�HD5HD5z�D6HD6�HD7HD7z�D7��D8�HD9�D9�HD:HD:�HD;�D;�HD<HD<�HD=HD=�HD=��D>z�D?HD?��D@HD@�HDAHDA�HDBHDB��DC�DC�HDDHDD�HDEHDE�HDF�DF�HDG�DG�HDHHDH�HDH��DIt{DI��DJ��DK�DK�HDL�DL�HDMHDM��DNHDNz�DN��DO�HDP�DP�HDP��DQz�DRHDRz�DS�DS��DS��DT�HDU�DUz�DVHDV��DW�DWz�DW��DX�HDYHDYz�DZHDZ�HD[HD[��D\HD\�HD\��D]z�D^HD^��D_�D_�HD_��D`�HDa�Da��Da��Db�HDcHDcz�Dc��Dd�HDeHDez�De��Df�HDg�Dg�HDhHDh�HDiHDi�HDjHDjz�DkHDk�HDk��Dl�HDmHDm�HDn�Dn��DoHDo�HDp�Dp�HDqHDq��DrHDrz�Dr��Ds�HDt�Dt�HDu�Du�HDvHDvz�DwHDwZ�Dy��D�@RD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�;dA�A�A�E�A�E�A�I�A�C�A�I�A�Q�A�K�A�K�A�S�A�M�A�O�A�\)A�ZA�\)A�\)A�^5A�`BA�`BA�`BA�ffA�hsA�jA�n�A�r�A�p�A�l�A�jA�hsA�jA�jA�l�A�l�A�`BA�`BA�bNA�^5A�M�A��A�"�A�p�A�-A׋DA�%AҺ^AсAϰ!AΙ�Aͥ�A�ƨA�bA�?}A�(�A�=qA��HA��AƅA��HA���A��FA�ĜA�33A�;dA��FA�ffA�`BA���A�+A�"�A�A���A��jA�^5A�M�A���A�S�A��A��A�oA��^A�/A��A���A�1A��`A���A��;A��!A���A��A���A�A�33A�7LA��A�v�A�&�A���A�  A�hsA�A�bNA�+A�VA��A��hA�;dA�G�A��hA�oA�(�A�z�A�K�A��A�G�A�1A��uA�ĜA�;dA�t�A�7LA�S�A�A}ƨAz�Aw��AuK�Arn�AqC�Apz�Ann�AkS�Ah�AgƨAf�HAe�mAd�DAc�A`ZA^(�A]�A\~�AZȴAZbAY\)AW�AV�jAV1'AU��AT$�ARAQ
=AP=qAN��AMAK�wAIl�AHjAG�hAF�AEhsACO�A@��A@=qA>�\A<�A<A:ffA8�yA7�A7G�A6ȴA4�!A3�A1/A/�
A.�/A.9XA-t�A,�A,�\A*�/A*�A(��A'�TA&Q�A%�^A$�DA#S�A"r�A!33A �+AhsA�/AJAx�A�A1'AQ�A�A�AG�AoA�A��AbNAA�An�AZAjAE�A�wAXAn�A�yA
-A9XA�A��A^5A�A�PA�+A1A�A;dA 1'@�=q@���@�ƨ@�+@�  @�33@��y@���@�ff@�@�7L@��
@�bN@��@�O�@�@�?}@��/@�z�@�bN@��/@���@��@��m@�t�@߅@�ƨ@���@�  @���@��T@�J@�A�@�?}@�
=@Չ7@���@�;d@�-@�j@�"�@�5?@��@�9X@υ@�
=@ΰ!@͙�@̴9@�~�@�G�@�I�@�bN@ȓu@� �@�
=@��@ě�@��@�|�@�;d@�ff@�(�@��@��\@���@��@���@�`B@��@��`@��@��@�ƨ@�dZ@���@�(�@��D@��@��^@���@���@��@�o@�$�@���@��@���@��@�z�@���@���@��@��;@�I�@�z�@�r�@���@��m@��@��@�7L@�G�@�Ĝ@�9X@��;@���@�K�@�@���@���@�M�@��@���@�%@���@�9X@��@��
@���@�S�@�C�@���@�ff@��-@��@�ƨ@�K�@��@��
@�  @��m@��@�K�@�"�@��R@�E�@��h@�O�@�V@��/@���@�bN@�b@��@��w@�S�@�+@�
=@��@�
=@���@�J@�J@���@�%@���@�1'@��m@�ƨ@�dZ@�+@�@�33@���@�1@��@��@�r�@��;@�@��\@�@�"�@��F@��@�|�@�t�@�|�@�t�@���@���@��+@�=q@���@�@�`B@�%@��/@��9@�bN@� �@��@��
@���@�ƨ@��w@��w@��w@��w@��w@��@��@��F@�(�@��@��;@��P@���@�(�@���@�@��\@��-@���@��@��7@��7@��@�V@�G�@�G�@�&�@�7L@�?}@�?}@�V@���@��`@���@���@�Z@�  @��@�ƨ@���@��@�C�@�@��R@�~�@�V@�J@��@���@��-@�x�@�p�@�hs@�7L@���@���@��9@��@�Z@�b@��w@�S�@�o@���@���@�~�@�{@��T@�J�@|�O@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�;dA�A�A�E�A�E�A�I�A�C�A�I�A�Q�A�K�A�K�A�S�A�M�A�O�A�\)A�ZA�\)A�\)A�^5A�`BA�`BA�`BA�ffA�hsA�jA�n�A�r�A�p�A�l�A�jA�hsA�jA�jA�l�A�l�A�`BA�`BA�bNA�^5A�M�A��A�"�A�p�A�-A׋DA�%AҺ^AсAϰ!AΙ�Aͥ�A�ƨA�bA�?}A�(�A�=qA��HA��AƅA��HA���A��FA�ĜA�33A�;dA��FA�ffA�`BA���A�+A�"�A�A���A��jA�^5A�M�A���A�S�A��A��A�oA��^A�/A��A���A�1A��`A���A��;A��!A���A��A���A�A�33A�7LA��A�v�A�&�A���A�  A�hsA�A�bNA�+A�VA��A��hA�;dA�G�A��hA�oA�(�A�z�A�K�A��A�G�A�1A��uA�ĜA�;dA�t�A�7LA�S�A�A}ƨAz�Aw��AuK�Arn�AqC�Apz�Ann�AkS�Ah�AgƨAf�HAe�mAd�DAc�A`ZA^(�A]�A\~�AZȴAZbAY\)AW�AV�jAV1'AU��AT$�ARAQ
=AP=qAN��AMAK�wAIl�AHjAG�hAF�AEhsACO�A@��A@=qA>�\A<�A<A:ffA8�yA7�A7G�A6ȴA4�!A3�A1/A/�
A.�/A.9XA-t�A,�A,�\A*�/A*�A(��A'�TA&Q�A%�^A$�DA#S�A"r�A!33A �+AhsA�/AJAx�A�A1'AQ�A�A�AG�AoA�A��AbNAA�An�AZAjAE�A�wAXAn�A�yA
-A9XA�A��A^5A�A�PA�+A1A�A;dA 1'@�=q@���@�ƨ@�+@�  @�33@��y@���@�ff@�@�7L@��
@�bN@��@�O�@�@�?}@��/@�z�@�bN@��/@���@��@��m@�t�@߅@�ƨ@���@�  @���@��T@�J@�A�@�?}@�
=@Չ7@���@�;d@�-@�j@�"�@�5?@��@�9X@υ@�
=@ΰ!@͙�@̴9@�~�@�G�@�I�@�bN@ȓu@� �@�
=@��@ě�@��@�|�@�;d@�ff@�(�@��@��\@���@��@���@�`B@��@��`@��@��@�ƨ@�dZ@���@�(�@��D@��@��^@���@���@��@�o@�$�@���@��@���@��@�z�@���@���@��@��;@�I�@�z�@�r�@���@��m@��@��@�7L@�G�@�Ĝ@�9X@��;@���@�K�@�@���@���@�M�@��@���@�%@���@�9X@��@��
@���@�S�@�C�@���@�ff@��-@��@�ƨ@�K�@��@��
@�  @��m@��@�K�@�"�@��R@�E�@��h@�O�@�V@��/@���@�bN@�b@��@��w@�S�@�+@�
=@��@�
=@���@�J@�J@���@�%@���@�1'@��m@�ƨ@�dZ@�+@�@�33@���@�1@��@��@�r�@��;@�@��\@�@�"�@��F@��@�|�@�t�@�|�@�t�@���@���@��+@�=q@���@�@�`B@�%@��/@��9@�bN@� �@��@��
@���@�ƨ@��w@��w@��w@��w@��w@��@��@��F@�(�@��@��;@��P@���@�(�@���@�@��\@��-@���@��@��7@��7@��@�V@�G�@�G�@�&�@�7L@�?}@�?}@�V@���@��`@���@���@�Z@�  @��@�ƨ@���@��@�C�@�@��R@�~�@�V@�J@��@���@��-@�x�@�p�@�hs@�7L@���@���@��9@��@�Z@�b@��w@�S�@�o@���@���@�~�@�{@��T@�J�@|�O@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B#�B#�B#�B#�B#�B#�B$�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B$�B$�B$�B%�B&�B-B7LB;dB=qB>wB?}B?}B@�B@�B@�BA�BB�BD�BJ�Bm�B�VB�-B��B�BC�B0!B��B�mB��B	7B{B�B49BaHB� B�1B�JB�hB�oB��B�uB��B��B��B�B�B�B�B�B�-B�wB��BƨB��B��B�
B��BɺB�^B�B��B~�Bs�BaHBD�B0!B#�B<jB;dB/B$�BVB��B��B��B�yB�BÖB�wB�?B��B��B��B��B�+By�BcTBR�BL�BA�B8RB2-B&�B�BbB  B
�B
�B
�fB
�B
��B
�?B
��B
�oB
�=B
x�B
]/B
J�B
6FB
#�B
�B
oB
B	�B	�;B	�B	��B	��B	ĜB	�dB	�B	��B	��B	�uB	�=B	�%B	�B	z�B	v�B	r�B	p�B	gmB	\)B	VB	P�B	J�B	C�B	:^B	.B	(�B	#�B	�B	{B	1B��B��B�B�B�yB�NB�#B�B�B��B��BȴBB�}B�dB�XB�LB�9B�-B�B�B��B��B��B��B��B��B��B�{B�bB�JB�+B�B�B�B~�B}�B{�B{�Bz�By�Bw�Bu�Bp�Bm�BiyBhsBhsBgmBffBe`BbNB_;BZBT�BP�BK�BE�BD�BB�B@�B>wB=qB=qB<jB:^B9XB8RB:^B>wBG�BJ�BP�BR�BT�BYBhsBe`B]/BgmBk�Bo�Bo�Br�Bo�B`BBG�B<jB;dB<jB@�BJ�BM�BN�BN�BM�BJ�BT�BW
BM�BJ�BJ�BL�BH�BI�B_;B_;B]/B\)BZBZB[#BZBYBW
BS�BQ�BQ�BVB\)B]/B\)B]/B_;BbNBaHB`BB_;B\)B[#BYBYB^5BffBn�Bp�Br�Bu�Bz�B{�B|�B�B� B�B�=B�7B�7B�7B�1B�7B�=B�DB�DB�PB�hB�{B��B��B��B��B�-B�FB�XB�^B�^B�wBȴB��B�B�#B�BB�TB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B	  B	  B	B	
=B	DB	1B	1B	
=B	bB	oB	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	'�B	(�B	-B	0!B	49B	5?B	7LB	<jB	@�B	A�B	E�B	G�B	I�B	N�B	Q�B	R�B	VB	W
B	YB	[#B	[#B	^5B	`BB	cTB	hsB	m�B	q�B	r�B	v�B	|�B	{�B	z�B	z�B	�B	�1B	�JB	�\B	�bB	�bB	�hB	�hB	�oB	�oB	�oB	�{B	�uB	�uB	�uB	�uB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�-B	�3B	�?B	�RB	�dB	�qB	�wB	�}B	B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�IB
KB
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B#�B#�B#�B#�B#�B#�B$�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B$�B$�B$�B%�B&�B-B7LB;dB=qB>wB?}B?}B@�B@�B@�BA�BB�BD�BJ�Bm�B�VB�-B��B�BC�B0!B��B�mB��B	7B{B�B49BaHB� B�1B�JB�hB�oB��B�uB��B��B��B�B�B�B�B�B�-B�wB��BƨB��B��B�
B��BɺB�^B�B��B~�Bs�BaHBD�B0!B#�B<jB;dB/B$�BVB��B��B��B�yB�BÖB�wB�?B��B��B��B��B�+By�BcTBR�BL�BA�B8RB2-B&�B�BbB  B
�B
�B
�fB
�B
��B
�?B
��B
�oB
�=B
x�B
]/B
J�B
6FB
#�B
�B
oB
B	�B	�;B	�B	��B	��B	ĜB	�dB	�B	��B	��B	�uB	�=B	�%B	�B	z�B	v�B	r�B	p�B	gmB	\)B	VB	P�B	J�B	C�B	:^B	.B	(�B	#�B	�B	{B	1B��B��B�B�B�yB�NB�#B�B�B��B��BȴBB�}B�dB�XB�LB�9B�-B�B�B��B��B��B��B��B��B��B�{B�bB�JB�+B�B�B�B~�B}�B{�B{�Bz�By�Bw�Bu�Bp�Bm�BiyBhsBhsBgmBffBe`BbNB_;BZBT�BP�BK�BE�BD�BB�B@�B>wB=qB=qB<jB:^B9XB8RB:^B>wBG�BJ�BP�BR�BT�BYBhsBe`B]/BgmBk�Bo�Bo�Br�Bo�B`BBG�B<jB;dB<jB@�BJ�BM�BN�BN�BM�BJ�BT�BW
BM�BJ�BJ�BL�BH�BI�B_;B_;B]/B\)BZBZB[#BZBYBW
BS�BQ�BQ�BVB\)B]/B\)B]/B_;BbNBaHB`BB_;B\)B[#BYBYB^5BffBn�Bp�Br�Bu�Bz�B{�B|�B�B� B�B�=B�7B�7B�7B�1B�7B�=B�DB�DB�PB�hB�{B��B��B��B��B�-B�FB�XB�^B�^B�wBȴB��B�B�#B�BB�TB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B	  B	  B	B	
=B	DB	1B	1B	
=B	bB	oB	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	'�B	(�B	-B	0!B	49B	5?B	7LB	<jB	@�B	A�B	E�B	G�B	I�B	N�B	Q�B	R�B	VB	W
B	YB	[#B	[#B	^5B	`BB	cTB	hsB	m�B	q�B	r�B	v�B	|�B	{�B	z�B	z�B	�B	�1B	�JB	�\B	�bB	�bB	�hB	�hB	�oB	�oB	�oB	�{B	�uB	�uB	�uB	�uB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�-B	�3B	�?B	�RB	�dB	�qB	�wB	�}B	B	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�IB
KB
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191728                              AO  ARCAADJP                                                                    20181005191728    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191728  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191728  QCF$                G�O�G�O�G�O�8000            