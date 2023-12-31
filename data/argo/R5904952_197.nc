CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:50Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190550  20181005190550  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���t"d1   @��ff{`@1�������c��n��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�33B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� DfD� D  D� DfDy�D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"fD"�fD#  D#y�D#��D$y�D%  D%y�D&  D&� D&��D'� D(  D(�fD)fD)� D*  D*�fD+  D+y�D+��D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4fD4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9�fD:fD:� D;  D;�fD<  D<y�D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF�fDG  DGy�DG��DHy�DI  DI� DJ  DJ� DJ��DKy�DK��DL� DMfDM� DN  DN�fDOfDO�fDP  DP� DQfDQ� DR  DRy�DR��DS� DT  DT� DUfDU�fDV  DV� DW  DW� DW��DX� DY  DYy�DZ  DZ� D[  D[�fD\  D\y�D]  D]� D^  D^� D_  D_� D`  D`y�D`��Da� Db  Dby�Dc  Dc� Dc��Dd� DefDe� Df  Df� Dg  Dg�fDh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� DofDo�fDpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Du  Du� DvfDv� Dw  Dw� DwٚDy�3D�6D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
=@ȣ�AQ�A$Q�AB�RAdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{Bz�B!{B(�B1{B9{BA{BI{BP�BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��pB��pB��pB��pB��=C ECECECECEC
ECECECECECECECECECECEC EC"EC$EC&EC(EC*+�C,EC.^�C0^�C2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�/\C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C��C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HD�D�HDHD�HDHD��D
�D�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HD�D�HD�D�HDHD�HD�D��DHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HD�D�HD
�D�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD ��D!HD!�HD"�D"��D#HD#��D$
�D$��D%HD%��D&HD&�HD'
�D'�HD(HD(��D)�D)�HD*HD*��D+HD+��D,
�D,�HD-HD-��D.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3��D4�D4�HD5HD5�HD6HD6�HD7
�D7�HD8HD8�HD9HD9��D:�D:�HD;HD;��D<HD<��D=
�D=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB��DCHDC�HDDHDD�HDEHDE�HDFHDF��DGHDG��DH
�DH��DIHDI�HDJHDJ�HDK
�DK��DL
�DL�HDM�DM�HDNHDN��DO�DO��DPHDP�HDQ�DQ�HDRHDR��DS
�DS�HDTHDT�HDU�DU��DVHDV�HDWHDW�HDX
�DX�HDYHDY��DZHDZ�HD[HD[��D\HD\��D]HD]�HD^HD^�HD_HD_�HD`HD`��Da
�Da�HDbHDb��DcHDc�HDd
�Dd�HDe�De�HDfHDf�HDgHDg��DhHDh�HDiHDi��DjHDj�HDkHDk�HDlHDl�HDmHDm�HDn
�Dn�HDo�Do��Dp�Dp�HDqHDq�HDrHDr�HDsHDs�HDt
�Dt��DuHDu�HDv�Dv�HDwHDw�HDw��Dy�{D�>�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1A� �A�oA�(�A�&�A�(�A�&�A�$�A�&�A�&�A��A�+A�1'A�1'A�33A�1'A�+A�{A�dZA�Q�Aƺ^AƑhA�p�A�A�A�;dA�;dA�?}A�E�A�-A��A���AŬAŧ�AŲ-Aŧ�Ať�Aŝ�A�jA���A��FA�7LA��mA��\A��A�9XA���A�E�A���A�K�A���A��RA��A�A�A��DA�;dA��PA�-A��A�S�A�l�A��+A��PA�=qA��#A��A�=qA�%A��7A�%A�9XA�ĜA�M�A��yA���A��#A��A� �A��#A�+A�-A�+A���A�A��-A�5?A�%A���A�`BA�p�A���A��AyoAv�RApn�An�Al��Aj�DAf{Aa?}A^JAY��AUt�AR  APJAOx�AN-AK�AH5?AF^5AC�AB��AAdZA@jA?;dA;ƨA:bNA9;dA8  A7��A69XA4E�A2ȴA2M�A0M�A/�^A.=qA,�uA+S�A)�FA(�DA'�-A%��A$ȴA$��A$�DA#x�A"�!A!7LA {A 1'A�hA7LA��AM�A/A�AƨA&�A�yA��AVAI�A7LA��A��A33A�uA�A�PAK�A
�A
M�A
bA	p�A��AVAv�A5?AhsA�A�A�yA�A�A ȴ@��@�V@�@��9@�I�@� �@�b@���@���@�@��/@�M�@���@@���@���@��@��@���@��/@�33@���@䛦@�ƨ@���@�n�@�J@��#@��D@�C�@�^5@�@�Ĝ@��;@ۍP@�ȴ@�~�@�5?@�`B@ؓu@��@�t�@�ȴ@��@� �@���@�V@���@�`B@�%@���@�z�@�33@�o@�
=@��y@ΰ!@·+@�=q@͑h@��/@̼j@�r�@���@�dZ@�
=@���@ʟ�@ʇ+@��@ɑh@�hs@��@���@Ȭ@�(�@�ƨ@Ǖ�@�t�@ǍP@ǝ�@Ǯ@ǥ�@ǶF@��
@Ǯ@�+@��#@Ł@�V@���@� �@�\)@��y@¸R@+@�n�@�5?@�@���@��h@�X@��@���@��@�`B@�x�@�`B@��@�5?@���@��@���@�Ĝ@��@��m@�S�@��!@�M�@�@��7@��@�%@�Ĝ@�b@��\@�@��-@���@��@���@��@��@��R@�n�@���@��`@�j@�1@��m@��
@��
@�b@�Q�@��
@�S�@��y@��H@��!@�"�@�"�@�o@��!@�=q@�&�@�bN@��;@��@�+@��@���@�n�@�M�@�5?@�@���@�O�@��9@�C�@��H@�S�@��w@��@�1'@���@��m@���@��@�1'@���@��@���@��-@�x�@�/@��@�Ĝ@���@��@�bN@�b@��m@���@���@��P@�|�@�S�@��@��!@��\@�n�@�V@�5?@���@��9@��m@�dZ@�33@�o@���@�ȴ@���@�=q@���@�hs@�&�@�/@�&�@��@��9@�I�@�C�@�ȴ@��+@�^5@�5?@�@��@��@��j@�A�@�1'@�j@��D@��u@��u@���@� �@���@�t�@�33@��H@���@���@�n�@���@�X@���@��j@��9@��9@��@��u@�r�@�(�@���@���@�1'@��
@�t�@�"�@��H@�ȴ@��!@�^5@�{@��@�hs@�?}@�7L@���@���@��D@�I�@� �@�1@�ƨ@��F@��@�
=@�=q@��#@��7@�x�@�hs@�X@�&�@��/@��9@��@��@��@��@��@��D@�Z@���@��@��K@tV�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1A� �A�oA�(�A�&�A�(�A�&�A�$�A�&�A�&�A��A�+A�1'A�1'A�33A�1'A�+A�{A�dZA�Q�Aƺ^AƑhA�p�A�A�A�;dA�;dA�?}A�E�A�-A��A���AŬAŧ�AŲ-Aŧ�Ať�Aŝ�A�jA���A��FA�7LA��mA��\A��A�9XA���A�E�A���A�K�A���A��RA��A�A�A��DA�;dA��PA�-A��A�S�A�l�A��+A��PA�=qA��#A��A�=qA�%A��7A�%A�9XA�ĜA�M�A��yA���A��#A��A� �A��#A�+A�-A�+A���A�A��-A�5?A�%A���A�`BA�p�A���A��AyoAv�RApn�An�Al��Aj�DAf{Aa?}A^JAY��AUt�AR  APJAOx�AN-AK�AH5?AF^5AC�AB��AAdZA@jA?;dA;ƨA:bNA9;dA8  A7��A69XA4E�A2ȴA2M�A0M�A/�^A.=qA,�uA+S�A)�FA(�DA'�-A%��A$ȴA$��A$�DA#x�A"�!A!7LA {A 1'A�hA7LA��AM�A/A�AƨA&�A�yA��AVAI�A7LA��A��A33A�uA�A�PAK�A
�A
M�A
bA	p�A��AVAv�A5?AhsA�A�A�yA�A�A ȴ@��@�V@�@��9@�I�@� �@�b@���@���@�@��/@�M�@���@@���@���@��@��@���@��/@�33@���@䛦@�ƨ@���@�n�@�J@��#@��D@�C�@�^5@�@�Ĝ@��;@ۍP@�ȴ@�~�@�5?@�`B@ؓu@��@�t�@�ȴ@��@� �@���@�V@���@�`B@�%@���@�z�@�33@�o@�
=@��y@ΰ!@·+@�=q@͑h@��/@̼j@�r�@���@�dZ@�
=@���@ʟ�@ʇ+@��@ɑh@�hs@��@���@Ȭ@�(�@�ƨ@Ǖ�@�t�@ǍP@ǝ�@Ǯ@ǥ�@ǶF@��
@Ǯ@�+@��#@Ł@�V@���@� �@�\)@��y@¸R@+@�n�@�5?@�@���@��h@�X@��@���@��@�`B@�x�@�`B@��@�5?@���@��@���@�Ĝ@��@��m@�S�@��!@�M�@�@��7@��@�%@�Ĝ@�b@��\@�@��-@���@��@���@��@��@��R@�n�@���@��`@�j@�1@��m@��
@��
@�b@�Q�@��
@�S�@��y@��H@��!@�"�@�"�@�o@��!@�=q@�&�@�bN@��;@��@�+@��@���@�n�@�M�@�5?@�@���@�O�@��9@�C�@��H@�S�@��w@��@�1'@���@��m@���@��@�1'@���@��@���@��-@�x�@�/@��@�Ĝ@���@��@�bN@�b@��m@���@���@��P@�|�@�S�@��@��!@��\@�n�@�V@�5?@���@��9@��m@�dZ@�33@�o@���@�ȴ@���@�=q@���@�hs@�&�@�/@�&�@��@��9@�I�@�C�@�ȴ@��+@�^5@�5?@�@��@��@��j@�A�@�1'@�j@��D@��u@��u@���@� �@���@�t�@�33@��H@���@���@�n�@���@�X@���@��j@��9@��9@��@��u@�r�@�(�@���@���@�1'@��
@�t�@�"�@��H@�ȴ@��!@�^5@�{@��@�hs@�?}@�7L@���@���@��D@�I�@� �@�1@�ƨ@��F@��@�
=@�=q@��#@��7@�x�@�hs@�X@�&�@��/@��9@��@��@��@��@��@��D@�Z@���@��@��K@tV�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�HB	8RB
8RB
��B
�?B
�wB
��B
��B
�B
�)B
�NB
��B
��B
��B
��BVB�B �B"�B$�B>wB:^B.B�B�B{B�BoBoB �B/B2-B5?B33B)�BuB��B�B�TB��BĜB�HB�`B�B�B��B�B�)BŢB�3B��B��B��Bx�B_;BS�B8RB�B{BVB
��B
�B
�^B
��B
��B
��B
�\B
�B
dZB
K�B
F�B
6FB
)�B	��B	�XB	��B	ffB	^5B	`BB	K�B	.B	1B�B�TB��B��B�
B��B��BÖB�jB�LB�-B�9B�FB�FB�?B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LBBBŢB��B��B�#B�
BǮBĜB�wB�XB�B�B�B�B��B��B��B�B�B�!B�'B�'B�B�-B�FB�?B�?B�LB�LB�XB�LB�RB�dB�^B�}BBŢBƨBÖB��B�}BÖBĜBƨBȴBȴBȴBȴBȴBȴBɺB��B��B��B��B�B�)B�BB�;B�HB�NB�`B�fB�mB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	%B	1B		7B	JB	hB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	"�B	"�B	#�B	&�B	,B	,B	-B	-B	,B	+B	+B	+B	,B	-B	/B	/B	/B	0!B	0!B	1'B	2-B	5?B	:^B	=qB	?}B	@�B	A�B	D�B	F�B	H�B	J�B	J�B	K�B	L�B	P�B	O�B	Q�B	R�B	R�B	R�B	R�B	T�B	W
B	XB	ZB	ZB	ZB	\)B	]/B	`BB	aHB	cTB	e`B	l�B	n�B	r�B	u�B	v�B	y�B	x�B	y�B	{�B	}�B	~�B	�B	�%B	�7B	�=B	�=B	�PB	�PB	�PB	�VB	�PB	�=B	�1B	�=B	�JB	�PB	�bB	�bB	�bB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�RB	�wB	ÖB	ƨB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�sB	�yB	�sB	�sB	�mB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB

=B
DB
DB
PB
�B
�B
*e2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�HB	8RB
8RB
��B
�?B
�wB
��B
��B
�B
�)B
�NB
��B
��B
��B
��BVB�B �B"�B$�B>wB:^B.B�B�B{B�BoBoB �B/B2-B5?B33B)�BuB��B�B�TB��BĜB�HB�`B�B�B��B�B�)BŢB�3B��B��B��Bx�B_;BS�B8RB�B{BVB
��B
�B
�^B
��B
��B
��B
�\B
�B
dZB
K�B
F�B
6FB
)�B	��B	�XB	��B	ffB	^5B	`BB	K�B	.B	1B�B�TB��B��B�
B��B��BÖB�jB�LB�-B�9B�FB�FB�?B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LBBBŢB��B��B�#B�
BǮBĜB�wB�XB�B�B�B�B��B��B��B�B�B�!B�'B�'B�B�-B�FB�?B�?B�LB�LB�XB�LB�RB�dB�^B�}BBŢBƨBÖB��B�}BÖBĜBƨBȴBȴBȴBȴBȴBȴBɺB��B��B��B��B�B�)B�BB�;B�HB�NB�`B�fB�mB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	%B	1B		7B	JB	hB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	"�B	"�B	#�B	&�B	,B	,B	-B	-B	,B	+B	+B	+B	,B	-B	/B	/B	/B	0!B	0!B	1'B	2-B	5?B	:^B	=qB	?}B	@�B	A�B	D�B	F�B	H�B	J�B	J�B	K�B	L�B	P�B	O�B	Q�B	R�B	R�B	R�B	R�B	T�B	W
B	XB	ZB	ZB	ZB	\)B	]/B	`BB	aHB	cTB	e`B	l�B	n�B	r�B	u�B	v�B	y�B	x�B	y�B	{�B	}�B	~�B	�B	�%B	�7B	�=B	�=B	�PB	�PB	�PB	�VB	�PB	�=B	�1B	�=B	�JB	�PB	�bB	�bB	�bB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�RB	�wB	ÖB	ƨB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�sB	�yB	�sB	�sB	�mB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB

=B
DB
DB
PB
�B
�B
*e2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190550                              AO  ARCAADJP                                                                    20181005190550    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190550  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190550  QCF$                G�O�G�O�G�O�8000            