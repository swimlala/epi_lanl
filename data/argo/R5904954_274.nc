CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:51Z creation      
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
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005191751  20181005191751  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @������1   @���'ҏ�@5B�\(���d|bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`�Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C��C�  C��C�  C��3C�  C��C��C�  C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C�  C��C��C�  C��3C�  C�  C��C�  C��3C�  C��C��C�  C�  C��3C��3C�  C�  C��C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3D y�D  D� DfD� D  Dy�D�3D� D  Dy�D��Dy�D��Dy�D��D� D	fD	�fD
  D
y�D
��Dy�D�3Dy�D  D�fD  Dy�D  D�fD��Dy�D��D� DfD� D�3Dy�D��D� D�D��DfD�fD  D� D  Dy�DfD� D  D�fD  D�fD  Dy�D  D� D  Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D#  D#y�D$  D$� D%fD%�fD&  D&� D'fD'�fD(fD(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-�fD.  D.y�D.��D/y�D0  D0� D0��D1y�D2  D2y�D2��D3� D4fD4� D5  D5� D6  D6� D7  D7�fD8  D8y�D9  D9y�D9��D:y�D:�3D;y�D<  D<� D<��D=� D>  D>� D?fD?�fD@  D@y�DA  DA�fDB  DBy�DC  DC� DC��DD� DE  DEy�DFfDF� DF��DG� DH  DH� DI  DI� DI��DJ� DK  DK� DK��DL� DM  DM�fDN  DN� DOfDO� DP  DP� DQ  DQ�fDRfDR� DR��DS� DTfDT�fDU  DU� DU��DV� DWfDW�fDXfDX�fDY  DYy�DZ  DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`fD`� Da  Da� Da��Db�fDc  Dc� Dd  Dd�fDe  De� Df  Dfy�Dg  Dg� Dh  Dh� Dh��Di� DjfDj��Dk�Dk�fDk��Dly�Dl��Dmy�DnfDn�fDo  Do� Dp  Dp� DqfDq�fDr  Dr�fDs  Dsy�Ds��Dty�Du  Du�fDv  Dv� Dw  Dw� Dw�3Dy��D�-qD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A���A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQz�BYz�Ba{Bi{Bp�Bx�B��=B��=B��=B��=B��=B��=B��=B��=B��pB��=B��=B��=B�W
B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�W
B��=B�=B��=B��=C ECECECECEC
EC+�CECECECECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<+�C>+�C@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\^�C^^�C`^�CbECdECfECh^�Cj^�ClECnECpECrECtECvECxECz+�C|EC~EC�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C��C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�/\C�/\C�/\C�"�C�"�C�"�C��C�"�C�/\C�"�C�/\C�"�C��C�"�C�/\C�/\C�"�C��C��C��C��C��C��C��C��C��C��C��C��C�"�C�/\C�/\C�"�C��C�"�C�"�C�/\C�"�C��C�"�C�/\C�/\C�"�C�"�C��C��C�"�C�"�C�/\C�/\C�/\C�/\C�/\C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C��D 
�D ��DHD�HD�D�HDHD��D{D�HDHD��D
�D��D
�D��D
�D�HD	�D	��D
HD
��D
�D��D{D��DHD��DHD��DHD��D
�D��D
�D�HD�D�HD{D��D
�D�HDD�D�D��DHD�HDHD��D�D�HDHD��DHD��DHD��DHD�HDHD��D
�D��D 
�D ��D!
�D!��D"
�D"��D#HD#��D$HD$�HD%�D%��D&HD&�HD'�D'��D(�D(�HD)HD)�HD*HD*�HD+�D+�HD,HD,�HD-HD-��D.HD.��D/
�D/��D0HD0�HD1
�D1��D2HD2��D3
�D3�HD4�D4�HD5HD5�HD6HD6�HD7HD7��D8HD8��D9HD9��D:
�D:��D;{D;��D<HD<�HD=
�D=�HD>HD>�HD?�D?��D@HD@��DAHDA��DBHDB��DCHDC�HDD
�DD�HDEHDE��DF�DF�HDG
�DG�HDHHDH�HDIHDI�HDJ
�DJ�HDKHDK�HDL
�DL�HDMHDM��DNHDN�HDO�DO�HDPHDP�HDQHDQ��DR�DR�HDS
�DS�HDT�DT��DUHDU�HDV
�DV�HDW�DW��DX�DX��DYHDY��DZHDZ��D[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_��D`�D`�HDaHDa�HDb
�Db��DcHDc�HDdHDd��DeHDe�HDfHDf��DgHDg�HDhHDh�HDi
�Di�HDj�Dj�DkDk��Dl
�Dl��Dm
�Dm��Dn�Dn��DoHDo�HDpHDp�HDq�Dq��DrHDr��DsHDs��Dt
�Dt��DuHDu��DvHDv�HDwHDw�HDx{Dy��D�6D�ۅ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��FA��9A��FA��RA��FA��9A��RA��wA��wA���A���A��wA��wA���A��wA���A�ĜA�ĜA�ĜA�ĜA�ƨA�A�A�ȴA���A�ƨA�ƨA�ƨA�ĜA��wA��FA��!A��A���A���A���A���A���A��PA�9XA���A�l�A�&�A��;A�bNA��/A�x�A�O�A�&�A�ƨA���A��\A�JA��A�jA�\)A�|�A���A�9XA�
=A��A�
=A�ffA�dZA���A�dZA��A�bNA��A���A���A�`BA�p�A�9XA��A�z�A�=qA�
=A�E�A�l�A��A��A�5?A���A�M�A��A��DA�&�A��A��+A��HA�A��A�$�A��9A���A�ffA�ĜA�ȴA�VA�A�A�+A�A�7LA�z�A���A�JA�;dA~�A{��AyVAv��As�mAr�!AqVAohsAk|�AjȴAj9XAh��Ae�Ad5?Ab1A`I�A_ƨA_��A_�7A_hsA_%A^1A]"�AZ�AX�9AW\)AUC�ATȴAT�DAS�hAR��AP��AOVAM�AL��ALJA3p�A21'A0�RA/�A.�A,ZA*��A)��A(M�A'l�A&�RA&v�A%��A#�A"ZA!��A!�A ~�A�-A�jA��AdZA;dAoA��A��A��A  A�A�AS�A�/A9XA�^A�/A��A?}AȴA��A�mA+Az�AbA
=A�A;dA	�hA��An�A{A��A�A�A�A�;A��A�A��A�\Av�A�^AO�A �A ZA 1@�
=@�/@�bN@�;d@�@���@�M�@�5?@��;@@�w@�&�@�!@�@�A�@�"�@�+@�x�@�?}@�"�@���@�G�@��@�$�@���@ى7@���@�%@أ�@�@�v�@�J@պ^@�I�@���@щ7@�hs@�O�@�7L@�7L@�V@д9@�j@�b@϶F@�l�@�@��#@�Q�@�K�@�n�@�`B@ȋD@�Z@�+@�`B@�dZ@��H@�^5@�{@���@���@���@�t�@�C�@��H@�ff@�-@�%@��@�
=@�^5@�{@�&�@�Ĝ@���@�bN@�I�@�A�@�Q�@�z�@�l�@�J@���@�b@�t�@�
=@���@�v�@�=q@�{@��@��^@�p�@�%@�z�@��@��H@��#@��!@��\@�^5@���@���@��j@�  @�\)@��@���@�%@��/@��w@��@�dZ@�
=@���@���@�E�@��@��@���@�O�@��@�%@���@��j@�j@��w@���@��R@�n�@��@�@��@���@�M�@�$�@�@��^@���@�9X@� �@�1'@�Z@�Z@� �@��F@�ƨ@�|�@��@���@�^5@���@�@�p�@�G�@�G�@��u@��P@���@��m@� �@�(�@�(�@�j@�I�@��F@�o@��@�v�@�@���@��/@�A�@�(�@��@��P@���@���@�C�@��H@�~�@���@���@��h@�O�@��h@��@�&�@��@��@���@�Z@�9X@�  @��@���@��@�t�@�o@���@���@���@���@���@��\@�v�@�^5@�E�@��@��#@��#@��@�p�@���@�I�@�1'@�b@�1'@�(�@�  @���@�l�@���@�l�@�o@���@��+@�{@��@�hs@�7L@�V@�V@�V@���@��/@��@�j@�Q�@�9X@��
@���@�ƨ@���@�|�@�l�@�\)@�o@��+@�V@�M�@��@���@�&�@��`@��/@���@���@�Q�@��
@���@�dZ@�;d@��L@}Vm@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��FA��9A��FA��RA��FA��9A��RA��wA��wA���A���A��wA��wA���A��wA���A�ĜA�ĜA�ĜA�ĜA�ƨA�A�A�ȴA���A�ƨA�ƨA�ƨA�ĜA��wA��FA��!A��A���A���A���A���A���A��PA�9XA���A�l�A�&�A��;A�bNA��/A�x�A�O�A�&�A�ƨA���A��\A�JA��A�jA�\)A�|�A���A�9XA�
=A��A�
=A�ffA�dZA���A�dZA��A�bNA��A���A���A�`BA�p�A�9XA��A�z�A�=qA�
=A�E�A�l�A��A��A�5?A���A�M�A��A��DA�&�A��A��+A��HA�A��A�$�A��9A���A�ffA�ĜA�ȴA�VA�A�A�+A�A�7LA�z�A���A�JA�;dA~�A{��AyVAv��As�mAr�!AqVAohsAk|�AjȴAj9XAh��Ae�Ad5?Ab1A`I�A_ƨA_��A_�7A_hsA_%A^1A]"�AZ�AX�9AW\)AUC�ATȴAT�DAS�hAR��AP��AOVAM�AL��ALJA3p�A21'A0�RA/�A.�A,ZA*��A)��A(M�A'l�A&�RA&v�A%��A#�A"ZA!��A!�A ~�A�-A�jA��AdZA;dAoA��A��A��A  A�A�AS�A�/A9XA�^A�/A��A?}AȴA��A�mA+Az�AbA
=A�A;dA	�hA��An�A{A��A�A�A�A�;A��A�A��A�\Av�A�^AO�A �A ZA 1@�
=@�/@�bN@�;d@�@���@�M�@�5?@��;@@�w@�&�@�!@�@�A�@�"�@�+@�x�@�?}@�"�@���@�G�@��@�$�@���@ى7@���@�%@أ�@�@�v�@�J@պ^@�I�@���@щ7@�hs@�O�@�7L@�7L@�V@д9@�j@�b@϶F@�l�@�@��#@�Q�@�K�@�n�@�`B@ȋD@�Z@�+@�`B@�dZ@��H@�^5@�{@���@���@���@�t�@�C�@��H@�ff@�-@�%@��@�
=@�^5@�{@�&�@�Ĝ@���@�bN@�I�@�A�@�Q�@�z�@�l�@�J@���@�b@�t�@�
=@���@�v�@�=q@�{@��@��^@�p�@�%@�z�@��@��H@��#@��!@��\@�^5@���@���@��j@�  @�\)@��@���@�%@��/@��w@��@�dZ@�
=@���@���@�E�@��@��@���@�O�@��@�%@���@��j@�j@��w@���@��R@�n�@��@�@��@���@�M�@�$�@�@��^@���@�9X@� �@�1'@�Z@�Z@� �@��F@�ƨ@�|�@��@���@�^5@���@�@�p�@�G�@�G�@��u@��P@���@��m@� �@�(�@�(�@�j@�I�@��F@�o@��@�v�@�@���@��/@�A�@�(�@��@��P@���@���@�C�@��H@�~�@���@���@��h@�O�@��h@��@�&�@��@��@���@�Z@�9X@�  @��@���@��@�t�@�o@���@���@���@���@���@��\@�v�@�^5@�E�@��@��#@��#@��@�p�@���@�I�@�1'@�b@�1'@�(�@�  @���@�l�@���@�l�@�o@���@��+@�{@��@�hs@�7L@�V@�V@�V@���@��/@��@�j@�Q�@�9X@��
@���@�ƨ@���@�|�@�l�@�\)@�o@��+@�V@�M�@��@���@�&�@��`@��/@���@���@�Q�@��
@���@�dZ@�;d@��L@}Vm@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BC�BC�BC�BC�BD�BC�BC�BE�BD�BE�BD�BD�BD�BD�BC�BD�BE�BE�BF�BE�BE�BC�BD�BF�BH�BJ�BK�BK�BM�BP�BS�BT�BT�BW
BZB\)B`BBaHBhsBv�B�DB��B��B��B��B�B�9B�9B�!B��B��B��B��B��B��B��B�VB�B�Bs�Bn�BiyBdZB^5BZBW
BP�BO�BQ�BK�BG�B@�B:^B9XB6FB2-B-B$�B�BhB�B�^B��B��B��B��B��B�oB�bB�=B{�Bo�BffBQ�B@�B33B.B#�BuBB
��B
�NB
ɺB
�?B
��B
�{B
�DB
}�B
o�B
[#B
D�B
2-B
 �B
�B
PB
  B	�sB	�B	�B	�`B	��B	ȴB	�jB	�LB	�LB	�LB	�RB	�RB	�FB	�'B	�B	��B	�uB	�DB	~�B	{�B	z�B	v�B	r�B	iyB	_;B	ZB	S�B	N�BÖB�}B�dB�XB�LB�9B�'B�B�B��B��B��B��B��B��B��B��B�{B�uB�uB�bB�JB�\B�oB�oB�uB�hB�VB�1B�%B�B�B� B� B� B|�B{�B{�Bz�Bz�Bx�Bv�Bt�Bt�Bu�Bs�Bs�Bp�Bo�Bo�Bs�Bv�Bq�Bp�Bo�Bo�Bo�Bo�Bq�Br�Bq�Bq�Bq�Br�Br�Br�Bv�Bv�Bv�Bt�Br�Bo�Bk�BffBdZBbNB_;B]/B^5B^5B_;B_;BbNBbNBjBl�Bm�Bq�Bs�Br�Bt�By�Bz�B~�B�B�B�B�B�%B�VB�\B�\B�\B�bB�\B�bB�hB�hB�oB�uB�uB�{B��B��B��B��B��B�B�B�B�9B�dB�qB�wB�wB�wB��BĜBĜBŢBƨBȴBɺB��B��B��B��B��B��B��B��B�
B�B�#B�;B�mB�B��B��B	B	1B	
=B	
=B	JB	VB	bB	oB	uB	{B	�B	�B	�B	�B	�B	&�B	,B	,B	,B	+B	)�B	)�B	-B	.B	.B	7LB	<jB	@�B	@�B	A�B	D�B	F�B	I�B	L�B	M�B	N�B	P�B	T�B	W
B	XB	\)B	]/B	`BB	dZB	jB	l�B	m�B	u�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�%B	�1B	�=B	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�3B	�FB	�RB	�XB	�dB	�}B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�5B	�BB	�;B	�;B	�BB	�NB	�ZB	�`B	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
%B
1B
�B
�B
-)22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BC�BC�BC�BC�BD�BC�BC�BE�BD�BE�BD�BD�BD�BD�BC�BD�BE�BE�BF�BE�BE�BC�BD�BF�BH�BJ�BK�BK�BM�BP�BS�BT�BT�BW
BZB\)B`BBaHBhsBv�B�DB��B��B��B��B�B�9B�9B�!B��B��B��B��B��B��B��B�VB�B�Bs�Bn�BiyBdZB^5BZBW
BP�BO�BQ�BK�BG�B@�B:^B9XB6FB2-B-B$�B�BhB�B�^B��B��B��B��B��B�oB�bB�=B{�Bo�BffBQ�B@�B33B.B#�BuBB
��B
�NB
ɺB
�?B
��B
�{B
�DB
}�B
o�B
[#B
D�B
2-B
 �B
�B
PB
  B	�sB	�B	�B	�`B	��B	ȴB	�jB	�LB	�LB	�LB	�RB	�RB	�FB	�'B	�B	��B	�uB	�DB	~�B	{�B	z�B	v�B	r�B	iyB	_;B	ZB	S�B	N�BÖB�}B�dB�XB�LB�9B�'B�B�B��B��B��B��B��B��B��B��B�{B�uB�uB�bB�JB�\B�oB�oB�uB�hB�VB�1B�%B�B�B� B� B� B|�B{�B{�Bz�Bz�Bx�Bv�Bt�Bt�Bu�Bs�Bs�Bp�Bo�Bo�Bs�Bv�Bq�Bp�Bo�Bo�Bo�Bo�Bq�Br�Bq�Bq�Bq�Br�Br�Br�Bv�Bv�Bv�Bt�Br�Bo�Bk�BffBdZBbNB_;B]/B^5B^5B_;B_;BbNBbNBjBl�Bm�Bq�Bs�Br�Bt�By�Bz�B~�B�B�B�B�B�%B�VB�\B�\B�\B�bB�\B�bB�hB�hB�oB�uB�uB�{B��B��B��B��B��B�B�B�B�9B�dB�qB�wB�wB�wB��BĜBĜBŢBƨBȴBɺB��B��B��B��B��B��B��B��B�
B�B�#B�;B�mB�B��B��B	B	1B	
=B	
=B	JB	VB	bB	oB	uB	{B	�B	�B	�B	�B	�B	&�B	,B	,B	,B	+B	)�B	)�B	-B	.B	.B	7LB	<jB	@�B	@�B	A�B	D�B	F�B	I�B	L�B	M�B	N�B	P�B	T�B	W
B	XB	\)B	]/B	`BB	dZB	jB	l�B	m�B	u�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�%B	�1B	�=B	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�3B	�FB	�RB	�XB	�dB	�}B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�5B	�BB	�;B	�;B	�BB	�NB	�ZB	�`B	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
%B
1B
�B
�B
-)22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191751                              AO  ARCAADJP                                                                    20181005191751    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191751  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191751  QCF$                G�O�G�O�G�O�8000            