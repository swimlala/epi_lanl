CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:10Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190610  20181005190610  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @� )�Q�1   @� *'Ғ@1�~��"��c��hr�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  D   D � D  D� D  Dy�D  D�fD  D� D  D� D  Dy�D��Dy�D  D� D��D	� D
fD
�fDfD� D  D�fDfD� D  D� D  D� D  D�fD  D� D��D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D��D� DfD�fD  D� D  D� D��Dy�D   D �fD!  D!y�D"  D"� D#  D#�fD$  D$y�D%  D%� D%��D&� D'  D'� D(  D(� D)fD)� D*  D*�fD+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D0��D1y�D1��D2y�D2��D3y�D4  D4� D5  D5� D5��D6y�D6��D7y�D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DA��DB� DC  DC� DC��DDy�DE  DE�fDF  DF� DG  DGy�DH  DH�fDI  DI�fDJfDJ� DK  DKy�DL  DL� DM  DMy�DN  DN�fDOfDO�fDP  DPy�DP��DQy�DR  DR�fDSfDS� DS��DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\y�D\��D]� D^  D^�fD_  D_y�D`  D`� Da  Da� Db  Db�fDc  Dc� Dd  Dd�fDe  De� DffDf�fDg  Dg� Dh  Dhy�Dh��Diy�Di��Djy�Dk  Dk� DlfDl� Dm  Dm� DnfDn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� DsfDs�fDt  Dt� Du  Du� Du��Dvy�Dw  Dw�fDwٚDyt{D�MqD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@ȣ�AQ�A$Q�ADQ�AdQ�A���A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bh�Bq{By{B��=B��pB��pB��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��pB��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�W
B�W
B�=B��=B��=C EC+�CECECEC
ECECECECECECECEC^�CECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECFECHECJECLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECn+�CpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C��C�"�C�"�C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C�"�D HD �HDHD�HDHD��DHD��DHD�HDHD�HDHD��D
�D��DHD�HD	
�D	�HD
�D
��D�D�HDHD��D�D�HDHD�HDHD�HDHD��DHD�HD
�D�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HD
�D�HD�D��DHD�HDHD�HD
�D��D HD ��D!HD!��D"HD"�HD#HD#��D$HD$��D%HD%�HD&
�D&�HD'HD'�HD(HD(�HD)�D)�HD*HD*��D+�D+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0��D1
�D1��D2
�D2��D3
�D3��D4HD4�HD5HD5�HD6
�D6��D7
�D7��D8HD8�HD9HD9�HD:HD:��D;HD;�HD<HD<�HD=HD=�HD>HD>�HD?�D?�HD@HD@�HDAHDA�HDB
�DB�HDCHDC�HDD
�DD��DEHDE��DFHDF�HDGHDG��DHHDH��DIHDI��DJ�DJ�HDKHDK��DLHDL�HDMHDM��DNHDN��DO�DO��DPHDP��DQ
�DQ��DRHDR��DS�DS�HDT
�DT�HDUHDU��DVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\�D\��D]
�D]�HD^HD^��D_HD_��D`HD`�HDaHDa�HDbHDb��DcHDc�HDdHDd��DeHDe�HDf�Df��DgHDg�HDhHDh��Di
�Di��Dj
�Dj��DkHDk�HDl�Dl�HDmHDm�HDn�Dn�HDoHDo��DpHDp�HDqHDq�HDrHDr�HDs�Ds��DtHDt�HDuHDu�HDv
�Dv��DwHDw��Dw��Dy��D�VD�">111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A���A�ĜA�ĜA�ƨA�ƨA���A���A�ƨAʼjAʶFAʲ-Aʣ�Aʉ7A�r�A�jA�XA�9XA��mA�S�A���AȶFAȩ�Aȥ�Aș�A�hsA�M�A�?}A�/A�JAǁA�$�A�
=A��HAƝ�A��AŋDA�;dA�JA�VAčPA�"�A�%A��yA��mA��;A��/A���A¶FA�{A�bNA��A�^5A��`A�n�A���A���A��A�/A��mA�ȴA�=qA���A��A�/A��hA�VA���A�"�A���A��A��DA�$�A��TA�  A�$�A��yA�K�A��A��^A��#A�$�A�+A�p�A���A�Q�A�A�oA�
=A��^A�?}A�
=A��A���A��A��A�%A�hsA��#A���A�{A�A��9A�ȴA�ĜA��A�ȴA�S�A}��Az$�AwAt��As"�AqdZAm�Aj�DAhbNAgAedZAc�Ab��A`�9A_��A]%AX��AVAR�yAP��AO�AL�AK&�AI33AF~�ADv�ABE�AA33A@�uA>�`A;33A9G�A8bA6�DA25?A0��A0~�A0bNA0M�A/O�A.{A-x�A-dZA-K�A,��A)|�A'�^A&(�A$��A#��A#�A"�`A!�#A!G�A&�Av�A�A�7AĜAI�A��A��A�A�A&�A��A~�A1A��A�An�A��AO�A�^AE�A`BA��AdZAoAv�A��A��A
��A	|�A	33A�9A�A��A��AjA�A �A
=A+A
=Az�A$�A�;A��A\)A&�A �jA �A J@�dZ@�O�@��@��@���@�I�@��F@��R@��#@�`B@���@��;@�|�@�@�+@�~�@�p�@�F@�\@�=q@��@���@�X@�%@�ȴ@�Q�@��@�7@���@�&�@���@��@�K�@�;d@��@�hs@��`@�z�@�Z@�(�@���@�Q�@��@���@�+@�33@�t�@�S�@�C�@�"�@�;d@�C�@�+@�33@��@�"�@��H@ޏ\@�5?@���@݁@�j@��;@�t�@�+@���@�v�@�M�@�=q@�-@�J@���@��@ؼj@ش9@�9X@׶F@׍P@�dZ@֏\@���@�p�@�G�@�V@ԛ�@�b@ӍP@�;d@�ff@�{@��T@мj@ϕ�@�33@Η�@�M�@���@�?}@�V@���@���@�1'@˥�@ʸR@�-@�{@��H@�"�@�ȴ@�-@�x�@�/@��/@�bN@�ƨ@�"�@Ƈ+@�@Ł@�7L@��`@ě�@�I�@�(�@å�@�33@�
=@\@���@��#@���@��@�O�@�G�@�%@���@��u@�bN@�A�@��@��;@��@��@�ȴ@���@��\@�E�@�X@�bN@��@��;@�t�@��@���@�v�@�5?@�J@��@���@�9X@�ƨ@���@�\)@�@���@�n�@�-@�{@�{@��@�?}@��9@�  @���@�S�@�^5@���@��@��`@��/@���@�Ĝ@��j@���@��u@�r�@�I�@�ƨ@�S�@�;d@���@��!@�M�@�x�@�(�@�  @�  @��;@��P@�
=@�^5@��@�@��@�x�@�&�@���@��`@��@� �@��@�l�@�;d@�@��!@�5?@�{@���@�/@���@�Z@�bN@��D@�z�@�(�@���@��w@���@�dZ@�+@�ȴ@��!@�J@��h@�p�@�`B@�&�@���@�I�@�|�@�33@��@���@���@�n�@�-@�{@��#@��h@�?}@���@�z�@�I�@���@�
=@��R@��+@�M�@��^@���@�l�@�"�@��!@�V@�=q@�$�@���@���@���@��@�x�@�p�@�p�@�/@���@���@�z�@�Z@�1'@�ƨ@��y@�E�@���@��'@�'�@z��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A���A�ĜA�ĜA�ƨA�ƨA���A���A�ƨAʼjAʶFAʲ-Aʣ�Aʉ7A�r�A�jA�XA�9XA��mA�S�A���AȶFAȩ�Aȥ�Aș�A�hsA�M�A�?}A�/A�JAǁA�$�A�
=A��HAƝ�A��AŋDA�;dA�JA�VAčPA�"�A�%A��yA��mA��;A��/A���A¶FA�{A�bNA��A�^5A��`A�n�A���A���A��A�/A��mA�ȴA�=qA���A��A�/A��hA�VA���A�"�A���A��A��DA�$�A��TA�  A�$�A��yA�K�A��A��^A��#A�$�A�+A�p�A���A�Q�A�A�oA�
=A��^A�?}A�
=A��A���A��A��A�%A�hsA��#A���A�{A�A��9A�ȴA�ĜA��A�ȴA�S�A}��Az$�AwAt��As"�AqdZAm�Aj�DAhbNAgAedZAc�Ab��A`�9A_��A]%AX��AVAR�yAP��AO�AL�AK&�AI33AF~�ADv�ABE�AA33A@�uA>�`A;33A9G�A8bA6�DA25?A0��A0~�A0bNA0M�A/O�A.{A-x�A-dZA-K�A,��A)|�A'�^A&(�A$��A#��A#�A"�`A!�#A!G�A&�Av�A�A�7AĜAI�A��A��A�A�A&�A��A~�A1A��A�An�A��AO�A�^AE�A`BA��AdZAoAv�A��A��A
��A	|�A	33A�9A�A��A��AjA�A �A
=A+A
=Az�A$�A�;A��A\)A&�A �jA �A J@�dZ@�O�@��@��@���@�I�@��F@��R@��#@�`B@���@��;@�|�@�@�+@�~�@�p�@�F@�\@�=q@��@���@�X@�%@�ȴ@�Q�@��@�7@���@�&�@���@��@�K�@�;d@��@�hs@��`@�z�@�Z@�(�@���@�Q�@��@���@�+@�33@�t�@�S�@�C�@�"�@�;d@�C�@�+@�33@��@�"�@��H@ޏ\@�5?@���@݁@�j@��;@�t�@�+@���@�v�@�M�@�=q@�-@�J@���@��@ؼj@ش9@�9X@׶F@׍P@�dZ@֏\@���@�p�@�G�@�V@ԛ�@�b@ӍP@�;d@�ff@�{@��T@мj@ϕ�@�33@Η�@�M�@���@�?}@�V@���@���@�1'@˥�@ʸR@�-@�{@��H@�"�@�ȴ@�-@�x�@�/@��/@�bN@�ƨ@�"�@Ƈ+@�@Ł@�7L@��`@ě�@�I�@�(�@å�@�33@�
=@\@���@��#@���@��@�O�@�G�@�%@���@��u@�bN@�A�@��@��;@��@��@�ȴ@���@��\@�E�@�X@�bN@��@��;@�t�@��@���@�v�@�5?@�J@��@���@�9X@�ƨ@���@�\)@�@���@�n�@�-@�{@�{@��@�?}@��9@�  @���@�S�@�^5@���@��@��`@��/@���@�Ĝ@��j@���@��u@�r�@�I�@�ƨ@�S�@�;d@���@��!@�M�@�x�@�(�@�  @�  @��;@��P@�
=@�^5@��@�@��@�x�@�&�@���@��`@��@� �@��@�l�@�;d@�@��!@�5?@�{@���@�/@���@�Z@�bN@��D@�z�@�(�@���@��w@���@�dZ@�+@�ȴ@��!@�J@��h@�p�@�`B@�&�@���@�I�@�|�@�33@��@���@���@�n�@�-@�{@��#@��h@�?}@���@�z�@�I�@���@�
=@��R@��+@�M�@��^@���@�l�@�"�@��!@�V@�=q@�$�@���@���@���@��@�x�@�p�@�p�@�/@���@���@�z�@�Z@�1'@�ƨ@��y@�E�@���@��'@�'�@z��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�HB�HB�HB�HB�BB�;B�BB�BB�;B�;B�BB�BB�HB�NB�TB�`B�B	PB	I�B	��B	�NB
  B
B
	7B
JB
uB
�B
�B
"�B
5?B
_;B
|�B
�bB
��B
�FB
ƨB
��B
��B
�B{B&�BD�B~�B�B�1B�7B�7B�+B�Bn�B|�B�VB��B��B��B��B�B�/B��B�B/BE�BM�BN�BN�BXBZBZBXBT�BH�B:^B33B0!B5?B5?B2-B(�B �B�BbB��B�NB��BB�B�+BVB.B�B
��B
�B
�B
�sB
�/B
��B
ǮB
��B
ÖB
VB
+B
	7B	�B	�/B	ɺB	�3B	��B	�+B	gmB	YB	VB	T�B	P�B	L�B	A�B	9XB	2-B	-B	(�B	!�B	�B	{B	PB	  B�yB�NB�B��B��BB�^B�^B��B�qB�?B�3B�-B�!B�B�B�B��B�B�-B�FB�RB�wB��B�
B�B�B�
B��BɺBƨBƨBŢBŢBĜBÖBŢBÖBĜBĜBŢBƨBȴB��B�#B�sB�yB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	uB	�B	�B	#�B	$�B	"�B	�B	�B	2-B	9XB	:^B	;dB	:^B	;dB	;dB	9XB	8RB	8RB	9XB	;dB	<jB	=qB	B�B	F�B	B�B	A�B	C�B	F�B	I�B	K�B	L�B	N�B	N�B	N�B	N�B	N�B	M�B	M�B	L�B	L�B	N�B	S�B	XB	[#B	_;B	bNB	`BB	ZB	T�B	W
B	P�B	S�B	YB	YB	W
B	VB	VB	VB	S�B	S�B	T�B	VB	W
B	XB	^5B	iyB	l�B	r�B	u�B	z�B	|�B	}�B	� B	�B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	�wB	�wB	�}B	��B	��B	��B	B	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�NB	�NB	�NB	�fB	�mB	�sB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
JB
DB
�B
$t222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�HB�HB�HB�HB�BB�;B�BB�BB�;B�;B�BB�BB�HB�NB�TB�`B�B	PB	I�B	��B	�NB
  B
B
	7B
JB
uB
�B
�B
"�B
5?B
_;B
|�B
�bB
��B
�FB
ƨB
��B
��B
�B{B&�BD�B~�B�B�1B�7B�7B�+B�Bn�B|�B�VB��B��B��B��B�B�/B��B�B/BE�BM�BN�BN�BXBZBZBXBT�BH�B:^B33B0!B5?B5?B2-B(�B �B�BbB��B�NB��BB�B�+BVB.B�B
��B
�B
�B
�sB
�/B
��B
ǮB
��B
ÖB
VB
+B
	7B	�B	�/B	ɺB	�3B	��B	�+B	gmB	YB	VB	T�B	P�B	L�B	A�B	9XB	2-B	-B	(�B	!�B	�B	{B	PB	  B�yB�NB�B��B��BB�^B�^B��B�qB�?B�3B�-B�!B�B�B�B��B�B�-B�FB�RB�wB��B�
B�B�B�
B��BɺBƨBƨBŢBŢBĜBÖBŢBÖBĜBĜBŢBƨBȴB��B�#B�sB�yB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	uB	�B	�B	#�B	$�B	"�B	�B	�B	2-B	9XB	:^B	;dB	:^B	;dB	;dB	9XB	8RB	8RB	9XB	;dB	<jB	=qB	B�B	F�B	B�B	A�B	C�B	F�B	I�B	K�B	L�B	N�B	N�B	N�B	N�B	N�B	M�B	M�B	L�B	L�B	N�B	S�B	XB	[#B	_;B	bNB	`BB	ZB	T�B	W
B	P�B	S�B	YB	YB	W
B	VB	VB	VB	S�B	S�B	T�B	VB	W
B	XB	^5B	iyB	l�B	r�B	u�B	z�B	|�B	}�B	� B	�B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�dB	�jB	�wB	�wB	�wB	�}B	��B	��B	��B	B	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�NB	�TB	�TB	�NB	�NB	�NB	�fB	�mB	�sB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
JB
DB
�B
$t222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190610                              AO  ARCAADJP                                                                    20181005190610    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190610  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190610  QCF$                G�O�G�O�G�O�8000            