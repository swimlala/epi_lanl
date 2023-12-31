CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:16Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140816  20181024140816  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               >A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׿$���1   @׿%Q��b@2�KƧ��c�hr� �1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      >A   A   A   @�ff@�  @���A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C	�fC  C  C  C  C  C�C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb�Cd  Ce�fCh  Cj  Cl  Cn  Cp�Cr�Ct�Cv  Cx  Cz  C|  C~�C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� DfD� D  Dy�D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&fD&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D.��D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DIy�DJ  DJ�fDK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DS��DTy�DU  DU� DV  DV� DW  DW� DW��DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�A�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@ȣ�A�RA$Q�AE�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BĽpBȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�=B�=B��=B�=B��pB��=C ECECECECEC
+�CECECECECEC^�CEC+�CECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECD+�CFECHECJECLECNECPECRECTECVECXECZEC\EC^+�C`ECb^�CdECf+�ChECjEClECnECp^�Cr^�Ct^�CvECxECzEC|EC~^�C�"�C�"�C�/\C�"�C�"�C�"�C��C��C�"�C�"�C�/\C�"�C�/\C�"�C�"�C�"�C�"�C��C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HD	HD	��D
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD
�D�HDHD�HD�D�HDHD��DHD�HD HD �HD!�D!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&�D&�HD'HD'�HD(HD(�HD)HD)�HD*HD*��D+HD+�HD,HD,�HD-HD-�HD.HD.�HD/
�D/��D0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDB�DB�HDCHDC�HDDHDD�HDEHDE�HDFHDF��DGHDG�HDHHDH�HDIHDI��DJHDJ��DKHDK�HDLHDL��DMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDS�DS�HDT
�DT��DUHDU�HDVHDV�HDWHDW�HDX
�DX�HDY�DY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa��DbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm��DnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs��DtHDt�HDuHDu�HDvHDv�HDwHDw�HDw�{Dy��D�J>D�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A� �A�1'A�33A�/A�+A�-A�/A�1'A�5?A�C�A�O�A�dZAݕ�A���A��#A��/A��#A�ĜA݋DA�bNA�9XA���A�C�A۴9AۃA�^5A�oA�-A��#Aٙ�A�=qA���Aء�A�&�A��A�K�A��A�JA�oAө�A���A�O�A���A�oA�^5A�+Aϟ�A��A��A��A�VA˸RA���A�&�A���A�-A���A���Aƴ9AƏ\A�ȴA�%A�A��A�  A�  A��A�O�A�$�A�G�A��9A���A�1'A���A�  A�XA��A� �A��A���A���A�l�A�XA��A��DA��^A�%A�hsA���A�C�A�{A��A��TA���A��9A��DA�M�A��/A��!A��A��9A�ĜA�\)A��A���A���A�p�A�^5A��A�^5A�(�A�+A�TA}�A}�FA|�yAx��Aup�As�Arn�AnjAmXAlA�Aix�Ae�Ac�A`�!A_�FA^=qA\�A\1AZVAWdZAU�AU�AT�HAT{AQ�;AM�AL�\AKXAH=qAF-AE\)AD�AC�
ABZA@�yA@I�A@{A>v�A=7LA<�A<jA9�A6�DA4~�A3�hA2jA1O�A/hsA,jA*��A*bNA*A)��A)S�A)A(�jA&�9A%�A$5?A"ȴA!�TA!dZA ĜA ��A ^5A r�A��A|�A�HA{AE�A�;A�^AG�AS�A33A%A1A"�Al�A��A�\A �A�^A�HA�A�#AE�AAG�A��A��A
��A	�mA
ȴA	��A��A	A�A�!AƨAn�A�FA�jAbNA{A?}A�A��A�A ff@��;@���@���@��@��F@���@���@�u@���@���@��@�^@�`B@��`@�9@�z�@��m@��@�hs@�9@�D@�F@�@���@�9X@��H@�V@�E�@�=q@�{@���@�`B@�b@��@��@��@؃@�1'@׾w@�;d@�E�@��/@�G�@�-@�t�@ʰ!@���@�&�@��`@�G�@�^5@Å@�"�@�I�@�1@��H@�~�@�K�@�dZ@�|�@��R@��T@�bN@���@�
=@�  @��
@�~�@�E�@���@�V@��9@��@�ff@���@�V@��@���@�j@���@�33@���@���@�`B@���@�9X@��
@��w@�dZ@���@�$�@��#@���@���@��^@��^@�hs@���@�I�@�1'@��@��@��@�C�@�"�@���@���@��h@���@��u@�b@��F@��@�"�@���@�E�@�-@�J@���@��h@�?}@���@�(�@��@�\)@��@��@��@��+@���@��h@�Ĝ@� �@��@�ƨ@���@�t�@��@��@��H@��@��R@��+@�5?@��T@�hs@�G�@��`@�Ĝ@��@�1@��@�@��@��y@��R@���@��h@�X@�X@�G�@�G�@�?}@�7L@�&�@���@�r�@�9X@� �@�  @��;@�dZ@�{@�@�x�@��/@���@��@�z�@�Q�@�Q�@�Q�@��;@�ȴ@���@��@���@��9@��u@���@���@�Ĝ@��j@��j@��9@��@���@�Z@��@��m@��m@��@��y@�V@�?}@�bN@�j@�z�@�Ĝ@��
@��P@�K�@���@��@�@�
=@�+@��@��\@���@�v�@�@��^@�O�@��@��@���@�33@��!@��+@�5?@�/@���@�Ĝ@��j@��j@��9@��@�  @�t�@�C�@�o@��@���@�v�@��@��T@���@���@���@���@���@���@��-@��@�r�@�  @���@��@�C�@��@�
=@�^5@���@��7@�7L@��j@�I�@��@��	@vH�@_1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A� �A�1'A�33A�/A�+A�-A�/A�1'A�5?A�C�A�O�A�dZAݕ�A���A��#A��/A��#A�ĜA݋DA�bNA�9XA���A�C�A۴9AۃA�^5A�oA�-A��#Aٙ�A�=qA���Aء�A�&�A��A�K�A��A�JA�oAө�A���A�O�A���A�oA�^5A�+Aϟ�A��A��A��A�VA˸RA���A�&�A���A�-A���A���Aƴ9AƏ\A�ȴA�%A�A��A�  A�  A��A�O�A�$�A�G�A��9A���A�1'A���A�  A�XA��A� �A��A���A���A�l�A�XA��A��DA��^A�%A�hsA���A�C�A�{A��A��TA���A��9A��DA�M�A��/A��!A��A��9A�ĜA�\)A��A���A���A�p�A�^5A��A�^5A�(�A�+A�TA}�A}�FA|�yAx��Aup�As�Arn�AnjAmXAlA�Aix�Ae�Ac�A`�!A_�FA^=qA\�A\1AZVAWdZAU�AU�AT�HAT{AQ�;AM�AL�\AKXAH=qAF-AE\)AD�AC�
ABZA@�yA@I�A@{A>v�A=7LA<�A<jA9�A6�DA4~�A3�hA2jA1O�A/hsA,jA*��A*bNA*A)��A)S�A)A(�jA&�9A%�A$5?A"ȴA!�TA!dZA ĜA ��A ^5A r�A��A|�A�HA{AE�A�;A�^AG�AS�A33A%A1A"�Al�A��A�\A �A�^A�HA�A�#AE�AAG�A��A��A
��A	�mA
ȴA	��A��A	A�A�!AƨAn�A�FA�jAbNA{A?}A�A��A�A ff@��;@���@���@��@��F@���@���@�u@���@���@��@�^@�`B@��`@�9@�z�@��m@��@�hs@�9@�D@�F@�@���@�9X@��H@�V@�E�@�=q@�{@���@�`B@�b@��@��@��@؃@�1'@׾w@�;d@�E�@��/@�G�@�-@�t�@ʰ!@���@�&�@��`@�G�@�^5@Å@�"�@�I�@�1@��H@�~�@�K�@�dZ@�|�@��R@��T@�bN@���@�
=@�  @��
@�~�@�E�@���@�V@��9@��@�ff@���@�V@��@���@�j@���@�33@���@���@�`B@���@�9X@��
@��w@�dZ@���@�$�@��#@���@���@��^@��^@�hs@���@�I�@�1'@��@��@��@�C�@�"�@���@���@��h@���@��u@�b@��F@��@�"�@���@�E�@�-@�J@���@��h@�?}@���@�(�@��@�\)@��@��@��@��+@���@��h@�Ĝ@� �@��@�ƨ@���@�t�@��@��@��H@��@��R@��+@�5?@��T@�hs@�G�@��`@�Ĝ@��@�1@��@�@��@��y@��R@���@��h@�X@�X@�G�@�G�@�?}@�7L@�&�@���@�r�@�9X@� �@�  @��;@�dZ@�{@�@�x�@��/@���@��@�z�@�Q�@�Q�@�Q�@��;@�ȴ@���@��@���@��9@��u@���@���@�Ĝ@��j@��j@��9@��@���@�Z@��@��m@��m@��@��y@�V@�?}@�bN@�j@�z�@�Ĝ@��
@��P@�K�@���@��@�@�
=@�+@��@��\@���@�v�@�@��^@�O�@��@��@���@�33@��!@��+@�5?@�/@���@�Ĝ@��j@��j@��9@��@�  @�t�@�C�@�o@��@���@�v�@��@��T@���@���@���@���@���@���@��-@��@�r�@�  @���@��@�C�@��@�
=@�^5@���@��7@�7L@��j@�I�@��@��	@vH�@_1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
p�B
p�B
q�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
t�B
y�B
}�B
�%B
��B
�jB
ÖB
ƨB
ȴB
ɺB
ȴB
ǮB
ƨB
B
��B
ǮB
�}B
ƨB
�NB
�yB
�B
�B
�B
��B
��B
��B
��BB\B+B<jBA�BL�B[#BbNBo�Bu�Br�B}�B�DB�+B��B��BȴB�B�B��BB�B,B)�B'�B8RBL�BR�B`BBx�B�VB�B�jB�B�uB�BXB0!B�BbB%B��B��B�B�`B�5B��B��B��B��B��B�DBt�Bq�Bp�B~�Bv�BgmB�B�B|�Bk�BcTBQ�BG�B49B&�B�B�B  B
�ZB
�/B
��B
�?B
�=B
z�B
aHB
P�B
C�B
F�B
D�B
.B
�B
	7B	��B	�`B	�/B	��B	�}B	��B	��B	�=B	�B	y�B	q�B	m�B	gmB	W
B	J�B	D�B	@�B	9XB	+B	�B	hB	DB��B��B�B�B�B�`B�;B�/B�#B��B��B��BɺB��B�qB�^B�FB�'B�-B�B��B��B��B��B��B��B��B��B�hB�DB�+B�B�B�%B�B�B�B��B��B��B�3B�!B�B��B��B��B�B�B�B��B�B��B��B��B�B�!B�3B�RB�^BBŢBŢBƨBŢBÖBƨB�HB�B�B�B�B�B�mB�BB�;B�B�B��B��B��B��B��B��B��BȴBƨBĜB��B�jB�FB�RB�?B�?B�RB�XB�XB�^B�^B�dB�dB�qB�wB�}B��BÖBȴB��B��B��B��B��B��B��B��B��B�B�B�/B�;B�BB�HB�NB�TB�fB�TB�B��B��B�}B�3B��B��B��B�!B�XB�LB�B��B��B��B��B�B�B�B�BĜB�/B��B	B	B��B�B�HB�mB��B		7B	VB	hB	�B	�B	$�B	$�B	$�B	$�B	&�B	)�B	-B	0!B	49B	7LB	8RB	;dB	B�B	G�B	J�B	K�B	K�B	K�B	K�B	M�B	R�B	W
B	YB	\)B	]/B	]/B	^5B	_;B	`BB	cTB	e`B	jB	m�B	r�B	t�B	u�B	x�B	|�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�JB	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�FB	�LB	�XB	�XB	�XB	�XB	�jB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	B	B	B	B	B	B	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB
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
PB
PB
PB
PB
JB
JB
PB
VB
\B
bB
hB
{B
FB
"�B
6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
p�B
p�B
q�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
t�B
y�B
}�B
�%B
��B
�jB
ÖB
ƨB
ȴB
ɺB
ȴB
ǮB
ƨB
B
��B
ǮB
�}B
ƨB
�NB
�yB
�B
�B
�B
��B
��B
��B
��BB\B+B<jBA�BL�B[#BbNBo�Bu�Br�B}�B�DB�+B��B��BȴB�B�B��BB�B,B)�B'�B8RBL�BR�B`BBx�B�VB�B�jB�B�uB�BXB0!B�BbB%B��B��B�B�`B�5B��B��B��B��B��B�DBt�Bq�Bp�B~�Bv�BgmB�B�B|�Bk�BcTBQ�BG�B49B&�B�B�B  B
�ZB
�/B
��B
�?B
�=B
z�B
aHB
P�B
C�B
F�B
D�B
.B
�B
	7B	��B	�`B	�/B	��B	�}B	��B	��B	�=B	�B	y�B	q�B	m�B	gmB	W
B	J�B	D�B	@�B	9XB	+B	�B	hB	DB��B��B�B�B�B�`B�;B�/B�#B��B��B��BɺB��B�qB�^B�FB�'B�-B�B��B��B��B��B��B��B��B��B�hB�DB�+B�B�B�%B�B�B�B��B��B��B�3B�!B�B��B��B��B�B�B�B��B�B��B��B��B�B�!B�3B�RB�^BBŢBŢBƨBŢBÖBƨB�HB�B�B�B�B�B�mB�BB�;B�B�B��B��B��B��B��B��B��BȴBƨBĜB��B�jB�FB�RB�?B�?B�RB�XB�XB�^B�^B�dB�dB�qB�wB�}B��BÖBȴB��B��B��B��B��B��B��B��B��B�B�B�/B�;B�BB�HB�NB�TB�fB�TB�B��B��B�}B�3B��B��B��B�!B�XB�LB�B��B��B��B��B�B�B�B�BĜB�/B��B	B	B��B�B�HB�mB��B		7B	VB	hB	�B	�B	$�B	$�B	$�B	$�B	&�B	)�B	-B	0!B	49B	7LB	8RB	;dB	B�B	G�B	J�B	K�B	K�B	K�B	K�B	M�B	R�B	W
B	YB	\)B	]/B	]/B	^5B	_;B	`BB	cTB	e`B	jB	m�B	r�B	t�B	u�B	x�B	|�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�JB	�\B	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�3B	�FB	�LB	�XB	�XB	�XB	�XB	�jB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	B	B	B	B	B	B	ĜB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
DB
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
PB
PB
PB
PB
JB
JB
PB
VB
\B
bB
hB
{B
FB
"�B
6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140816                              AO  ARCAADJP                                                                    20181024140816    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140816  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140816  QCF$                G�O�G�O�G�O�0               