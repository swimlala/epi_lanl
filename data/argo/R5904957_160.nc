CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:34Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140834  20181024140834  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�פ�S��1   @�ץ""4v@5���Q��c���v�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @,��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D�fDfD� DfD�fDfD� D  D� DfD�fD  Dy�D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D �fD!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8�fD9fD9�fD:fD:� D:��D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy~�D�H�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>{@���@ȣ�A�RA$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B	{B{B{B!z�B)z�B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��pB��=B��=B��=B��=B��=B��=B��=B��=B��=B�W
B�W
B��=B��=B��=B��=B��pBĊ=BȊ=B�W
BЊ=BԊ=B؊=BܽpB��=B�=B�=B�=B��=B�=B��=B��=C ECECECECEC
ECECECEC+�CECECECECECEC EC"EC$EC&EC(EC*EC,EC.EC0EC2EC4EC6EC8EC:EC<EC>EC@ECBECDECF^�CHECJECLECNECPECRECTECVECXECZEC\EC^EC`+�CbECdECf^�ChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C��C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�"�D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD��DHD�HDHD�HDHD��D�D�HD�D��D�D�HDHD�HD�D��DHD��DHD�HDHD�HDHD�HDHD��D
�D�HDHD�HDHD�HDHD�HDHD��DHD�HDHD�HD HD ��D!HD!��D"HD"�HD#HD#�HD$HD$�HD%HD%�HD&
�D&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/
�D/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4�D4�HD5HD5�HD6HD6�HD7HD7�HD8HD8��D9�D9��D:�D:�HD;
�D;�HD<HD<�HD=HD=�HD>HD>�HD?
�D?�HD@HD@��DAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDS�DS��DTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[��D\�D\�HD]HD]��D^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDn�Dn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDt��DuHDu�HDvHDv�HDwHDw�HDw�{Dy� D�Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�bA�bA�bA�oA�{A�{A��A��A�VA�JA�VA�JA�JA�JA�oA�+A�7LAԁA��;A��HA���A�ȴA�AԲ-Aԡ�Aԇ+A�l�A�VA��AсA���Aɇ+Aŗ�A�"�A�A���A�9XA��!A��mA�1A�%A�bA�~�A�l�A�bA��^A��A��!A�`BA�1'A���A��A�;dA�v�A���A�Q�A��A�E�A��TA��A��jA���A�+A�ȴA��7A�r�A�n�A�ZA�+A��yA�E�A�%A���A���A�&�A��A��A�O�A�~�A���A�v�A��#A�&�A�ZA�-A��A�p�A���A��+A���A�I�A��\A�5?A��DA��+A���A7LA~�!A}��A|v�Aw7LAsoAp��AnM�Ak��Ak"�Ajv�Ah�!Af��A`z�A^�jA^1'A\M�AX1'AU/AT~�AS�#ARĜAQ|�AP��AO/AM�wALJAJ~�AI�hAI�AH�jAHE�AG��AF�RAE|�AD�AD{AC��ABĜAAx�A@JA?7LA>I�A=;dA;�-A:�9A:z�A9�A7�#A5�7A37LA2=qA1��A0ȴA0  A/&�A-�;A+�
A)�-A(��A(~�A'�TA'�hA'&�A&��A$��A!G�A =qA�;A�-A7LAA�/A��Ar�A=qA�#A�hA`BA�A��A^5A�-A�wAVA��A��A33A�+A�A��A��A�^AZA�FA	��A	`BA	&�A�A�AffAE�A(�A��A��AffA �@�dZ@��y@�M�@���@�x�@�{@��h@�%@���@��@�E�@�%@� �@�!@�`B@��@��@�~�@���@�+@�&�@�w@��@�$�@�h@�b@�hs@���@ۍP@���@�  @��y@�E�@��@��T@�p�@��`@���@�ƨ@�S�@�@�~�@�Q�@ΰ!@��#@�x�@��`@���@���@�&�@Ǿw@ǶF@�|�@�E�@���@�  @Å@�=q@��@��j@���@�^5@��@���@�x�@�7L@�Ĝ@�j@�(�@�  @�ƨ@�dZ@�ȴ@�^5@���@�O�@���@���@�j@���@�S�@�@��R@�~�@�5?@���@���@��j@�r�@�bN@�Z@�I�@�9X@�9X@�1'@�1'@�1'@�1'@�(�@� �@��;@�\)@�V@��T@��h@�G�@��@���@��D@��@�z�@�bN@�9X@���@��w@�|�@�K�@�o@��R@�v�@�E�@��T@�V@�  @��y@�V@�O�@�r�@�ƨ@���@�S�@���@���@���@�l�@���@���@���@��+@�V@�J@���@��h@��@�O�@�O�@�O�@�?}@��@���@�%@�%@��`@��/@��j@��j@���@��@�bN@��@�33@�@��H@���@�-@�J@��T@���@���@�/@���@�(�@���@���@�"�@��@�^5@�-@�$�@��@�{@�J@��@��#@��^@�O�@�%@���@��9@���@��D@�r�@�bN@�Z@�Q�@�I�@�A�@�A�@� �@���@��F@�S�@���@���@�v�@�=q@��@���@�p�@�x�@�hs@�7L@�&�@��@�V@�%@���@�Ĝ@���@��D@�j@�b@���@��F@�dZ@���@���@��@��h@��@�p�@�X@�/@�%@��j@��@��P@�dZ@�\)@�S�@�C�@�"�@�o@��H@���@�=q@��@�p�@���@��@��9@���@�z�@�z�@�z�@�z�@�j@�bN@�Z@�Z@�Z@�Z@�Z@�bN@�bN@�Z@�Q�@�1'@�1@���@��m@��m@��;@��w@���@���@�ff@�-@�@��@��#@���@��^@��@�/@�&�@�V@���@��/@�Ĝ@��@�A�@��@yf�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�oA�bA�bA�bA�oA�{A�{A��A��A�VA�JA�VA�JA�JA�JA�oA�+A�7LAԁA��;A��HA���A�ȴA�AԲ-Aԡ�Aԇ+A�l�A�VA��AсA���Aɇ+Aŗ�A�"�A�A���A�9XA��!A��mA�1A�%A�bA�~�A�l�A�bA��^A��A��!A�`BA�1'A���A��A�;dA�v�A���A�Q�A��A�E�A��TA��A��jA���A�+A�ȴA��7A�r�A�n�A�ZA�+A��yA�E�A�%A���A���A�&�A��A��A�O�A�~�A���A�v�A��#A�&�A�ZA�-A��A�p�A���A��+A���A�I�A��\A�5?A��DA��+A���A7LA~�!A}��A|v�Aw7LAsoAp��AnM�Ak��Ak"�Ajv�Ah�!Af��A`z�A^�jA^1'A\M�AX1'AU/AT~�AS�#ARĜAQ|�AP��AO/AM�wALJAJ~�AI�hAI�AH�jAHE�AG��AF�RAE|�AD�AD{AC��ABĜAAx�A@JA?7LA>I�A=;dA;�-A:�9A:z�A9�A7�#A5�7A37LA2=qA1��A0ȴA0  A/&�A-�;A+�
A)�-A(��A(~�A'�TA'�hA'&�A&��A$��A!G�A =qA�;A�-A7LAA�/A��Ar�A=qA�#A�hA`BA�A��A^5A�-A�wAVA��A��A33A�+A�A��A��A�^AZA�FA	��A	`BA	&�A�A�AffAE�A(�A��A��AffA �@�dZ@��y@�M�@���@�x�@�{@��h@�%@���@��@�E�@�%@� �@�!@�`B@��@��@�~�@���@�+@�&�@�w@��@�$�@�h@�b@�hs@���@ۍP@���@�  @��y@�E�@��@��T@�p�@��`@���@�ƨ@�S�@�@�~�@�Q�@ΰ!@��#@�x�@��`@���@���@�&�@Ǿw@ǶF@�|�@�E�@���@�  @Å@�=q@��@��j@���@�^5@��@���@�x�@�7L@�Ĝ@�j@�(�@�  @�ƨ@�dZ@�ȴ@�^5@���@�O�@���@���@�j@���@�S�@�@��R@�~�@�5?@���@���@��j@�r�@�bN@�Z@�I�@�9X@�9X@�1'@�1'@�1'@�1'@�(�@� �@��;@�\)@�V@��T@��h@�G�@��@���@��D@��@�z�@�bN@�9X@���@��w@�|�@�K�@�o@��R@�v�@�E�@��T@�V@�  @��y@�V@�O�@�r�@�ƨ@���@�S�@���@���@���@�l�@���@���@���@��+@�V@�J@���@��h@��@�O�@�O�@�O�@�?}@��@���@�%@�%@��`@��/@��j@��j@���@��@�bN@��@�33@�@��H@���@�-@�J@��T@���@���@�/@���@�(�@���@���@�"�@��@�^5@�-@�$�@��@�{@�J@��@��#@��^@�O�@�%@���@��9@���@��D@�r�@�bN@�Z@�Q�@�I�@�A�@�A�@� �@���@��F@�S�@���@���@�v�@�=q@��@���@�p�@�x�@�hs@�7L@�&�@��@�V@�%@���@�Ĝ@���@��D@�j@�b@���@��F@�dZ@���@���@��@��h@��@�p�@�X@�/@�%@��j@��@��P@�dZ@�\)@�S�@�C�@�"�@�o@��H@���@�=q@��@�p�@���@��@��9@���@�z�@�z�@�z�@�z�@�j@�bN@�Z@�Z@�Z@�Z@�Z@�bN@�bN@�Z@�Q�@�1'@�1@���@��m@��m@��;@��w@���@���@�ff@�-@�@��@��#@���@��^@��@�/@�&�@�V@���@��/@�Ĝ@��@�A�@��@yf�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B%B%B%B%B+B+B+B%B+B%BB%BBBB1BoB�Bp�BB%B+B+B1B+B+B+B%BB��B��B	7B{B$�B+B0!B:^B;dB=qB?}BC�BH�B[#B_;B_;B_;Be`BjBiyBiyBhsBgmBl�Bk�Bk�Bl�BhsB^5B]/B^5BW
BI�B<jB$�B�B�B�B�BuBbB	7B�B�B��B�XB��B��B�{B�7Bp�BffB_;BQ�BA�B2-B�B
��B
�B
�dB
�{B
u�B
gmB
bNB
J�B
@�B
?}B
!�B
�B
!�B
�B
uB	��B	�mB	�BB	��B	�}B	�dB	�FB	��B	��B	z�B	o�B	jB	_;B	J�B	;dB	6FB	33B	.B	'�B	!�B	�B	oB	JB	+B	B	B��B��B��B��B�B�B�B�B�mB�TB�;B�)B�B�
B��B��B��B��BȴBÖB�wB�jB�^B�RB�LB�9B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�hB�hB�bB�\B�PB�VB�\B�bB�\B�\B�VB�VB�\B�bB�hB�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�FB�LB�RB�RB�XB�dB�}B��B��BBBɺB��B��B��B��B�B�#B�/B�NB�NB�TB�sB�B�B�B��B��B	  B		7B	JB	VB	\B	\B	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	'�B	(�B	+B	-B	33B	5?B	8RB	9XB	;dB	=qB	A�B	A�B	C�B	D�B	E�B	E�B	F�B	H�B	R�B	ZB	aHB	e`B	gmB	iyB	jB	l�B	n�B	m�B	m�B	m�B	n�B	o�B	o�B	p�B	p�B	p�B	p�B	q�B	r�B	t�B	x�B	}�B	�B	�B	�1B	�PB	�VB	�VB	�PB	�JB	�VB	�hB	�hB	�hB	�hB	�hB	�bB	�bB	�bB	�bB	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�VB	�\B	�\B	�\B	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�^B	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�#B	�)B	�;B	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
+B
1B

=B

=B

=B

=B

=B

=B
DB
PB
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%B%B%B%B+B+B+B%B+B%BB%BBBB1BoB�Bp�BB%B+B+B1B+B+B+B%BB��B��B	7B{B$�B+B0!B:^B;dB=qB?}BC�BH�B[#B_;B_;B_;Be`BjBiyBiyBhsBgmBl�Bk�Bk�Bl�BhsB^5B]/B^5BW
BI�B<jB$�B�B�B�B�BuBbB	7B�B�B��B�XB��B��B�{B�7Bp�BffB_;BQ�BA�B2-B�B
��B
�B
�dB
�{B
u�B
gmB
bNB
J�B
@�B
?}B
!�B
�B
!�B
�B
uB	��B	�mB	�BB	��B	�}B	�dB	�FB	��B	��B	z�B	o�B	jB	_;B	J�B	;dB	6FB	33B	.B	'�B	!�B	�B	oB	JB	+B	B	B��B��B��B��B�B�B�B�B�mB�TB�;B�)B�B�
B��B��B��B��BȴBÖB�wB�jB�^B�RB�LB�9B�'B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�hB�hB�bB�\B�PB�VB�\B�bB�\B�\B�VB�VB�\B�bB�hB�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�3B�FB�LB�RB�RB�XB�dB�}B��B��BBBɺB��B��B��B��B�B�#B�/B�NB�NB�TB�sB�B�B�B��B��B	  B		7B	JB	VB	\B	\B	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	&�B	'�B	(�B	+B	-B	33B	5?B	8RB	9XB	;dB	=qB	A�B	A�B	C�B	D�B	E�B	E�B	F�B	H�B	R�B	ZB	aHB	e`B	gmB	iyB	jB	l�B	n�B	m�B	m�B	m�B	n�B	o�B	o�B	p�B	p�B	p�B	p�B	q�B	r�B	t�B	x�B	}�B	�B	�B	�1B	�PB	�VB	�VB	�PB	�JB	�VB	�hB	�hB	�hB	�hB	�hB	�bB	�bB	�bB	�bB	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�VB	�\B	�\B	�\B	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�^B	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�#B	�)B	�;B	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
+B
1B

=B

=B

=B

=B

=B

=B
DB
PB
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140834                              AO  ARCAADJP                                                                    20181024140834    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140834  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140834  QCF$                G�O�G�O�G�O�0               