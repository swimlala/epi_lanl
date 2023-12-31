CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:53Z AOML 3.0 creation; 2016-08-07T22:44:58Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221453  20160807154459  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               	A   AO  5288_9005_009                   2C  D   APEX                            6530                            072314                          846 @���ZO�1   @��o��@(>��"���cm�"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    	A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ��B��B  BffB��B(ffB0ffB733B?��BG��BP  BZffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D��D�FfD��3D�� D�fD�9�D���D�� D�fD�C3D���D�� D���D�,�DچfD�� D� D�FfD�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{A
=A'
=AG
=Ag
=A��A��A��A��AÅAӅA�A�Q�B�\B	\)BB(�B!\)B*(�B2(�B8��BA\)BI\)BQB\(�BaBiBqByB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HC p�Cp�Cp�Cp�Cp�C
�>C�>Cp�Cp�CW
CW
Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR��DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Du�Dy�)D��D�TzD��GD��D�zD�G�D���D��D�$zD�QGD���D��D�
�D�:�DڔzD��D�D�TzD�GD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��HA��HA��TA��TA��TA��`A��TA��TA��HA��#A���Aޟ�A�r�A���A�O�A�=qA���A�oAա�A�oA� �A�$�A�VA�n�A���AŅA�7LA��jA��A���A�33A��;A�S�A���A�ffA�p�A��/A��FA���A���A�/A�\)A��`A���A���A�"�A��yA��jA��wA�ȴA�t�A���A���A���A�ȴA�v�A��A���A�n�A��-A���A���A���A�dZAz��Au7LAp{Al�/Ad��AZ1'AS��AM��AJ{AH��AHjAFȴAD �AB��AAA?�#A=�TA;�A9��A8�yA8�\A8E�A8I�A8z�A7�A8-A7�#A7�A61'A5l�A3�mA2��A/�A.  A-C�A,�yA,9XA+XA*n�A)hsA)oA(ȴA(��A'��A'dZA'S�A'33A&��A&n�A%�;A%7LA$z�A#�FA"ȴA"jA!�#A!
=A bNA�wA+A��Az�A{A�/A=qA  A�-AG�Av�AffA�A�hAl�A;dAG�A&�Ar�A$�A�mA  A��A�A��A7LAz�A�`AVAA�A�A��AO�A�A��A�!AȴA��A�9A��Az�A  A�-A�AS�A+A��AĜA^5A�A�^A�PAhsAC�Av�A1A�#AAdZA��Az�AA�A$�A  A�TA�FAC�A�HAZA�A�mA�-A�At�A?}A7LA/A&�AA
ffA
(�A	�TA	�PA	�A	XA�A�A��AVA��A��A%A�!AA�A  A�wAdZA��A�A�\A�AdZAȴAI�A�#A��A��A��A�A�A ��A ��A bNA =q@��m@���@�33@���@���@�G�@�z�@�ƨ@�l�@�"�@��R@��@�hs@��`@�t�@�+@�o@���@�^5@�J@�O�@�z�@��m@�w@�P@�\)@�o@�J@�@�/@�r�@�  @@�"�@�!@�V@��@�7L@�Ĝ@�u@�I�@�F@���@�-@�h@��@蛦@�@� �@�\)@�E�@�&�@�9X@�F@��@��@��#@�V@�r�@���@�C�@��H@ޗ�@�$�@݉7@���@�(�@ڧ�@���@�p�@؋D@�(�@��m@��@�@Ցh@�&�@��@�b@Ұ!@�5?@��#@��@�9X@�dZ@Χ�@�ff@���@��@��@�33@�o@��@ʧ�@��#@�`B@�V@�A�@Ǿw@�|�@�{@ũ�@���@�bN@��m@Å@�l�@¸R@�E�@���@�&�@��@�9X@�dZ@��+@�J@�p�@���@��@��@�bN@�I�@�(�@�b@���@�|�@���@���@��-@��-@��h@�hs@�G�@�&�@���@�r�@� �@��m@���@�\)@��R@�{@���@���@�X@�O�@���@��9@�1'@��
@��@�33@��H@��\@�^5@�-@���@�7L@���@��9@�j@�9X@�1@�1@���@�+@�
=@���@���@�V@�=q@�-@�@��@�r�@�1'@�b@��;@��@�K�@�o@��H@���@�n�@��@�X@�%@�b@�S�@�+@��H@�~�@�5?@��@���@�`B@���@��@��;@���@�S�@�
=@��+@��@��@��D@� �@��
@��@�;d@�o@�@���@�=q@�@��T@�x�@��@���@��j@�r�@��@��P@�K�@�o@��H@��R@�n�@�E�@�@�/@���@���@�j@�1@�K�@�"�@�
=@��H@���@�E�@�{@���@�/@���@��j@�9X@��F@���@��@�t�@�S�@�7L@��/@�l�@|�@q�7@f�y@`bN@Y��@R��@Kt�@B=q@9&�@/�w@)hs@#�
@��@��@��@^5@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��HA��HA��HA��TA��TA��TA��`A��TA��TA��HA��#A���Aޟ�A�r�A���A�O�A�=qA���A�oAա�A�oA� �A�$�A�VA�n�A���AŅA�7LA��jA��A���A�33A��;A�S�A���A�ffA�p�A��/A��FA���A���A�/A�\)A��`A���A���A�"�A��yA��jA��wA�ȴA�t�A���A���A���A�ȴA�v�A��A���A�n�A��-A���A���A���A�dZAz��Au7LAp{Al�/Ad��AZ1'AS��AM��AJ{AH��AHjAFȴAD �AB��AAA?�#A=�TA;�A9��A8�yA8�\A8E�A8I�A8z�A7�A8-A7�#A7�A61'A5l�A3�mA2��A/�A.  A-C�A,�yA,9XA+XA*n�A)hsA)oA(ȴA(��A'��A'dZA'S�A'33A&��A&n�A%�;A%7LA$z�A#�FA"ȴA"jA!�#A!
=A bNA�wA+A��Az�A{A�/A=qA  A�-AG�Av�AffA�A�hAl�A;dAG�A&�Ar�A$�A�mA  A��A�A��A7LAz�A�`AVAA�A�A��AO�A�A��A�!AȴA��A�9A��Az�A  A�-A�AS�A+A��AĜA^5A�A�^A�PAhsAC�Av�A1A�#AAdZA��Az�AA�A$�A  A�TA�FAC�A�HAZA�A�mA�-A�At�A?}A7LA/A&�AA
ffA
(�A	�TA	�PA	�A	XA�A�A��AVA��A��A%A�!AA�A  A�wAdZA��A�A�\A�AdZAȴAI�A�#A��A��A��A�A�A ��A ��A bNA =q@��m@���@�33@���@���@�G�@�z�@�ƨ@�l�@�"�@��R@��@�hs@��`@�t�@�+@�o@���@�^5@�J@�O�@�z�@��m@�w@�P@�\)@�o@�J@�@�/@�r�@�  @@�"�@�!@�V@��@�7L@�Ĝ@�u@�I�@�F@���@�-@�h@��@蛦@�@� �@�\)@�E�@�&�@�9X@�F@��@��@��#@�V@�r�@���@�C�@��H@ޗ�@�$�@݉7@���@�(�@ڧ�@���@�p�@؋D@�(�@��m@��@�@Ցh@�&�@��@�b@Ұ!@�5?@��#@��@�9X@�dZ@Χ�@�ff@���@��@��@�33@�o@��@ʧ�@��#@�`B@�V@�A�@Ǿw@�|�@�{@ũ�@���@�bN@��m@Å@�l�@¸R@�E�@���@�&�@��@�9X@�dZ@��+@�J@�p�@���@��@��@�bN@�I�@�(�@�b@���@�|�@���@���@��-@��-@��h@�hs@�G�@�&�@���@�r�@� �@��m@���@�\)@��R@�{@���@���@�X@�O�@���@��9@�1'@��
@��@�33@��H@��\@�^5@�-@���@�7L@���@��9@�j@�9X@�1@�1@���@�+@�
=@���@���@�V@�=q@�-@�@��@�r�@�1'@�b@��;@��@�K�@�o@��H@���@�n�@��@�X@�%@�b@�S�@�+@��H@�~�@�5?@��@���@�`B@���@��@��;@���@�S�@�
=@��+@��@��@��D@� �@��
@��@�;d@�o@�@���@�=q@�@��T@�x�@��@���@��j@�r�@��@��P@�K�@�o@��H@��R@�n�@�E�@�@�/@���@���@�j@�1@�K�@�"�@�
=@��H@���@�E�@�{@���@�/@���@��j@�9X@��F@���@��@�t�G�O�@�7L@��/@�l�@|�@q�7@f�y@`bN@Y��@R��@Kt�@B=q@9&�@/�w@)hs@#�
@��@��@��@^5@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
]/B
\)B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
aHB
aHB
dZB
m�B
v�B
�oB
�jB	7Bp�B�VBn�Bk�Bn�BffB� B��B��B�B�{B�B��B�{BǮB�yB�sB�`B�BB�/B�#BÖB��B[#BaHB&�B%B��B�yB�HB�5B�)B�B�jB��B��B�dB�mB��B�Bn�B.BB
��B
��B
l�B
.B
B	�B	�B	�VB	t�B	=qB	B�;BȴB�B	B	%B	�B	�B	�B	�B	�B	�B	-B	C�B	ZB	v�B	}�B	�B	�uB	��B	ƨB	�ZB	��B
B	��B	��B
B	��B
B
B
DB
DB
\B
�B
�B
�B
1'B
>wB
F�B
K�B
K�B
L�B
L�B
M�B
P�B
N�B
L�B
H�B
C�B
H�B
H�B
B�B
@�B
?}B
=qB
;dB
?}B
B�B
@�B
<jB
;dB
;dB
:^B
<jB
<jB
=qB
>wB
>wB
B�B
H�B
I�B
G�B
F�B
N�B
VB
\)B
cTB
bNB
aHB
]/B
gmB
o�B
k�B
k�B
l�B
n�B
q�B
q�B
x�B
|�B
� B
�B
� B
~�B
|�B
{�B
z�B
y�B
x�B
w�B
w�B
u�B
s�B
r�B
r�B
q�B
p�B
jB
iyB
hsB
jB
m�B
jB
jB
jB
jB
iyB
iyB
hsB
ffB
dZB
cTB
cTB
bNB
aHB
`BB
`BB
_;B
^5B
^5B
]/B
\)B
ZB
XB
W
B
VB
W
B
W
B
W
B
ZB
XB
W
B
VB
T�B
S�B
R�B
P�B
P�B
O�B
N�B
M�B
L�B
M�B
J�B
F�B
D�B
B�B
A�B
A�B
@�B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
?}B
?}B
>wB
>wB
=qB
=qB
<jB
;dB
;dB
;dB
:^B
9XB
9XB
7LB
7LB
6FB
6FB
6FB
5?B
49B
49B
33B
33B
33B
2-B
2-B
1'B
1'B
0!B
0!B
/B
/B
/B
.B
.B
-B
-B
,B
,B
,B
+B
)�B
)�B
(�B
(�B
'�B
'�B
'�B
&�B
%�B
#�B
#�B
#�B
"�B
"�B
!�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
oB
hB
hB
hB
hB
bB
bB
bB
bB
bB
bB
bB
\B
VB
PB
PB
VB
PB
JB
JB
DB

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
PB
JB
DB
JB
PB
\B
\B
VB
VB
PB
PB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
bB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
oB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
+B
0!B
9XB
B�B
H�B
L�B
O�B
S�B
W
B
]/B
cTB
jB
o�B
s�B
v�B
y�B
{�B
~�B
�B
�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
]B
\B
]B
]B
]B
]B
^B
_B
_B
a-B
a*B
d?B
mrB
v�B
�RB
�IB	Bp�B�2BnuBkbBnuBf@B�B�cBʞB�B�VB��B�[B�UBǈB�VB�MB�=B�B�B��B�oB��BZ�BaB&�B�B��B�OB�!B�B��B��B�BB��B��B�<B�BB�^B��BnoB-�B�B
��B
��B
lfB
-�B
�B	��B	��B	�4B	t�B	=SB	�B�BȖB�B	�B	B	cB	�B	�B	|B	`B	{B	,�B	CsB	Y�B	v�B	}�B	��B	�MB	��B	�B	�0B	��B
�B	��B	��B
 �B	��B
�B
�B
B
B
1B
`B
xB
�B
0�B
>HB
FzB
K�B
K�B
L�B
L�B
M�B
P�B
N�B
L�B
H�B
ChB
H�B
H�B
BdB
@UB
?OB
=BB
;5B
?NB
BbB
@TB
<<B
;6B
;6B
:-B
<<B
<<B
=@B
>IB
>HB
BbB
H�B
I�B
GB
FwB
N�B
U�B
[�B
c#B
bB
aB
\�B
g>B
omB
kUB
kUB
lZB
nhB
qzB
q|B
x�B
|�B
�B
��B
�B
~�B
|�B
{�B
z�B
y�B
x�B
w�B
w�B
u�B
s�B
r}B
r~B
qxB
prB
jNB
iHB
hCB
jNB
m`B
jOB
jNB
jMB
jOB
iHB
iHB
hAB
f4B
d(B
c#B
c#B
bB
aB
`B
`B
_	B
^B
^B
\�B
[�B
Y�B
W�B
V�B
U�B
V�B
V�B
V�B
Y�B
W�B
V�B
U�B
T�B
S�B
R�B
P�B
P�B
O�B
N�B
M�B
L�B
M�B
J�B
FxB
DjB
B_B
AXB
AXB
@QB
@SB
@PB
@TB
AWB
@RB
@SB
@RB
?LB
?MB
>FB
>EB
=BB
=@B
<:B
;4B
;2B
;3B
:.B
9'B
9'B
7B
7B
6B
6B
6B
5B
4B
4	B
3B
3B
3B
1�B
1�B
0�B
0�B
/�B
/�B
.�B
.�B
.�B
-�B
-�B
,�B
,�B
+�B
+�B
+�B
*�B
)�B
)�B
(�B
(�B
'�B
'�B
'�B
&�B
%�B
#�B
#�B
#�B
"�B
"�B
!�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
zB
|B
zB
tB
nB
nB
nB
nB
hB
`B
aB
[B
\B
_B
\B
VB
MB
PB
IB
CB
CB
<B
5B
6B
5B
6B
/B
2B
2B
0B
1B
2B
1B
+B
$B
 B
B
$B
B
B
B
B

B

B

B


B

B

B


B

	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
$B
#B
"B
B
B
B
B
B
*B
*B
%B
$B
B
B
#B
$B
#B
#B
#B
*B
/B
0B
0B
-B
0B
1B
2B
0B
0B
1B
)B
1B
1B
.B
1B
1B
6B
<B
;B
=B
<B
9B
;B
;B
;B
;B
;B
;B
BB
<B
AB
IB
JB
HB
HB
IB
GB
OB
OB
MB
OB
KB
LB
LB
MB
MB
TB
YB
[B
`B
cB
bB
hB
iB
iB
fB
gB
lB
mB
sB
rB
sB
qB
tB
tB
tB
tB
yB
yB
wB
{B
yB
~B
B
�B
~B
B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�G�O�B
%�B
*�B
/�B
9%B
B[B
H�B
L�B
O�B
S�B
V�B
\�B
c!B
jIB
ojB
s�B
v�B
y�B
{�B
~�B
��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544592016080715445920160807154459  AO  ARCAADJP                                                                    20150226221453    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221453  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221453  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154459  IP                  G�O�G�O�G�O�                