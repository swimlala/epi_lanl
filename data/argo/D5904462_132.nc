CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-09T18:01:35Z AOML 3.0 creation; 2016-08-07T21:51:31Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160709180135  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_132                   2C  D   APEX                            6529                            072314                          846 @׺&H�l�1   @׺&�L:@0�7KƧ��d��^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�� D�0 D�vfD�� D�fD�P D�� D���D� D�@ D�y�D�ٚD� D�P DچfD���D�fD�0 D�y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�{A
=A'
=AG
=Ag
=A��A��A��A��AÅAӅA�A�BB	BBB!B)\)B1B9BABIBQBYBaBiBqByB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HC p�C�>Cp�Cp�Cp�C
p�Cp�CW
Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C p�C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2�>C4p�C6p�C8p�C:p�C<W
C>W
C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cb�>Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�+�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�\Dy��D��D�>D��zD��D�zD�^D��D�ǮD�D�ND���D��D�D�^DڔzD���D�$zD�>D�D��z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�^5A�VA�dZA�jA�bNA�^5A�dZA�O�A�C�A�A�A�;dA�33A�;dA�=qA�VA�A�A�A���A��A��A��A��A���A���Aٰ!AٮA٬A٩�A٧�A٣�Aٟ�Aٛ�Aُ\A�~�A�^5A��yA�-A���A�ffAӉ7A���AҲ-A�7LAѥ�A�ZAЃA���Aϙ�A�/A��A��A��A΍PA�$�A�p�A�A̴9A��/A��A���Aɣ�A��A�&�Aơ�AŋDA�"�A�ffA�-A��RA��HA���A��A�A�A��!A�VA��A��A�{A�bNA��A�A�t�A� �A���A��A��A�dZA�oA��`A�ƨA��wA��A��TA��/A�&�A�/A��jA���A��FA�VA�ȴA���A�bNA�r�A�z�A�`BA��!A�ȴA�5?A}�Az�/Aw`BAr�+Aot�Amp�Aj��Agl�Adv�Aa�A_�A]
=A[�#AY
=AW�AVn�AS�hAO��AM�7AK?}AGx�AB��AA�A?dZA>�+A<��A;&�A:M�A6��A4�A2��A1��A0��A/�PA.�RA-�A-%A,5?A+`BA*��A*�A'�A%�7A$�yA$v�A#t�A"�yA!G�A ~�AC�A�7A�HA�PA�+A`BA��A�#A��A
=A�A�HA��AĜAr�A?}A��Ar�A�#A�AC�A�RA��At�A/A�A�;A�7A/A��A�uAbNA�Ap�A�A
�RA	�A	/A	oA	%A��A�A�HA��A$�AhsA�jA5?A�AK�AVA�uA�TAXAĜA �A�A��AVA ��A r�A VA v�A   @�+@���@�?}@��/@�Q�@�(�@�  @���@�33@��!@�^5@�{@���@��@��u@�(�@�t�@�
=@��!@���@�A�@��m@�@�l�@�C�@�33@�33@�"�@�
=@�R@�v�@�@�O�@��/@�u@�bN@�(�@�  @��m@�ƨ@@�@�!@�v�@��@�7@�p�@�?}@��@�bN@�ƨ@�@�@�@�~�@��#@�1@�~�@�O�@�Ĝ@��@�u@�z�@�I�@�\)@�\@��@�G�@��@���@�@���@�o@���@�^5@�%@�l�@��@�v�@�ff@�V@��@ى7@أ�@�1'@�1@�dZ@�n�@�O�@�\)@�?}@�r�@���@�ƨ@�;d@��H@���@�ff@��T@ͩ�@́@�hs@���@̃@˾w@�^5@�-@�$�@���@���@���@�G�@�%@��/@ȼj@�1'@��m@�ƨ@Ǖ�@�K�@��H@Ɨ�@�=q@��@�/@���@�Z@�1'@�1'@�ƨ@�;d@�"�@�
=@��@�V@��#@��^@��h@�?}@��D@��@�o@���@�hs@���@��u@��@�bN@�z�@��@�l�@�
=@�@�%@�Z@��F@��@�33@���@�M�@���@��@��T@��#@���@��^@���@���@��@�%@���@�r�@�A�@��@��@��@�b@�;d@�33@�33@�+@�o@���@�=q@��@�J@���@���@�x�@�hs@�G�@�%@��j@���@���@���@���@���@��D@��@�I�@��@���@�dZ@�S�@�
=@��!@��+@�$�@���@���@�z�@��m@��@�33@�
=@��y@���@��!@��!@��\@��+@�v�@�n�@�n�@�V@�@���@�Z@�Q�@�9X@�1@���@�@��@���@�ff@�=q@��T@���@��h@��@�z�@�I�@�9X@��w@���@�t�@�dZ@���@�~�@�E�@��@���@�/@��/@��@�z�@�A�@�b@�  @��;@�t�@�K�@�z�@��y@��!@�{@���@{S�@n�+@f�R@a7L@Wl�@Mp�@E�h@=��@8b@0��@)��@$1@�-@�9@��@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�\)A�^5A�VA�dZA�jA�bNA�^5A�dZA�O�A�C�A�A�A�;dA�33A�;dA�=qA�VA�A�A�A���A��A��A��A��A���A���Aٰ!AٮA٬A٩�A٧�A٣�Aٟ�Aٛ�Aُ\A�~�A�^5A��yA�-A���A�ffAӉ7A���AҲ-A�7LAѥ�A�ZAЃA���Aϙ�A�/A��A��A��A΍PA�$�A�p�A�A̴9A��/A��A���Aɣ�A��A�&�Aơ�AŋDA�"�A�ffA�-A��RA��HA���A��A�A�A��!A�VA��A��A�{A�bNA��A�A�t�A� �A���A��A��A�dZA�oA��`A�ƨA��wA��A��TA��/A�&�A�/A��jA���A��FA�VA�ȴA���A�bNA�r�A�z�A�`BA��!A�ȴA�5?A}�Az�/Aw`BAr�+Aot�Amp�Aj��Agl�Adv�Aa�A_�A]
=A[�#AY
=AW�AVn�AS�hAO��AM�7AK?}AGx�AB��AA�A?dZA>�+A<��A;&�A:M�A6��A4�A2��A1��A0��A/�PA.�RA-�A-%A,5?A+`BA*��A*�A'�A%�7A$�yA$v�A#t�A"�yA!G�A ~�AC�A�7A�HA�PA�+A`BA��A�#A��A
=A�A�HA��AĜAr�A?}A��Ar�A�#A�AC�A�RA��At�A/A�A�;A�7A/A��A�uAbNA�Ap�A�A
�RA	�A	/A	oA	%A��A�A�HA��A$�AhsA�jA5?A�AK�AVA�uA�TAXAĜA �A�A��AVA ��A r�A VA v�A   @�+@���@�?}@��/@�Q�@�(�@�  @���@�33@��!@�^5@�{@���@��@��u@�(�@�t�@�
=@��!@���@�A�@��m@�@�l�@�C�@�33@�33@�"�@�
=@�R@�v�@�@�O�@��/@�u@�bN@�(�@�  @��m@�ƨ@@�@�!@�v�@��@�7@�p�@�?}@��@�bN@�ƨ@�@�@�@�~�@��#@�1@�~�@�O�@�Ĝ@��@�u@�z�@�I�@�\)@�\@��@�G�@��@���@�@���@�o@���@�^5@�%@�l�@��@�v�@�ff@�V@��@ى7@أ�@�1'@�1@�dZ@�n�@�O�@�\)@�?}@�r�@���@�ƨ@�;d@��H@���@�ff@��T@ͩ�@́@�hs@���@̃@˾w@�^5@�-@�$�@���@���@���@�G�@�%@��/@ȼj@�1'@��m@�ƨ@Ǖ�@�K�@��H@Ɨ�@�=q@��@�/@���@�Z@�1'@�1'@�ƨ@�;d@�"�@�
=@��@�V@��#@��^@��h@�?}@��D@��@�o@���@�hs@���@��u@��@�bN@�z�@��@�l�@�
=@�@�%@�Z@��F@��@�33@���@�M�@���@��@��T@��#@���@��^@���@���@��@�%@���@�r�@�A�@��@��@��@�b@�;d@�33@�33@�+@�o@���@�=q@��@�J@���@���@�x�@�hs@�G�@�%@��j@���@���@���@���@���@��D@��@�I�@��@���@�dZ@�S�@�
=@��!@��+@�$�@���@���@�z�@��m@��@�33@�
=@��y@���@��!@��!@��\@��+@�v�@�n�@�n�@�V@�@���@�Z@�Q�@�9X@�1@���@�@��@���@�ff@�=q@��T@���@��h@��@�z�@�I�@�9X@��w@���@�t�@�dZ@���@�~�@�E�@��@���@�/@��/@��@�z�@�A�@�b@�  @��;@�t�G�O�@�z�@��y@��!@�{@���@{S�@n�+@f�R@a7L@Wl�@Mp�@E�h@=��@8b@0��@)��@$1@�-@�9@��@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�yB
�yB
�sB
�mB
�sB
�fB
�fB
�`B
�`B
�`B
�`B
�`B
�TB
�NB
�TB
�TB
�NB
�NB
�NB
�NB
�HB
�BB
�BB
�BB
�BB
�HB
�HB
�NB
�NB
�TB
�TB
�ZB
�ZB
�NB
�5B
��B
��B
�B
��B
�oB
�bB
�JB
�1B
�%B
�%B
�DB
��B
��B
��B
��B
�B
�B
�B
��B
�B
�B
�B
ŢB
�B!�B>wBcTB��B�dB��B��B�#B�`B��B1B �B5?B<jBD�B8RB(�B!�B&�B�B�B�B�BuBhBhBhBJBB�B�`B�jB��B�VB�B{�B� B~�Bm�BN�B#�BPB
��B
ɺB
��B
m�B
T�B
F�B
0!B
�B
%B	�B	�)B	��B	ĜB	�?B	��B	��B	�7B	{�B	q�B	k�B	^5B	VB	M�B	?}B	2-B	'�B	�B	{B	\B	+B��B��B��B�B�B�sB�HB�5B�/B�)B�
B�B��B�
B�
B��B��B��B��BȴBǮBǮBȴBɺB��B��B��BŢBB�jB��B�XBƨB��B��B�B�BB�NB�B��B��B	  B	B	%B		7B	VB	hB	�B	�B	�B	�B	#�B	'�B	'�B	)�B	+B	)�B	)�B	'�B	%�B	&�B	&�B	5?B	>wB	?}B	?}B	?}B	?}B	>wB	>wB	@�B	D�B	G�B	I�B	K�B	J�B	H�B	F�B	E�B	C�B	@�B	B�B	B�B	A�B	@�B	A�B	D�B	H�B	M�B	M�B	[#B	jB	m�B	y�B	� B	�B	�%B	�1B	�1B	�DB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�3B	�3B	�9B	�dB	�}B	B	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�
B	�B	�#B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�)B	�#B	�B	�B	�B	�)B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�NB	�;B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�BB	�;B	�5B	�/B	�/B	�)B	�)B	�/B	�5B	�5B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�sB	�B	�B	�yB	�sB	�sB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
{B
{B
�B
$�B
+B
0!B
6FB
:^B
?}B
G�B
M�B
T�B
ZB
^5B
cTB
jB
n�B
r�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�^B
�^B
�^B
�XB
�YB
�RB
�MB
�RB
�EB
�HB
�?B
�?B
�AB
�?B
�AB
�5B
�/B
�8B
�3B
�-B
�-B
�0B
�,B
�+B
�!B
�#B
�"B
�%B
�'B
�*B
�,B
�.B
�4B
�2B
�8B
�8B
�-B
�B
��B
�dB
��B
�pB
�NB
�BB
�+B
�B
�B
�B
�&B
�kB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ŁB
�oB!�B>TBc2B��B�>B�fBͬB�B�=B��BB �B5B<CBDuB8,B(�B!�B&�B�B�BgBYBLB@BABABB�B�B�:B�BB��B�-B��B{�B�B~�BmiBN�B#�B(B
��B
ɑB
��B
miB
T�B
F�B
/�B
YB
B	�xB	�B	ϻB	�zB	�B	��B	�^B	�B	{�B	q�B	kdB	^B	U�B	M�B	?_B	2
B	'�B	�B	YB	<B	
B��B��B��B�B�B�SB�(B�B�B�B��B��B��B��B��B��B��B̬BʡBȔBǌBǍBȔBɛBͱBͱB˦BŁB�pB�GB�aB�7BƇB��B��B��B�!B�+B�nB��B��B��B	�B	�B		B	2B	DB	YB	{B	�B	�B	#�B	'�B	'�B	)�B	*�B	)�B	)�B	'�B	%�B	&�B	&�B	5B	>NB	?VB	?WB	?UB	?WB	>PB	>NB	@\B	DuB	G�B	I�B	K�B	J�B	H�B	F�B	E{B	ClB	@\B	BhB	BfB	A`B	@ZB	AbB	DtB	H�B	M�B	M�B	Z�B	jTB	miB	y�B	�B	��B	��B	�B	�B	�B	�#B	�7B	�EB	�WB	�aB	�oB	��B	��B	��B	��B	��B	� B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�9B	�OB	�`B	�nB	�vB	ǁB	ȇB	ȄB	ɍB	ʖB	̠B	ΩB	ϱB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѿB	ѾB	ѾB	ѽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�)B	�,B	�)B	�B	�B	��B	��B	зB	ϮB	ϭB	ϯB	жB	ѽB	дB	ϮB	ϱB	ϯB	жB	ϯB	ΩB	ΪB	дB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	��B	��B	��B	��B	� B	�B	�B	� B	�B	�B	�B	�B	�B	�$B	�"B	�*B	�*B	�/B	�5B	�5B	�6B	�0B	�0B	�/B	�5B	�7B	�5B	�5B	�6B	�5B	�BB	�MB	�OB	�IB	�CB	�CB	�;B	�<B	�>B	�CB	�JB	�HB	�FB	�EB	�NB	�TB	�\B	�`B	�gB	�eB	�lB	�nB	�nB	�pB	�lB	�mB	�iB	�nB	�sB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
	B
	B
	B
	B
	G�O�B
B
IB
GB
~B
$�B
*�B
/�B
6B
:*B
?JB
G{B
M�B
T�B
Y�B
^ B
c!B
jKB
n`B
r{B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451312016080714513120160807145131  AO  ARCAADJP                                                                    20160709180135    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160709180135  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160709180135  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145131  IP                  G�O�G�O�G�O�                