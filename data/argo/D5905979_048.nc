CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170905  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               0A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؚ�o�t1   @ؚq�+�@6O��v��c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    0A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�RD��D�_
D���D���D��D�N�D���D��\D�=D�K�D���D���D��D�P�Dګ�D���D�D�P D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw��B�\B�ǮB���B�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB��{B��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�=C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&�D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt�)Dy�HD�
D�[�D��
D��gD�D�K3D��gD���D��D�H D��>D��HD�>D�MDڨRD��>D��D�L{D��D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��;A��/A��;A��;A��HA��TA��HA��TA��TA��TA��`A��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A��A���A���A�A��wA�G�A��hA��DA���A��7A���A��!A�z�A�VA�&�A��PA��;A�(�A��uA�O�A�bA��mA���A�1'A��A��#A�v�A���A���A�ZA���A�-A�&�A��A��PA�A�?}A���A���A�VA���A��A��yA�/A���A�M�A���A���A��hA���A���A��
A�VA�ȴA���A��uA�ƨA�M�A�z�A���A�;dA�A�E�A���A��jA���A�`BA��A�ffA�VA���A��\A�n�A���A���A��/A��\A�M�A�  A���A�|�A�$�A��yA�9XA7LA}��Az(�Ax�AvI�At�Ar��Ap�An�`Am�PAlbAhn�Af��AeK�Ad��Aa�;A_oA^�\A^n�A]�AZ��AWx�AU
=AR�yAQ�hAP��ANbNAM�AK��AI��AH5?AE�^AD�\AC��AC33AB-AAl�AA/A@��A?��A;�TA9G�A89XA5oA3�A25?A1�wA1%A0z�A0ZA0M�A/��A-�7A)ƨA(ffA($�A'��A'`BA'C�A&I�A%�^A$$�A!��A ��A/AXA�RAx�A�uA�RAp�AM�A=qAr�A�AƨA�PA�DA-A��Ax�A;dA�AM�A�
A��At�A
=A
��A
n�A
�A	��A	?}A��A|�AVA�mA�hAhsAG�A��A�DAbNA�A��A;dA ��@�"�@���@��@��w@���@��;@�+@�@��;@�M�@�r�@�$�@�%@�u@�w@���@�&�@�dZ@�
=@��@���@�dZ@��#@�r�@�|�@��@���@��
@�dZ@֗�@�p�@��m@��y@ҧ�@�$�@ёh@Л�@��;@��#@̬@��;@�dZ@���@ɑh@�1'@�ƨ@�ȴ@�V@�&�@��@å�@�33@���@��@�l�@���@��h@�/@��/@��D@���@�~�@��h@���@��@���@�$�@���@��^@��^@�p�@�V@�9X@�"�@�v�@��@��@��#@���@��-@��h@���@��u@�I�@��@��w@��R@���@�/@��j@��j@��@��j@�1'@��
@�+@��@��H@���@�n�@�J@��@��T@���@���@��7@�?}@��9@�(�@���@�K�@�@���@�n�@�E�@�$�@�J@��@��#@�x�@�/@�Ĝ@�z�@�b@�ƨ@��F@��P@�K�@�S�@�o@�o@�K�@��!@�M�@��^@��u@� �@���@���@�b@�bN@��j@�Ĝ@��9@�9X@��@�ƨ@�|�@�K�@�S�@�S�@���@�ȴ@���@���@�v�@�E�@�{@���@��-@���@��h@�hs@��@�9X@�\)@��@�M�@�@�9X@��w@�dZ@��H@�V@��+@�5?@�V@��u@� �@��@��P@�t�@�"�@�
=@���@���@�V@�J@�@�{@�^5@�{@�p�@�Ĝ@��D@�r�@�r�@��;@��@���@�x�@��@�j@�A�@�  @��
@��w@��w@�b@�9X@�(�@���@�S�@�@�o@�33@�\)@�;d@�"�@�o@�
=@�@���@��@��y@��H@���@���@�ȴ@�ȴ@��!@���@��!@���@�E�@�$�@�E�@�E�@�J@�@��@�J@��T@�O�@���@��@��D@�dZ@��+@�-@�@��@��!@�K�@�|�@��m@�b@��@�+@���@�4n@�  @r1�@f�@]��@X��@S.I@K��@E�@?�K@7��@4M@/!-@&@ �K@��@g�@2�@4n@,=@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��/A��;A��/A��;A��;A��HA��TA��HA��TA��TA��TA��`A��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A��A���A���A�A��wA�G�A��hA��DA���A��7A���A��!A�z�A�VA�&�A��PA��;A�(�A��uA�O�A�bA��mA���A�1'A��A��#A�v�A���A���A�ZA���A�-A�&�A��A��PA�A�?}A���A���A�VA���A��A��yA�/A���A�M�A���A���A��hA���A���A��
A�VA�ȴA���A��uA�ƨA�M�A�z�A���A�;dA�A�E�A���A��jA���A�`BA��A�ffA�VA���A��\A�n�A���A���A��/A��\A�M�A�  A���A�|�A�$�A��yA�9XA7LA}��Az(�Ax�AvI�At�Ar��Ap�An�`Am�PAlbAhn�Af��AeK�Ad��Aa�;A_oA^�\A^n�A]�AZ��AWx�AU
=AR�yAQ�hAP��ANbNAM�AK��AI��AH5?AE�^AD�\AC��AC33AB-AAl�AA/A@��A?��A;�TA9G�A89XA5oA3�A25?A1�wA1%A0z�A0ZA0M�A/��A-�7A)ƨA(ffA($�A'��A'`BA'C�A&I�A%�^A$$�A!��A ��A/AXA�RAx�A�uA�RAp�AM�A=qAr�A�AƨA�PA�DA-A��Ax�A;dA�AM�A�
A��At�A
=A
��A
n�A
�A	��A	?}A��A|�AVA�mA�hAhsAG�A��A�DAbNA�A��A;dA ��@�"�@���@��@��w@���@��;@�+@�@��;@�M�@�r�@�$�@�%@�u@�w@���@�&�@�dZ@�
=@��@���@�dZ@��#@�r�@�|�@��@���@��
@�dZ@֗�@�p�@��m@��y@ҧ�@�$�@ёh@Л�@��;@��#@̬@��;@�dZ@���@ɑh@�1'@�ƨ@�ȴ@�V@�&�@��@å�@�33@���@��@�l�@���@��h@�/@��/@��D@���@�~�@��h@���@��@���@�$�@���@��^@��^@�p�@�V@�9X@�"�@�v�@��@��@��#@���@��-@��h@���@��u@�I�@��@��w@��R@���@�/@��j@��j@��@��j@�1'@��
@�+@��@��H@���@�n�@�J@��@��T@���@���@��7@�?}@��9@�(�@���@�K�@�@���@�n�@�E�@�$�@�J@��@��#@�x�@�/@�Ĝ@�z�@�b@�ƨ@��F@��P@�K�@�S�@�o@�o@�K�@��!@�M�@��^@��u@� �@���@���@�b@�bN@��j@�Ĝ@��9@�9X@��@�ƨ@�|�@�K�@�S�@�S�@���@�ȴ@���@���@�v�@�E�@�{@���@��-@���@��h@�hs@��@�9X@�\)@��@�M�@�@�9X@��w@�dZ@��H@�V@��+@�5?@�V@��u@� �@��@��P@�t�@�"�@�
=@���@���@�V@�J@�@�{@�^5@�{@�p�@�Ĝ@��D@�r�@�r�@��;@��@���@�x�@��@�j@�A�@�  @��
@��w@��w@�b@�9X@�(�@���@�S�@�@�o@�33@�\)@�;d@�"�@�o@�
=@�@���@��@��y@��H@���@���@�ȴ@�ȴ@��!@���@��!@���@�E�@�$�@�E�@�E�@�J@�@��@�J@��T@�O�@���@��@��D@�dZ@��+@�-@�@��@��!@�K�@�|�@��m@�b@��@�+G�O�@�4n@�  @r1�@f�@]��@X��@S.I@K��@E�@?�K@7��@4M@/!-@&@ �K@��@g�@2�@4n@,=@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B� B�B�B�B�B�B�B�B�B}�B~�B�\B}�B~�B�JB�B� B�B�B�B� B� B~�B~�B}�B~�B}�B|�B{�B{�Bz�By�Bw�Bx�Bs�Bo�Bn�Bm�BgmBe`BcTBcTB]/BXBT�BQ�BH�BE�B<jB49B!�BuBB�B�B��BÖB�B��B��B�%Bl�BT�BG�B-B"�B �B�B  B
��B
�B
�sB
�fB
�HB
��B
ƨB
�B
��B
��B
�{B
�\B
�DB
�1B
�B
|�B
w�B
[#B
P�B
:^B
+B
�B
\B
B	��B	�sB	�/B	��B	�RB	��B	��B	�oB	�%B	z�B	w�B	u�B	p�B	_;B	E�B	5?B	)�B	�B	�B	DB	%B	B��B�B�ZB�)B�5B�5B�B�B��B��B��B�3B��B��B��B�\B�DB�JB�=B�1B�1B�1B�oB�%Bs�BjBjBk�Bk�Br�Bv�Bv�Bm�BZBXBVBS�BQ�BO�BN�BK�BI�BG�BF�BB�BA�B@�B@�BA�B@�B@�B?}B?}B?}B?}B?}B>wB>wB?}B>wB>wB>wB>wB>wB<jB>wB=qB<jB:^B9XB9XB8RB7LB8RB:^B;dB9XB9XB;dB8RB:^B9XB9XB8RB8RB7LB7LB8RB8RB:^B9XB9XB:^B9XB;dB:^B:^B:^B:^B:^B;dB:^B:^B:^B;dB<jB;dB<jB=qBC�BC�BD�BF�BE�BG�BG�BL�BN�BO�BP�BP�BVBXBYB[#B[#B`BBcTBcTBdZBhsBn�Bp�Br�Bv�Bv�Bw�Bw�B{�B~�B�B�B�B�+B�=B�DB�DB�JB�bB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�-B�9B�LB�dB�qB�qB�qB�qB�wB�}B��BBÖBÖBÖBĜBǮB��B�B�#B�BB�ZB�mB�B�B�B�B�B�B��B��B��B��B	B	B	+B	
=B	
=B	
=B	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	)�B	1'B	6FB	8RB	9XB	;dB	<jB	=qB	=qB	>wB	C�B	I�B	K�B	N�B	P�B	R�B	VB	YB	]/B	`BB	aHB	bNB	cTB	dZB	e`B	hsB	hsB	gmB	gmB	hsB	hsB	gmB	iyB	k�B	n�B	s�B	v�B	y�B	y�B	z�B	{�B	|�B	~�B	�B	�1B	�7B	�7B	�7B	�=B	�=B	�=B	�VB	�bB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	�hB	�\B	�\B	�bB	�bB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�-B	�?B	�LB	�XB	�^B	�dB	�qB	�qB	�wB	��B	B	ÖB	��B	��B	ĜB	ÖB	��B	�jB	�qB	�wB	��B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	�_B	�nB
�B
�B
qB
(XB
,�B
33B
>BB
ESB
LB
Q�B
U2B
Z�B
`'B
d�B
i�B
l�B
raB
vB
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bx�Bx�By�By�By�B{�By�Bu�Bv�B�Bu�Bv�B��Bz�Bw�By�By�By�Bw�Bw�Bv�Bv�Bu�Bv�Bu�Bt�Bs�Bs�Br�Bq�BoBp�BkgBgOBfIBeBB_B]B[B[BT�BO�BL�BI�B@hB=VB4B+�B�B-B��B�FB��BĊB�TB��B��B�aB}�BdPBL�B?uB$�B�B�BLB
��B
�B
�rB
�BB
�5B
�B
ǯB
�yB
��B
��B
�hB
�PB
�1B
�B
�B
y�B
t�B
o�B
R�B
H�B
2:B
"�B
�B
;B	��B	�B	�UB	�B	��B	�7B	��B	��B	�WB	~B	r�B	o�B	m�B	h�B	W'B	=�B	-/B	!�B	�B	�B	7B�B�B��B�B�PB� B�,B�,B�B��B��B��BÿB�-B��B��B�~B�ZB�BB�HB�;B�/B�/B�/B�mB~$Bk�Bb�Bb�Bc�Bc�Bj�Bn�Bn�Be�BR BPBNBK�BI�BG�BF�BC�BA�B?�B>�B:�B9�B8�B8�B9�B8�B8�B7�B7�B7�B7�B7�B6~B6~B7�B6~B6~B6~B6~B6~B4rB6B5yB4rB2fB1`B1`B0ZB/TB0ZB2fB3mB1aB1aB3mB0[B2gB1aB1bB0\B0\B/VB/VB0\B0\B2hB1bB1bB2hB1cB3oB2iB2iB2iB2iB2iB3oB2iB2iB2iB3oB4uB3oB4uB5|B;�B;�B<�B>�B=�B?�B?�BD�BF�BG�BH�BH�BNBPBQ"BS.BS.BXLB[^B[^B\dB`}Bf�Bh�Bj�Bn�Bn�Bo�Bo�Bs�BwBzBzB{B4B�FB�MB�MB�SB�kB�kB��B��B��B��B��B��B��B��B��B��B�B�	B�B�B�B�(B�.B�4B�@B�SB�jB�wB�wB�wB�wB�}B��B��B��B��B��B��B��B��B��B�	B�(B�GB�^B�qB�B�B�B�B�B�B�B��B��B��B�	B�B�-B	?B	?B	?B	dB	
qB	}B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	)'B	.FB	0RB	1XB	3cB	4iB	5pB	5pB	6vB	;�B	A�B	C�B	F�B	H�B	J�B	NB	QB	U-B	X?B	YEB	ZKB	[QB	\WB	]]B	`pB	`pB	_jB	_jB	`pB	`pB	_jB	avB	c�B	f�B	k�B	n�B	q�B	q�B	r�B	s�B	t�B	v�B	{B	�-B	�3B	�3B	�3B	�9B	�9B	�9B	�QB	�]B	�cB	�jB	�jB	�pB	�vB	��B	�|B	�|B	�dB	�XB	�XB	�^B	�^B	�^B	�dB	�kB	�|B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�'B	�8B	�EB	�QB	�WB	�]B	�jB	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	�|B	�cB	�jB	�pB	�|B	��B	��B	ÿB	��B	��B	��B	��G�O�B	�WB	�dB	��B
�B
fB
 LB
$�B
+'B
66B
=FB
DB
I�B
M%B
R�B
XB
\�B
a�B
d�B
jSB
nB
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170905    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170905  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170905  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                