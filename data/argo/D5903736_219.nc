CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-05-14T17:02:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180514170220  20190604094145  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�b��4�1   @�b���Q@6���"���d{��v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DyaHD� D�EqD���D���D�)D�2�D��D���D��{D�A�D��=D���D� RD�J=D�l{D���D���D�7
D�hRD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��DtX�DyZ>D�{D�A�D�� D��RD��D�/\D�|)D��HD���D�>gD���D��RD���D�F�D�h�D��RD��)D�3�D�d�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�z�A�~�A�z�A�x�A�v�A�v�A�x�AÉ7AÉ7AÇ+AÇ+AÇ+AÅAÅA�~�A�Q�A�?}A�
=AA��A��RA��A���A�ĜA���A�  A��!A�7LA��A��A�9XA���A�
=A���A�A�K�A�E�A�+A��RA�p�A��;A�x�A�bA�dZA�A�A�(�A���A�G�A�A��A��FA�r�A�^5A�5?A��RA���A��^A��/A���A��^A��hA��\A��A�ȴA�-A�{A��A�x�A��A��uA�K�A��A���A�1A�jA��FA�p�A�~�A���A�ƨA��wA�1'A��PA�33A���A���A�E�A�K�A�1A�XA��A��7A�r�A�5?A��!A���A�1A��9A�ƨA�=qA�ffA���A�M�A��PA�"�A�\)A�n�A��A���A�/A��A�ĜA��A�t�A�M�A�  AoA}\)A{��Az�/Ax�uAv1As��Ar�`Aq
=Apz�Ao�hAm�Al�Ai`BAg��AdQ�Ab$�A` �A]��A\�A[�hAYG�AW
=AU��AT�AS��AR^5AO��AN~�AK��AH��AF��AE
=ADA�AC�#AB��A@��A@E�A@M�A@(�A>5?A<�DA;p�A;A9O�A7C�A6n�A4�A4Q�A2�A1��A1|�A0n�A/C�A.��A.bNA-��A+�wA)?}A';dA%%A"��A"A!`BA ��A��AoAffAK�A��A�-A��A�A�PAG�A�A=qA�A��Az�Ar�Ar�A�mA�DA��AVA��A��A^5AhsA�Al�AM�A|�At�A	�hA|�A�HA-A`BA
=A��AjA�FA~�A(�A�A  A�wA|�A`BA ��A ffA {@��H@��@���@��@���@���@���@�I�@�dZ@��@��m@�ȴ@�O�@�Ĝ@�ƨ@�ff@�p�@�Ĝ@��@�C�@�5?@�(�@�R@�u@���@�M�@�33@�x�@�r�@�Q�@۶F@ڰ!@�j@��@���@��
@ו�@�t�@��T@҇+@��T@�p�@��@��
@�@��`@�ƨ@˕�@�@�7L@� �@�  @Ǯ@ǅ@ǅ@�K�@��@Ɨ�@�{@Ų-@ř�@�Ĝ@���@�\)@�ȴ@�5?@��@��T@��7@�%@��@�dZ@��y@�-@��h@���@���@��@�J@�G�@���@�z�@�9X@���@�t�@���@�`B@��j@�j@�ƨ@�+@��@���@��!@�$�@��@�%@��@�1'@�b@��m@���@�t�@�@��!@���@�ff@��#@��7@�V@��j@�bN@�ƨ@���@�5?@�@���@��@�Ĝ@�1'@��F@�l�@�;d@�@�n�@��T@�hs@��@��u@� �@�l�@�C�@�"�@�
=@�ȴ@�M�@�{@���@�/@��/@��@���@�I�@��
@���@�dZ@�K�@��@���@�J@��@�O�@���@���@��/@�j@���@�33@�@���@��!@��+@�E�@�$�@�@���@�X@�V@��@�j@�1'@��m@���@���@�\)@�"�@�@���@�=q@�$�@�{@���@��@��T@���@�@��-@���@���@���@���@�`B@�/@�Ĝ@�bN@�A�@�9X@��@��@�l�@�o@��y@��@���@���@��\@��+@�~�@�v�@�$�@�@��#@�`B@���@��`@���@��j@�bN@�  @���@��w@���@��@�\)@�C�@�33@�33@�+@�"�@�@��H@���@��R@���@���@�~�@�~�@�v�@�M�@�-@�@���@��-@��h@�`B@�&�@��/@��@�j@��@��
@���@��@�K�@��@���@��H@��@��@zȴ@r@l�@b�X@Z	@Vff@N��@HPH@A��@9&�@4�[@/(@)��@$�@G�@�A@�<@+@
Ta11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�p�A�z�A�~�A�z�A�x�A�v�A�v�A�x�AÉ7AÉ7AÇ+AÇ+AÇ+AÅAÅA�~�A�Q�A�?}A�
=AA��A��RA��A���A�ĜA���A�  A��!A�7LA��A��A�9XA���A�
=A���A�A�K�A�E�A�+A��RA�p�A��;A�x�A�bA�dZA�A�A�(�A���A�G�A�A��A��FA�r�A�^5A�5?A��RA���A��^A��/A���A��^A��hA��\A��A�ȴA�-A�{A��A�x�A��A��uA�K�A��A���A�1A�jA��FA�p�A�~�A���A�ƨA��wA�1'A��PA�33A���A���A�E�A�K�A�1A�XA��A��7A�r�A�5?A��!A���A�1A��9A�ƨA�=qA�ffA���A�M�A��PA�"�A�\)A�n�A��A���A�/A��A�ĜA��A�t�A�M�A�  AoA}\)A{��Az�/Ax�uAv1As��Ar�`Aq
=Apz�Ao�hAm�Al�Ai`BAg��AdQ�Ab$�A` �A]��A\�A[�hAYG�AW
=AU��AT�AS��AR^5AO��AN~�AK��AH��AF��AE
=ADA�AC�#AB��A@��A@E�A@M�A@(�A>5?A<�DA;p�A;A9O�A7C�A6n�A4�A4Q�A2�A1��A1|�A0n�A/C�A.��A.bNA-��A+�wA)?}A';dA%%A"��A"A!`BA ��A��AoAffAK�A��A�-A��A�A�PAG�A�A=qA�A��Az�Ar�Ar�A�mA�DA��AVA��A��A^5AhsA�Al�AM�A|�At�A	�hA|�A�HA-A`BA
=A��AjA�FA~�A(�A�A  A�wA|�A`BA ��A ffA {@��H@��@���@��@���@���@���@�I�@�dZ@��@��m@�ȴ@�O�@�Ĝ@�ƨ@�ff@�p�@�Ĝ@��@�C�@�5?@�(�@�R@�u@���@�M�@�33@�x�@�r�@�Q�@۶F@ڰ!@�j@��@���@��
@ו�@�t�@��T@҇+@��T@�p�@��@��
@�@��`@�ƨ@˕�@�@�7L@� �@�  @Ǯ@ǅ@ǅ@�K�@��@Ɨ�@�{@Ų-@ř�@�Ĝ@���@�\)@�ȴ@�5?@��@��T@��7@�%@��@�dZ@��y@�-@��h@���@���@��@�J@�G�@���@�z�@�9X@���@�t�@���@�`B@��j@�j@�ƨ@�+@��@���@��!@�$�@��@�%@��@�1'@�b@��m@���@�t�@�@��!@���@�ff@��#@��7@�V@��j@�bN@�ƨ@���@�5?@�@���@��@�Ĝ@�1'@��F@�l�@�;d@�@�n�@��T@�hs@��@��u@� �@�l�@�C�@�"�@�
=@�ȴ@�M�@�{@���@�/@��/@��@���@�I�@��
@���@�dZ@�K�@��@���@�J@��@�O�@���@���@��/@�j@���@�33@�@���@��!@��+@�E�@�$�@�@���@�X@�V@��@�j@�1'@��m@���@���@�\)@�"�@�@���@�=q@�$�@�{@���@��@��T@���@�@��-@���@���@���@���@�`B@�/@�Ĝ@�bN@�A�@�9X@��@��@�l�@�o@��y@��@���@���@��\@��+@�~�@�v�@�$�@�@��#@�`B@���@��`@���@��j@�bN@�  @���@��w@���@��@�\)@�C�@�33@�33@�+@�"�@�@��H@���@��R@���@���@�~�@�~�@�v�@�M�@�-@�@���@��-@��h@�`B@�&�@��/@��@�j@��@��
@���@��@�K�@��@���G�O�@��@��@zȴ@r@l�@b�X@Z	@Vff@N��@HPH@A��@9&�@4�[@/(@)��@$�@G�@�A@�<@+@
Ta11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBW
BZBZB\)BffBx�B��B��B��B��B�B �B"�B.B49B@�BE�BH�BG�BL�BVB_;Bl�Bw�B� B}�B�B�=B��B�oB�bB�bB��B��B��B��B��B��B��B��B�B�-B��B��B�B� B�B�Bx�B}�B�B�B{�B|�B�PB��B�oB�bB�=Br�BffBXB>wB-B �BDB�mB�)B��BɺBƨB�}B�'B��Bw�BVBP�BL�BK�BI�BC�B7LB+B&�B�BB
�mB
�ZB
��B
��B
�B
��B
��B
�^B
�?B
�B
��B
��B
�\B
�1B
�B
� B
w�B
l�B
bNB
YB
K�B
=qB
/B
(�B
�B
�B
oB
+B	��B	�sB	�B	ƨB	�RB	�B	��B	��B	��B	�=B	�B	w�B	r�B	k�B	`BB	P�B	G�B	/B	�B	DB��B��B��B�B�fB�TB�TB�HB�B��B�B�#B�B��B��BĜB�}B�RB�^B�}B�wB�^B�RB�?B�'B��B��B��B�hB�=B�1B�B�B~�B{�Bz�Bx�Bv�Bt�Bs�Br�Br�Bq�Bp�Bn�Bm�Bl�Bm�Bq�Bt�B{�B{�Bz�B~�B�7B��B��B��B��B��B��B�hB�=B{�Br�Bs�Bv�Bv�Bw�Bw�Bu�Bv�By�B{�B{�B{�B|�B}�B}�B~�B~�B}�B}�B~�B}�B� B�B�B�B�B�B�%B�1B�7B�=B�DB�JB�PB�\B�bB�bB�bB�bB�oB�oB��B�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�RB�RB�dB�wB��BBŢBƨBƨBǮBɺB��B��B��B��B�
B�BB�NB�mB�B�B�B��B��B��B��B	  B	%B		7B	DB	hB	�B	�B	�B	�B	�B	!�B	$�B	%�B	(�B	)�B	+B	-B	.B	1'B	33B	5?B	7LB	:^B	<jB	=qB	>wB	>wB	>wB	A�B	C�B	C�B	D�B	F�B	J�B	N�B	Q�B	T�B	VB	W
B	YB	ZB	ZB	\)B	]/B	_;B	dZB	e`B	e`B	e`B	ffB	hsB	jB	m�B	o�B	q�B	r�B	r�B	t�B	v�B	x�B	z�B	z�B	{�B	~�B	�B	�1B	�7B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�?B	�FB	�RB	�^B	�dB	�dB	�jB	�qB	�qB	�qB	�wB	�wB	�wB	�wB	�wB	�wB	��B	��B	ÖB	ŢB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�/B	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�]B
�B
!B
,�B
3�B
9�B
=�B
CGB
G�B
J�B
PbB
X�B
^�B
d�B
f�B
j�B
q�B
t�B
{�B
}�B
�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BE�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BF�BI�BI�BK�BU�BhdB�,B�qB�BB�RB3BCBNB�B#�B/�B5B8*B7$B<DBEvBN�B[�BgABoqBm_Br�By�B��B��B�B�B��B�B�;B�=B�JB�EB�EB�IB��B��B�fB��Br�BorBt�BpzBhPBmkBq�BpyBk\BliB|�B��B��B�By�Bb)BU�BG�B-�B�BHB��B��B˷B�uB�KB�7B�B��B�EBglBE�B@�B<nB;gB9^B39B&�B�B�BZB
��B
�B
�B
��B
��B
źB
��B
�AB
�B
��B
��B
��B
�]B
B
w�B
t�B
o�B
g�B
\PB
RB
H�B
;�B
-:B
�B
�B
�B
	kB
AB	��B	��B	�KB	��B	��B	�2B	��B	��B	��B	�fB	z B	q�B	g�B	b�B	[rB	P1B	@�B	7�B	B	�B�?B��B��B��B�B�hB�VB�VB�IB�B��B�B�)B�B��B��B��B��B�XB�hB��B��B�fB�ZB�GB�3B�B��B��B�wBzOBxBBt+BqBo
Bk�Bj�Bh�Bf�Bd�Bc�Bb�Bb�Ba�B`�B^�B]�B\�B]�Ba�Bd�Bk�Bk�Bj�BoByMG�O�B��B��B��B��B��B�~BzSBl Bb�Bc�Bf�Bf�Bg�Bg�Be�Bf�Bi�BlBlBlBmBnBnBoBoBnBnBoBnBpBr)Bs+Bs*Br(Br*Bv@BxLByPBzWB{_B|cB}iByB�|B�B�}B�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�2B�KB�bB�hB�mB��B��B��B��B��B��B��B��B��B��B�B��B�
B�"B�YB�fBׄBݨB�B��B��B��B�B�B�B�;B�MB�ZB	~B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	 B	%B	!6B	#CB	%KB	'\B	*mB	,zB	-�B	.�B	.�B	.�B	1�B	3�B	3�B	4�B	6�B	:�B	>�B	A�B	EB	FB	GB	I%B	J)B	J*B	L3B	M9B	OBB	TiB	UkB	UkB	UjB	VpB	X}B	Z�B	]�B	_�B	a�B	b�B	b�B	d�B	f�B	h�B	j�B	j�B	k�B	oB	t!B	x;B	y>B	}WB	�fB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�'B	�+B	�3B	�=B	�@B	�HB	�YB	�_B	�dB	�bB	�mB	�rB	�tB	�tB	�wB	�yB	�yB	�{B	�vB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�+B	�AB	�>B	�BB	�DB	�IB	�PB	�WB	�WB	�WB	�YB	�UB	�[B	�bB	�cB	�gB	�jB	�hB	�nB	�oB	�oB	�tB	�B	ہB	܈B	ݎB	݋B	ޒB	ߘB	�B	�B	�B	�B	�B	��B	�B	��B	��B	��G�O�B	�YB
 �B
B
�B
#�B
)�B
-�B
3@B
7�B
:�B
@VB
H�B
N�B
T�B
V�B
Z�B
a�B
d�B
k�B
m�B
q_11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941452019060409414520190604094145  AO  ARCAADJP                                                                    20180514170220    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180514170220  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180514170220  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094145  IP                  G�O�G�O�G�O�                